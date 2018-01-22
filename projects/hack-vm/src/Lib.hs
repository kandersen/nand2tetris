{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Lib where

import Prelude
import Control.Applicative (empty)
import Control.Monad.State
import Control.Monad.Identity
import Data.Char (digitToInt)
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.String
import Lens.Micro.Platform
import qualified Text.Megaparsec.Lexer as L

data Segment = Argument
             | Local
             | Static
             | Constant
             | This
             | That
             | Pointer
             | Temp

type Symbol = String

data Command = Add
             | Sub
             | Neg
             | Eq
             | Gt
             | Lt
             | And
             | Or
             | Not
             | Push Segment Int
             | Pop Segment Int
             | Label Symbol
             | Goto Symbol
             | IfGoto Symbol
             | Function Symbol Int
             | Call Symbol Int
             | Return

data TranslatorState cu = TS {
  _labelSupply :: [String],
  _compilationUnit :: cu,
  _function :: String
  }
makeLenses ''TranslatorState

runTranslator :: StateT (TranslatorState ()) Identity a -> a
runTranslator t = runIdentity $ evalStateT t initialState
  where
    initialState = TS {
      _labelSupply = [ "_INTERNAL." ++ show n | n <- [0..] ],
      _compilationUnit = (),
      _function = ""
      }

freshLabel :: (MonadState (TranslatorState a) m) => m String
freshLabel = head <$> (labelSupply <%= tail)

staticLabel :: (MonadState (TranslatorState String) m) => Int -> m String
staticLabel n = (++ ("." ++ show n)) <$> use compilationUnit

resolveLabel :: MonadState (TranslatorState String) m => Symbol -> m Symbol
resolveLabel l = do
  fnprefix <- use function
  return $ fnprefix ++ "$" ++ l


translateCommand :: (MonadState (TranslatorState String) m) => Command -> m [String]
translateCommand Add = return $ translateBinOp "+"
translateCommand Sub = return $ translateBinOp "-"
translateCommand Neg = return $ translateUnOp "-"
translateCommand Eq = translateCmp "EQ"
translateCommand Gt = translateCmp "GT"
translateCommand Lt = translateCmp "LT"
translateCommand And = return $ translateBinOp "&"
translateCommand Or = return $ translateBinOp "|"
translateCommand Not = return $ translateUnOp "!"
translateCommand (Push segment index) = translatePush segment index
translateCommand (Pop segment index) = translatePop segment index
translateCommand (Label l) = translateLabel <$> resolveLabel l
translateCommand (Goto l) = translateGoto <$> resolveLabel l
translateCommand (IfGoto l) = translateIfGoto <$> resolveLabel l
translateCommand (Function name argcount) = translateFunction name argcount
translateCommand (Call name argcount) = translateCall name argcount
translateCommand Return = return $ translateReturn

translateCmp :: (MonadState (TranslatorState a) m) => String -> m [String]
translateCmp cmp = do
  ifTrue <- freshLabel
  done <- freshLabel
  return $ ["@SP","M=M-1","A=M","D=M", "A=A-1", "D=M-D","@" ++ ifTrue,"D;J" ++ cmp,
            "@SP","A=M-1","M=0","@" ++ done,"0;JMP",
            "(" ++ ifTrue ++ ")","@SP","A=M-1","M=-1",
            "(" ++ done ++ ")"]

translateBinOp :: String -> [String]
translateBinOp op = ["@SP", "M=M-1", "A=M", "D=M", "A=A-1", "M=M" ++ op ++ "D"]

translateUnOp :: String -> [String]
translateUnOp op = ["@SP", "A=M-1", "M=" ++ op ++ "M"]

translatePush :: (MonadState (TranslatorState String) m) => Segment -> Int -> m [String]
translatePush Constant n = return $ ["@" ++ show n, "D=A", "@SP", "M=M+1", "A=M-1", "M=D" ]
translatePush Local n = return $ translateIndirectPush "LCL" n
translatePush Argument n = return $ translateIndirectPush "ARG" n
translatePush This n = return $ translateIndirectPush "THIS" n
translatePush That n = return $ translateIndirectPush "THAT" n
translatePush Pointer 0 = return $ translateDirectPush "R3"
translatePush Pointer 1 = return $ translateDirectPush "R4"
translatePush Pointer n = error $ "push pointer index out of range: " ++ show n
translatePush Static n = translateDirectPush <$> staticLabel n
translatePush Temp n | 0 <= n && n < 8 = return $ translateDirectPush $ "R" ++ (show $ n + 5)
                     | otherwise = error $ "push temp index out of range: " ++ show n

translateDirectPush :: String -> [String]
translateDirectPush reg = ["@" ++ reg,"D=M","@SP","M=M+1","A=M-1","M=D"]

translateIndirectPush :: String -> Int -> [String]
translateIndirectPush seg n = ["@" ++ show n, "D=A",
                               "@" ++ seg, "A=M+D","D=M",
                               "@SP","M=M+1","A=M-1","M=D"
                              ]

translatePop :: MonadState (TranslatorState String) m => Segment -> Int -> m [String]
translatePop Constant _ = error "pop constant not allowed"
translatePop Local    n = return $ translateIndirectPop "LCL" n
translatePop Argument n = return $ translateIndirectPop "ARG" n
translatePop This     n = return $ translateIndirectPop "THIS" n
translatePop That     n = return $ translateIndirectPop "THAT" n
translatePop Pointer  0 = return $ translateDirectPop "R3"
translatePop Pointer  1 = return $ translateDirectPop "R4"
translatePop Pointer  n = error $ "pop pointer index out of range: " ++ show n
translatePop Static   n = translateDirectPop <$> staticLabel n
translatePop Temp     n | 0 <= n && n < 8 = return $ translateDirectPop $ "R" ++ show (5 + n)
                        | otherwise       = error $ "pop temp index out of range: " ++ show n

translateIndirectPop :: String -> Int -> [String]
translateIndirectPop seg n = ["@" ++ show n, "D=A",
                              "@" ++ seg,"D=D+M","@R13","M=D",
                              "@SP","M=M-1","A=M","D=M",
                              "@R13","A=M","M=D"
                             ]

translateDirectPop :: String -> [String]
translateDirectPop reg = ["@SP","M=M-1","A=M","D=M", "@" ++ reg, "M=D"]


translateLabel :: Symbol -> [String]
translateLabel l = ["(" ++ l ++ ")"]

translateGoto :: Symbol -> [String]
translateGoto l = ["@" ++ l, "0;JMP"]

translateIfGoto :: Symbol -> [String]
translateIfGoto l = ["@SP", "M=M-1", "A=M", "D=M", "@" ++ l, "D;JNE"]

translateFunction :: MonadState (TranslatorState a) m => Symbol -> Int -> m [String]
translateFunction name argcount = do
  function .= name
  return $ ["//function " ++ name ++ " " ++ show argcount,
            "(" ++ name ++ ")"]
    ++ go argcount
    where
      go 0 = []
      go n = ["@0","D=A"] ++ (concat . replicate n $ ["@SP","M=M+1","A=M-1","M=D"])

translateCall :: MonadState (TranslatorState a) m => Symbol -> Int -> m [String]
translateCall name argcount = do
  returnAddr <- freshLabel
  return $ concat [ ["//call " ++ name ++ " " ++ show argcount],
    push returnAddr,
    push "LCL",
    push "ARG",
    push "THIS",
    push "THAT"
    ] ++
    [ "@SP","D=M","@" ++ show argcount, "D=D-A", "@5", "D=D-A","@ARG","M=D",
      "@SP","D=M","@LCL","M=D",
      "@" ++ name, "0;JMP",
      "(" ++ returnAddr ++ ")"
    ]
    where
      push r = ["@" ++ r, "D=A","@SP","M=M+1","A=M-1","M=D"]

translateReturn :: [String]
translateReturn = ["//return",
  "@LCL","D=M","@R13","M=D", -- FRAME(R13) = LCL
  "@5","D=A","@R13","A=M-D","D=M","@R14","M=D", -- RET(R14) = *(FRAME - 5)
  "@SP","M=M-1","A=M","D=M","@ARG","A=M","M=D", -- *ARG = pop()
  "@ARG","D=M+1","@SP","M=D", -- SP = ARG+1
  "@R13","D=M","@1","A=D-A","D=M","@THAT","M=D",
  "@R13","D=M","@2","A=D-A","D=M","@THIS","M=D",
  "@R13","D=M","@3","A=D-A","D=M","@ARG","M=D",
  "@R13","D=M","@4","A=D-A","D=M","@LCL","M=D",
  "@R14","A=M","0;JMP"
  ]

pSegment :: Parser Segment
pSegment = spaces *> go
  where
    go = Argument <$ string "argument"
     <|> Local <$ string "local"
     <|> Static <$ string "static"
     <|> Constant <$ string "constant"
     <|> This <$ string "this"
     <|> That <$ string "that"
     <|> Pointer <$ string "pointer"
     <|> Temp <$ string "temp"

pSymbol :: Parser Symbol
pSymbol = (:) <$> pSymbolChar <*> many (pSymbolChar <|> digitChar)

pSymbolChar :: Parser Char
pSymbolChar = letterChar <|> char '_' <|> char '.' <|> char '$' <|> char ':'

pCommand :: Parser Command
pCommand = Add <$ string "add"
       <|> Sub <$ string "sub"
       <|> Neg <$ string "neg"
       <|> Eq  <$ string "eq"
       <|> Gt  <$ string "gt"
       <|> Lt  <$ string "lt"
       <|> And <$ string "and"
       <|> Or  <$ string "or"
       <|> Not <$ string "not"
       <|> Push <$ string "push" <*> pSegment <* spaces <*> natural
       <|> Pop <$ string "pop" <*> pSegment <* spaces <*> natural
       <|> Label <$ string "label" <* spaces <*> pSymbol
       <|> Goto <$ string "goto" <* spaces <*> pSymbol
       <|> IfGoto <$ string "if-goto" <* spaces <*> pSymbol
       <|> Function <$ string "function" <* spaces <*> pSymbol <* spaces <*> natural
       <|> Call <$ string "call" <* spaces <*> pSymbol <* spaces <*> natural
       <|> Return <$ string "return"

natural :: (Num a) => Parser a
natural = foldl (\acc n -> acc * 10 + fromIntegral (digitToInt n)) 0 <$> some digitChar

wspace :: Parser ()
wspace = L.space (spaceChar *> pure ()) (L.skipLineComment "//") empty

pLine :: Parser Command
pLine = wspace *> pCommand <* wspace

pCompilationUnit :: String -> Parser CompilationUnit
pCompilationUnit fileName = CU fileName <$>  manyTill pLine eof

spaces :: Parser String
spaces = many $ char ' '

type Program = [CompilationUnit]

data CompilationUnit = CU {
  _fileName :: String,
  _body :: [Command]
  }

translateCompilationUnit :: MonadState (TranslatorState ()) m => CompilationUnit -> m [String]
translateCompilationUnit cu = do
  s0 <- get
  (cs, s1) <- runStateT (fmap concat . traverse translateCommand .  _body $ cu) (s0 & compilationUnit .~ _fileName cu)
  put $ s1 & compilationUnit .~ ()
  return cs

translateProgram :: MonadState (TranslatorState ()) m => Program -> m [String]
translateProgram = fmap join . traverse translateCompilationUnit

translate :: Program -> String
translate = unlines . runTranslator . translateProgram
