{-# LANGUAGE TemplateHaskell #-}
module Core where

import Control.Applicative (empty)
import Control.Monad.Trans.Writer.Strict
import Data.Char (digitToInt)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Lens.Micro.Platform
import Numeric (showIntAtBase)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List (intersperse)
import Data.Maybe (fromMaybe)



import Control.Monad.Identity
import Control.Monad.State

data Atom = Zero
          | One
          | D
          | M
          | A
         deriving (Show)

pAtom :: Parser Atom
pAtom = do
  c <- anyChar
  case c of
    'A' -> return A
    'M' -> return M
    'D' -> return D
    '0' -> return Zero
    '1' -> return One
    _ -> unexpected (Tokens (c :| []))

data Expr = Const Atom
          | Neg Atom
          | Not Atom
          | Add Atom Atom
          | Sub Atom Atom
          | And Atom Atom
          | Or Atom Atom
          deriving (Show)

pExpr :: Parser Expr
pExpr = Neg <$> pNeg
  <|> Not <$> pNot
  <|> do
    a <- pAtom
    try (Add <$> pure a <* char '+' <*> pAtom)
      <|> try (Sub <$> pure a <* char '-' <*> pAtom)
      <|> try (And <$> pure a <* char '&' <*> pAtom)
      <|> try (Or <$> pure a <* char '|' <*> pAtom)
      <|> Const <$> pure a
  where
    pNeg = try (char '-') *> pAtom
    pNot = try (char '!') *> pAtom

data Compute = Compute {
  _toM :: Bool,
  _toD :: Bool,
  _toA :: Bool,
  _jmpGT :: Bool,
  _jmpEQ :: Bool,
  _jmpLT :: Bool,
  _comp :: Expr
}
  deriving Show
makeLenses ''Compute

emptyCompute :: Expr -> Compute
emptyCompute = Compute False False False False False False


data Command = ACommand (Either Integer Symbol)
             | CCommand Compute
             | LCommand Symbol
             deriving Show


natural :: (Num a) => Parser a
natural = foldl (\acc n -> acc * 10 + fromIntegral (digitToInt n)) 0 <$> some digitChar

wspace :: Parser ()
wspace = L.space (spaceChar *> pure ()) (L.skipLineComment "//") empty

pSymbol :: Parser Symbol
pSymbol = (:) <$> pSymbolChar <*> many (pSymbolChar <|> digitChar)

pSymbolChar :: Parser Char
pSymbolChar = letterChar <|> char '_' <|> char '.' <|> char '$' <|> char ':' 

pLabel :: Parser Symbol
pLabel = char '(' *> pSymbol <* char ')'

pCommand :: Parser Command
pCommand = ACommand <$ char '@' <*> ((Left <$> natural) <|> (Right <$> pSymbol)) <* wspace
       <|> LCommand <$> pLabel
       <|> CCommand <$> pCompute

pLine :: Parser Command
pLine = wspace *> pCommand <* wspace

pDest :: Parser (Compute -> Compute)
pDest = (foldr (.) id <$> sequence [
  option id (char 'A' >> pure (toA .~ True)),
  option id (char 'M' >> pure (toM .~ True)),
  option id (char 'D' >> pure (toD .~ True))
  ]) <* char '='

pJmp :: Parser (Compute -> Compute)
pJmp = char ';' *> (foldr (.) id <$> p)
  where
    p :: Parser [Compute -> Compute]
    p = string "JGT" *> pure [jmpGT .~ True]
        <|> string "JEQ" *> pure [jmpEQ .~ True]
        <|> string "JGE" *> pure [jmpEQ .~ True, jmpGT .~ True]
        <|> string "JLT" *> pure [jmpLT .~ True]
        <|> string "JNE" *> pure [jmpLT .~ True, jmpGT .~ True]
        <|> string "JLE" *> pure [jmpLT .~ True, jmpEQ .~ True]
        <|> string "JMP" *> pure [jmpLT .~ True, jmpEQ .~ True, jmpGT .~ True]

pCompute :: Parser Compute
pCompute = do
  setDest <- option id (try pDest)
  expr <- pExpr
  setJmp <- option id (try pJmp)
  return . setJmp . setDest $ emptyCompute expr

pProgram :: Parser [Command]
pProgram = manyTill pLine eof

forceParse :: Parser a -> String -> a
forceParse p inp = case parse p "" inp of
  Left e -> error $ parseErrorPretty e
  Right c -> c

parseFile :: FilePath -> IO [Command]
parseFile file = do
  input <- readFile file
  case parse pProgram file input of
    Left e -> error $ parseErrorPretty e
    Right c -> return c

parseLine :: Parser (Maybe Command)
parseLine = wspace *> pure Nothing <* eof
            <|> wspace *> (Just <$> pCommand) <* wspace <* eof

codegenA :: Integer -> String
codegenA a =
  if a <= 2^15 -1
  then pad $ showIntAtBase 2 ("01" !!) a ""
  else error $ "Literal address out of bounds: " ++ show a

pad :: String -> String
pad xs = replicate (16 - length xs) '0' ++ xs

codegenC :: Compute -> String
codegenC c = "111" ++ codegenExpr (_comp c) ++ (toBit <$> [_toA c, _toD c, _toM c,  _jmpLT c, _jmpEQ c, _jmpGT c])

codegenExpr :: Expr -> String
codegenExpr (Const Zero)   = "0101010"
codegenExpr (Const One)    = "0111111"
codegenExpr (Neg One)      = "0111010"
codegenExpr (Const D)      = "0001100"
codegenExpr (Const A)      = "0110000"
codegenExpr (Const M)      = "1110000"
codegenExpr (Not D)        = "0001101"
codegenExpr (Not A)        = "0110001"
codegenExpr (Not M)        = "1110001"
codegenExpr (Neg D)        = "0001111"
codegenExpr (Neg A)        = "0110011"
codegenExpr (Neg M)        = "1110011"
codegenExpr (Add D One)    = "0011111"
codegenExpr (Add A One)    = "0110111"
codegenExpr (Add M One)    = "1110111"
codegenExpr (Sub D One)    = "0001110"
codegenExpr (Sub A One)    = "0110010"
codegenExpr (Sub M One)    = "1110010"
codegenExpr (Add D A)      = "0000010"
codegenExpr (Add D M)      = "1000010"
codegenExpr (Sub D A)      = "0010011"
codegenExpr (Sub D M)      = "1010011"
codegenExpr (Sub A D)      = "0000111"
codegenExpr (Sub M D)      = "1000111"
codegenExpr (And D A)      = "0000000"
codegenExpr (And D M)      = "1000000"
codegenExpr (Or D A)       = "0010101"
codegenExpr (Or D M)       = "1010101"
codegenExpr e              = error $ "illegal compute expression: " ++ show e


toBit :: Bool -> Char
toBit True = '1'
toBit False = '0'

codegen :: Command -> String
codegen (CCommand c) = codegenC c
codegen (ACommand (Left n)) = codegenA n

type Symbol = String

type SymbolTable = Map Symbol Integer

data CodeGenState = CGS {
  _symbolTable :: SymbolTable,
  _nextAddr :: Integer,
  _nextROM :: Integer
  }
makeLenses ''CodeGenState


type CodeGenM = WriterT [String] (StateT CodeGenState Identity)

runCodeGenM :: CodeGenM a -> [String]
runCodeGenM cg = snd . runIdentity $ evalStateT (runWriterT cg) initialCodeGenState

initialSymbolTable :: SymbolTable
initialSymbolTable = Map.fromList $ registers ++ [
    ("SP", 0)
  , ("LCL", 1)
  , ("ARG", 2)
  , ("THIS", 3)
  , ("THAT", 4)
  , ("SCREEN", 16384)
  , ("KBD", 24576)
  ]
  where
    registers = map (\i -> ("R" ++ show i, i)) [0..15]

initialCodeGenState :: CodeGenState
initialCodeGenState = CGS initialSymbolTable 16 0

firstPass :: [Command] -> CodeGenM ()
firstPass = mapM_ go
  where
    go :: Command -> CodeGenM ()
    go (LCommand s) = do
      addr <- use nextROM
      symbolTable %= Map.insert s addr
      return ()
    go (CCommand _) = nextROM += 1
    go (ACommand _) = nextROM += 1

secondPass :: [Command] -> CodeGenM ()
secondPass = mapM_ go
  where
    go :: Command -> CodeGenM ()
    go (LCommand _) = return ()
    go (CCommand c) = tell $ [codegenC c]
    go (ACommand (Left n)) = tell $ [codegenA n]
    go (ACommand (Right s)) = do
      addr <- fromMaybe (error $ "secondPass -- unbound symbol: " ++ s) . Map.lookup s <$> use symbolTable
      tell $ [codegenA addr]

codegenS :: [Command] -> String
codegenS p = concat . intersperse "\n" . runCodeGenM $ firstPass p >> secondPass p
