module Parser where

import Core
import Text.Megaparsec hiding (char)
import Text.Megaparsec.String
import qualified Text.Megaparsec.Char as Char

--import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as Lexer
import Data.Functor (($>))
import Control.Monad

whitespace :: Parser ()
whitespace = Lexer.space space lineCmt blockCmt
  where
    lineCmt = Lexer.skipLineComment "//"
    blockCmt = Lexer.skipBlockComment "/*" "*/" <|> Lexer.skipBlockComment "/**" "*/"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

symbol :: String -> Parser String
symbol = Lexer.symbol whitespace

integer :: Parser Integer
integer = check =<< lexeme Lexer.decimal
  where
    check n = if 0 <= n && n <= 32767
              then return n
              else fail $ unwords ["Integer constant", show n, "out of range."]

typeAnn :: Parser Type
typeAnn = keyword "int" $> TypeInt
          <|> keyword "char" $> TypeChar
          <|> keyword "boolean" $> TypeBoolean

keyword :: String -> Parser ()
keyword w = lexeme (string w *> notFollowedBy alphaNumChar)

keywords :: [String]
keywords = [ "class"
           , "constructor"
           , "function"
           , "method"
           , "field"
           , "static"
           , "var"
           , "int"
           , "char"
           , "boolean"
           , "void"
           , "true"
           , "false"
           , "null"
           , "this"
           , "let"
           , "do"
           , "if"
           , "else"
           , "while"
           , "return"
           ]

braces, parens, brackets :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")

dot :: Parser ()
dot = void $ symbol "."

comma :: Parser ()
comma = void $ symbol ","

semi :: Parser ()
semi = void $ symbol ";"

plus :: Parser ()
plus = void $ symbol "+"

dash :: Parser ()
dash = void $ symbol "-"

asterix :: Parser ()
asterix = void $ symbol "*"

slash :: Parser ()
slash = void $ symbol "/"

ampersand :: Parser ()
ampersand = void $ symbol "&"

pipe :: Parser ()
pipe = void $ symbol "|"

leftCaret, rightCaret :: Parser ()
leftCaret = void $ symbol "<"
rightCaret = void $ symbol ">"

equal :: Parser ()
equal = void $ symbol "="

char :: Char -> Parser Char
char = lexeme . Char.char

underscore :: Parser ()
underscore = void $ symbol "_"

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> (char '_' <|> letterChar) <*> many alphaNumChar
    idChar = char '_' <|> alphaNumChar
    check x = if x `elem` keywords
              then fail $ unwords ["keyword", show x, " cannot be an identifier"]
              else return x

stringConstant :: Parser String
stringConstant = between (symbol "\"") (symbol "\"") (Char.notChar '"')
