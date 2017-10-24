module Lib where

import Text.Megaparsec
import Text.Megaparsec.String

parseACmd :: Parser Command
parseACmd = ACommand <$ char '@' <*> integer

data Dst
data Comp
data Jmp

data Command = ACommand Int
             | CCommand Dst Comp Jmp
             | LCommand String

type Program = [Command]

parseLines :: Parser a -> String -> [a]
parseLines p s = case runParser (manyTill (p <* eol) eof) "Advent: parseLines" s of
  Left e -> error $ parseErrorPretty e
  Right as -> as
