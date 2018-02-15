module Main where

import Core
import System.Environment
import System.FilePath
import System.Directory
import Text.Megaparsec

main :: IO ()
main = do
  [input] <- getArgs
  case splitExtension input of
    (_, "") -> processDirectory input
    (_, ".vm") -> processFile input
    _ -> error $ "unsupported input file extension: " ++ input

processDirectory :: FilePath -> IO ()
processDirectory input = do
  vmFiles <- fmap (input </>) . filter ((==".vm") . snd . splitExtension) <$> listDirectory input
  program <- mapM parseFile vmFiles
  writeFile (input </> takeBaseName input -<.> ".asm") $ translate program

processFile :: FilePath -> IO ()
processFile input = do
  cu <- parseFile input
  writeFile (input -<.> ".asm") $ translate [cu]

parseFile :: FilePath -> IO CompilationUnit
parseFile file = do
  input <- readFile file
  case parse (pCompilationUnit $ takeBaseName file) file input of
    Left e -> error $ parseErrorPretty e
    Right c -> return c
