{-# LANGUAGE RecordWildCards #-}
module Main where

import Core
import System.FilePath
import System.Directory
import Text.Megaparsec
import Control.Monad (forM_, when)
import Options.Applicative
import Data.Semigroup ((<>))

data Configuration = Configuration {
    _dumpAST :: Bool,
    _printVersion :: Bool,
    _targetPaths :: [FilePath]
  }

pConfiguration :: Parser Configuration
pConfiguration = Configuration
  <$> flag False True (long "dump-ast" <> short 'd' <> help "Dump intermediate representation of AST as xml")
  <*> flag False True (long "version" <> short 'v' <> help "Print version information")
  <*> many (argument str (metavar "TARGET" <> help "Jack file or directory of Jack files to be compiled"))


execute :: Configuration -> IO ()
execute Configuration{..} = do
  when _printVersion
    printVersion
  forM_ _targetPaths $ \path ->
    case splitExtension path of
      (_, "") -> processDirectory _dumpAST path
      (_, ".jack") -> compileClassFile _dumpAST path
      _ -> error $ "Unsupported unput file extension: " ++ path

main :: IO ()
main = execute =<< execParser options
  where
    options = info (pConfiguration <**> helper)
      ( fullDesc <> progDesc "hackc Jack Compiler")

printVersion :: IO ()
printVersion =
  putStrLn "hackc version 0"

processDirectory :: Bool -> FilePath -> IO ()
processDirectory dumpAST input = do
  jackFiles <- fmap (input </>) . filter ((== ".jack") . snd . splitExtension) <$> listDirectory input
  forM_ jackFiles (compileClassFile dumpAST)

compileClassFile :: Bool -> FilePath -> IO ()
compileClassFile dumpAST input = do
  ast <- parseFile input
  when dumpAST $
    writeFile (input -<.> ".xml") . xmlPrint $ ast
  writeFile (input -<.> ".vm") . compile =<< parseFile input

parseFile :: FilePath -> IO ClassDecl
parseFile file = do
  input <- readFile file
  case parse pClassDecl file input of
    Left e -> error $ parseErrorPretty e
    Right c -> return c
