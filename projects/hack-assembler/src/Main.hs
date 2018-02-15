module Main where
import System.Environment
import Core

main :: IO ()
main = do
  [inputFile] <- getArgs
  putStrLn =<< codegenS <$> parseFile inputFile
