module Main where

import Parser
import Options
import Asm
import VM

import Data.Binary
import qualified Data.ByteString.Lazy as B
import Options.Applicative

main :: IO ()
main = do
  options <- execParser opts
  case options of
    Assemble infile outfile -> do
      asm <- parse <$> readFile infile
      case asm of
        Nothing -> putStrLn "Parse Failure"
        Just prog -> B.writeFile outfile (encode . compile $ prog)
    Execute infile -> do
      prog <- decode <$> B.readFile infile
      runBytecode prog
  

