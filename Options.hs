module Options
  ( Options(..)
  , opts
  ) where

import Options.Applicative

data Options = Assemble FilePath FilePath
             | Execute FilePath
             deriving Show

parseAssemble :: Parser Options
parseAssemble = Assemble <$> argument str (metavar "FILE")
          <*> strOption (  short 'o'
                        <> metavar "FILE"
                        <> value "a.out" )

parseExecute :: Parser Options
parseExecute = Execute <$> argument str (metavar "FILE")


parseOptions :: Parser Options
parseOptions = subparser
  (  command "assemble" (info (parseAssemble <**> helper)
                        ( fullDesc ))
  <> command "execute" (info (parseExecute <**> helper)
                        ( fullDesc )))
                        
opts :: ParserInfo Options
opts = info (parseOptions <**> helper) (fullDesc)
