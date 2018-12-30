module Main ( main ) where

import           Data.Monoid
import qualified Options.Applicative as O
import           Prelude

import           Surveyor.Brick

data Options =
  Options { oInputFile :: Maybe FilePath
          -- ^ The first file to analyze; this is optional, and can be changed
          -- in the application
          }

options :: O.Parser Options
options = Options <$> O.optional (O.strArgument (O.metavar "FILE"
                                                <> O.help "The file to analyze"))

main :: IO ()
main = O.execParser opts >>= mainWith
  where
    opts = O.info (O.helper <*> options) components
    components = mconcat [ O.fullDesc
                         , O.progDesc "An interactive UI for exploring executables"
                         , O.header "surveyor"
                         ]

mainWith :: Options -> IO ()
mainWith opts = surveyor (oInputFile opts)
