module Main ( main ) where

import           Data.Monoid
import qualified Options.Applicative as O

import           Surveyor.QML

data Options =
  Options { oInputFile :: Maybe FilePath
          , oUI :: Maybe FilePath
          }

options :: O.Parser Options
options = Options <$> O.optional (O.strArgument (O.metavar "FILE"
                                                <> O.help "The file to analyze"))
                  <*> O.optional (O.strOption (O.metavar "FILE"
                                              <> O.long "ui"
                                              <> O.short 'u'
                                              <> O.help "The UI file to load"))

main :: IO ()
main = O.execParser opts >>= mainWith
  where
    opts = O.info (O.helper <*> options) components
    components = mconcat [ O.fullDesc
                         , O.progDesc "An interactive UI for exploring executables"
                         , O.header "surveyor"
                         ]

mainWith :: Options -> IO ()
mainWith opts = surveyor (oInputFile opts) (oUI opts)

