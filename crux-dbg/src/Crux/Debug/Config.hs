{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Crux.Debug.Config (
  DebugOptions(..),
  debugConfig
  ) where

import qualified Crux as C
import qualified Lang.Crucible.LLVM.MemModel as CLM

data DebugOptions =
  DebugOptions { entryPoint :: Maybe String
               , interactive :: Bool
               , laxArithmetic :: Bool
               , memoryOptions :: CLM.MemOptions
               }

debugConfig :: C.Config DebugOptions
debugConfig =
  C.Config { C.cfgFile = do
               ep <- C.sectionMaybe "entry-point" C.fileSpec "The entry point to simulate from (defaults to 'main')"
               int <- C.section "interactive" C.yesOrNoSpec False "Start in interactive (TUI) mode"
               lax <- C.section "lax-arithmetic" C.yesOrNoSpec False "Turn on lax enforcement of arithmetic overflow"
               memOpts <- do laxPtrOrd <- C.section "lax-pointer-ordering" C.yesOrNoSpec False
                                               "Allow order comparisons between pointers from different allocation blocks"
                             laxConstEq <-  C.section "lax-constant-equality" C.yesOrNoSpec False
                                               "Allow equality comparisons between pointers to constant data"
                             return CLM.MemOptions { CLM.laxPointerOrdering = laxPtrOrd
                                                   , CLM.laxConstantEquality= laxConstEq
                                                   }
               return DebugOptions { entryPoint = ep
                                   , interactive = int
                                   , laxArithmetic = lax
                                   , memoryOptions = memOpts
                                   }
           , C.cfgEnv = []
           , C.cfgCmdLineFlag =
             [ C.Option [] ["entry-point"] "The entry point to simulate from (defaults to 'main')"
               $ C.ReqArg "FUNC" $ \f opts -> Right opts { entryPoint = Just f }
             , C.Option ['i'] ["interactive"] "Start in interactive (TUI) mode"
               $ C.NoArg $ \opts -> Right opts { interactive = True }
             , C.Option [] ["lax-arithmetic"] "Turn on lax enforcement of arithmetic overflow"
               $ C.NoArg $ \opts -> Right opts { laxArithmetic = True }
             , C.Option [] ["lax-pointers"] "Turn on lax enforcement of pointer comparison rules"
               $ C.NoArg $ \opts -> Right opts { memoryOptions = CLM.laxPointerMemOptions }
             ]
           }
