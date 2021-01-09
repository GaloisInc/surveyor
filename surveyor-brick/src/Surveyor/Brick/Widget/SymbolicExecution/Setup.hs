{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.Brick.Widget.SymbolicExecution.Setup (
  renderSymbolicExecutionSetup,
  handleSymbolicExecutionSetupEvent
  ) where

import qualified Brick as B
import           Control.Lens ( (^.) )
import qualified Data.Text as T

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C

renderSymbolicExecutionSetup :: C.SymbolicExecutionState arch s C.SetupArgs
                             -> B.Widget Names
renderSymbolicExecutionSetup (C.Initializing st _initRegs) =
  B.vBox [ B.hBox [ B.txt "Solver: ", B.txt (T.pack (show solver))
                  , B.padLeft (B.Pad 10) (B.txt "Float Mode: ")
                  , B.txt (T.pack (show fm))
                  ]
         ]
  where
    solver = C.symbolicConfig st ^. C.configSolverL
    fm = C.symbolicConfig st ^. C.configFloatReprL

handleSymbolicExecutionSetupEvent :: B.BrickEvent Names e
                                  -> C.SymbolicExecutionState arch s C.SetupArgs
                                  -> B.EventM Names ()
handleSymbolicExecutionSetupEvent _ _ = return ()
