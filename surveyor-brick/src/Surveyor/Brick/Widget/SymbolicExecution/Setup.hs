{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.Brick.Widget.SymbolicExecution.Setup (
  SymbolicExecutionSetup,
  symbolicExecutionSetup,
  renderSymbolicExecutionSetup,
  handleSymbolicExecutionSetupEvent,
  withCurrentState
  ) where

import qualified Brick as B
import           Control.Lens ( (^.) )
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.Simulator as CS

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C

data SymbolicExecutionSetup arch s =
  forall solver fm init reg .
  SymbolicExecutionSetup { setupState :: C.SymbolicState arch s solver fm init reg
                         }

withCurrentState :: SymbolicExecutionSetup arch s
                 -> (forall init reg solver fm . C.SymbolicState arch s solver fm init reg -> a)
                 -> a
withCurrentState SymbolicExecutionSetup { setupState = st } k = k st

symbolicExecutionSetup :: C.SymbolicState arch s solver fm init reg
                       -> SymbolicExecutionSetup arch s
symbolicExecutionSetup = SymbolicExecutionSetup

renderSymbolicExecutionSetup :: SymbolicExecutionSetup arch s
                             -> B.Widget Names
renderSymbolicExecutionSetup SymbolicExecutionSetup { setupState = st } =
  B.vBox [ B.hBox [ B.txt "Solver: ", B.txt (T.pack (show solver))
                  , B.txt "Float Mode: ", B.txt (T.pack (show fm))
                  ]
         ]
  where
    solver = C.symbolicConfig st ^. C.configSolverL
    fm = C.symbolicConfig st ^. C.configFloatReprL

handleSymbolicExecutionSetupEvent :: Vty.Event
                                  -> SymbolicExecutionSetup arch s
                                  -> B.EventM Names (SymbolicExecutionSetup arch s)
handleSymbolicExecutionSetupEvent _ s = return s
