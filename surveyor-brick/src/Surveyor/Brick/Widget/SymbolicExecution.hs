{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
module Surveyor.Brick.Widget.SymbolicExecution (
  SymbolicExecutionManager,
  symbolicExecutionManager,
  renderSymbolicExecutionManager,
  handleSymbolicExecutionManagerEvent,
  symbolicExecutionManagerState
  ) where

import qualified Brick as B
import           Data.Parameterized.Some ( Some(..) )

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Brick.Widget.SymbolicExecution.Configuration as SEC
import qualified Surveyor.Brick.Widget.SymbolicExecution.Setup as SES
import qualified Surveyor.Core as C


data SymbolicExecutionManager e arch s =
  SymbolicExecutionManager { managerState :: Some (C.SymbolicExecutionState arch s)
                           }

symbolicExecutionManager :: Some (C.SymbolicExecutionState arch s)
                         -> SymbolicExecutionManager e arch s
symbolicExecutionManager = SymbolicExecutionManager

renderSymbolicExecutionManager :: SymbolicExecutionManager e arch s
                               -> B.Widget Names
renderSymbolicExecutionManager (managerState -> Some st) =
  case st of
    C.Configuring {} -> SEC.renderSymbolicExecutionConfigurator st
    C.Initializing {} -> SES.renderSymbolicExecutionSetup st

handleSymbolicExecutionManagerEvent :: B.BrickEvent Names e
                                    -> SymbolicExecutionManager e arch s
                                    -> B.EventM Names (SymbolicExecutionManager e arch s)
handleSymbolicExecutionManagerEvent evt (managerState -> Some st) =
  case st of
    C.Configuring {} -> toManager <$> SEC.handleSymbolicExecutionConfiguratorEvent evt st
    C.Initializing {} -> toManager <$> SES.handleSymbolicExecutionSetupEvent evt st

toManager :: C.SymbolicExecutionState arch s k -> SymbolicExecutionManager e arch s
toManager = SymbolicExecutionManager . Some

symbolicExecutionManagerState :: SymbolicExecutionManager e arch s
                              -> Some (C.SymbolicExecutionState arch s)
symbolicExecutionManagerState = managerState
