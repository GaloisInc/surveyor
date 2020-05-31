{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
-- | State extensions for the brick UI
module Surveyor.Brick.Extension (
  BrickUIState(..),
  BrickUIExtension(..),
  mkExtension,
  updateMinibufferCompletions,
  -- * Lenses
  minibufferL,
  minibufferG,
  functionSelectorL,
  functionSelectorG,
  blockSelectorL,
  blockSelectorG,
  blockViewersL,
  blockViewerG,
  functionViewersL,
  functionViewerG,
  symbolicExecutionManagerL,
  symbolicExecutionManagerG
  ) where

import           Control.Lens ( (^.), (&), (%~) )
import qualified Control.Lens as L
import qualified Control.NF as NF
import qualified Data.Generics.Product as GL
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Text as T
import qualified Data.Vector as V
import           GHC.Generics ( Generic )
import qualified Surveyor.Core as C

import qualified Brick.Widget.Minibuffer as MBW

import qualified Surveyor.Brick.Command as BC
import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Brick.Widget.BlockSelector as BS
import qualified Surveyor.Brick.Widget.BlockViewer as BV
import qualified Surveyor.Brick.Widget.FunctionSelector as FS
import qualified Surveyor.Brick.Widget.FunctionViewer as FV
import qualified Surveyor.Brick.Widget.Minibuffer as MB
import qualified Surveyor.Brick.Widget.SymbolicExecution as SEM

-- | State specific to the Brick UI
--
-- This is mostly storage for widgets
data BrickUIState arch s =
  BrickUIState { sFunctionSelector :: !(FS.FunctionSelector arch s)
               -- ^ Functions available in the function selector
               , sBlockSelector :: !(BS.BlockSelector arch s)
               , sBlockViewers :: !(MapF.MapF (C.IRRepr arch) (BV.BlockViewer arch s))
               , sFunctionViewer :: !(MapF.MapF (C.IRRepr arch) (FV.FunctionViewer arch s))
               , sSymbolicExecutionManager :: !(SEM.SymbolicExecutionManager (C.Events s (C.S BrickUIExtension BrickUIState)) arch s)
               }
  deriving (Generic)

-- | Extra UI extensions for the Brick UI
--
-- This differs from 'BrickUIState' in that it is not parameterized by the
-- architecture (@arch@).  That is important, as there is not always an active
-- architecture.  Objects in this extension state can always be available (e.g.,
-- the minibuffer).
data BrickUIExtension s =
  BrickUIExtension { sMinibuffer :: !(MB.Minibuffer (C.SurveyorCommand s (C.S BrickUIExtension BrickUIState)) T.Text Names)
                   -- ^ The persistent state of the minibuffer
                   }
  deriving (Generic)

mkExtension :: (C.Events s (C.S BrickUIExtension BrickUIState) -> IO ())
            -> PN.Nonce s arch
            -> (String -> Maybe (C.SomeAddress s)) -> T.Text -> BrickUIExtension s
mkExtension emitEvent archNonce addrParser prompt =
  BrickUIExtension { sMinibuffer = MB.minibuffer addrParser updater MinibufferEditor MinibufferCompletionList prompt (C.allCommands ++ BC.extraCommands)
                   }
  where
    updater = updateMinibufferCompletions emitEvent archNonce

updateMinibufferCompletions :: (C.Events s (C.S BrickUIExtension BrickUIState) -> IO ())
                            -> PN.Nonce s arch
                            -> (T.Text -> V.Vector T.Text -> IO ())
updateMinibufferCompletions emitEvent archNonce = \t matches -> do
  let stateTransformer matches' state
        | mb <- state ^. C.lUIExtension . minibufferL
        , MBW.activeCompletionTarget mb == Just t =
          state & C.lUIExtension . minibufferL %~ MBW.setCompletions matches'
        | otherwise = state
  emitEvent (C.AsyncStateUpdate archNonce (NF.nf matches) stateTransformer)


minibufferL :: L.Lens' (BrickUIExtension s) (MB.Minibuffer (C.SurveyorCommand s (C.S BrickUIExtension BrickUIState)) T.Text Names)
minibufferL = GL.field @"sMinibuffer"

minibufferG :: L.Getter (BrickUIExtension s) (MB.Minibuffer (C.SurveyorCommand s (C.S BrickUIExtension BrickUIState)) T.Text Names)
minibufferG = L.to (^. minibufferL)

functionSelectorL :: L.Lens' (C.ArchState BrickUIState arch s) (FS.FunctionSelector arch s)
functionSelectorL = C.lUIState . GL.field @"sFunctionSelector"

functionSelectorG :: L.Getter (C.ArchState BrickUIState arch s) (FS.FunctionSelector arch s)
functionSelectorG = L.to (^. functionSelectorL)

blockSelectorL :: L.Lens' (C.ArchState BrickUIState arch s) (BS.BlockSelector arch s)
blockSelectorL = C.lUIState . GL.field @"sBlockSelector"

blockSelectorG :: L.Getter (C.ArchState BrickUIState arch s) (BS.BlockSelector arch s)
blockSelectorG = L.to (^. blockSelectorL)

blockViewersL :: L.Lens' (C.ArchState BrickUIState arch s) (MapF.MapF (C.IRRepr arch) (BV.BlockViewer arch s))
blockViewersL = C.lUIState . GL.field @"sBlockViewers"

blockViewerG :: C.IRRepr arch ir -> L.Getter (C.ArchState BrickUIState arch s) (Maybe (BV.BlockViewer arch s ir))
blockViewerG rep = L.to (\as -> MapF.lookup rep (as ^. blockViewersL))

functionViewersL :: L.Lens' (C.ArchState BrickUIState arch s) (MapF.MapF (C.IRRepr arch) (FV.FunctionViewer arch s))
functionViewersL = C.lUIState . GL.field @"sFunctionViewer"

functionViewerG :: C.IRRepr arch ir -> L.Getter (C.ArchState BrickUIState arch s) (Maybe (FV.FunctionViewer arch s ir))
functionViewerG rep = L.to (\as -> MapF.lookup rep (as ^. functionViewersL))

symbolicExecutionManagerL :: L.Lens' (C.ArchState BrickUIState arch s) (SEM.SymbolicExecutionManager (C.Events s (C.S BrickUIExtension BrickUIState)) arch s)
symbolicExecutionManagerL = C.lUIState . GL.field @"sSymbolicExecutionManager"

symbolicExecutionManagerG :: L.Getter (C.ArchState BrickUIState arch s) (SEM.SymbolicExecutionManager (C.Events s (C.S BrickUIExtension BrickUIState)) arch s)
symbolicExecutionManagerG = L.to (^. symbolicExecutionManagerL)
