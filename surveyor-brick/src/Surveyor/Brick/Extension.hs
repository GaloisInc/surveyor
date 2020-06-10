{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- | State extensions for the brick UI
module Surveyor.Brick.Extension (
  BrickUIState(..),
  BrickUIExtension(..),
  BrickUIEvent(..),
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
import           Data.Kind ( Type )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Text as T
import qualified Data.Vector as V
import           GHC.Generics ( Generic )
import qualified Surveyor.Core as C

import qualified Brick.Widget.Minibuffer as MBW

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Brick.Widget.BlockSelector as BS
import qualified Surveyor.Brick.Widget.BlockViewer as BV
import qualified Surveyor.Brick.Widget.FunctionSelector as FS
import qualified Surveyor.Brick.Widget.FunctionViewer as FV
import qualified Surveyor.Brick.Widget.Minibuffer as MB
import qualified Surveyor.Brick.Widget.SymbolicExecution as SEM

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

L.makeLensesFor
  [("sMinibuffer", "minibufferL")]
  ''BrickUIExtension

L.makeLensesFor
  [ ("sFunctionSelector", "functionSelectorL")
  , ("sBlockSelector", "blockSelectorL")
  , ("sBlockViewers", "blockViewersL")
  , ("sFunctionViewer", "functionViewersL")
  , ("sSymbolicExecutionManager", "symbolicExecutionManagerL") ]
  ''BrickUIState

type instance C.EventExtension (C.S BrickUIExtension BrickUIState) = BrickUIEvent

-- | Events specific to the Brick UI
--
-- Note that it isn't parameterized by the @st@ type like the base event because
-- we can actually mention the full state type here, so it doesn't need to be a
-- parameter.
data BrickUIEvent s (st :: Type -> Type -> Type) where
  ShowSummary :: BrickUIEvent s st
  ShowDiagnostics :: BrickUIEvent s st
  OpenMinibuffer :: BrickUIEvent s st

  ListBlocks :: PN.Nonce s arch -> [C.Block arch s] -> BrickUIEvent s st
  ListFunctions :: PN.Nonce s arch -> [C.FunctionHandle arch s] -> BrickUIEvent s st
  FindFunctionsContaining :: PN.Nonce s arch -> Maybe (C.Address arch s) -> BrickUIEvent s st
  FindBlockContaining :: PN.Nonce s arch -> C.Address arch s -> BrickUIEvent s st

instance C.ToEvent s (C.S BrickUIExtension BrickUIState) BrickUIEvent where
  toEvent = C.ExtensionEvent

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

minibufferG :: L.Getter (BrickUIExtension s) (MB.Minibuffer (C.SurveyorCommand s (C.S BrickUIExtension BrickUIState)) T.Text Names)
minibufferG = L.to (^. minibufferL)

functionSelectorG :: L.Getter (C.ArchState BrickUIState arch s) (FS.FunctionSelector arch s)
functionSelectorG = L.to (^. C.lUIState . functionSelectorL)

blockSelectorG :: L.Getter (C.ArchState BrickUIState arch s) (BS.BlockSelector arch s)
blockSelectorG = L.to (^. C.lUIState . blockSelectorL)

blockViewerG :: C.IRRepr arch ir -> L.Getter (C.ArchState BrickUIState arch s) (Maybe (BV.BlockViewer arch s ir))
blockViewerG rep = L.to (\as -> MapF.lookup rep (as ^. C.lUIState . blockViewersL))

functionViewerG :: C.IRRepr arch ir -> L.Getter (C.ArchState BrickUIState arch s) (Maybe (FV.FunctionViewer arch s ir))
functionViewerG rep = L.to (\as -> MapF.lookup rep (as ^. C.lUIState . functionViewersL))

symbolicExecutionManagerG :: L.Getter (C.ArchState BrickUIState arch s) (SEM.SymbolicExecutionManager (C.Events s (C.S BrickUIExtension BrickUIState)) arch s)
symbolicExecutionManagerG = L.to (^. C.lUIState . symbolicExecutionManagerL)
