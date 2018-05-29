{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Brick.State (
  C.State(..),
  C.S(..),
  C.ArchState(..),
  BrickUIState(..),
  BrickUIExtension(..),
  C.AppState(..),
  -- * Lenses
  C.lInputFile,
  C.lLoader,
  C.lDiagnosticLog,
  C.lEchoArea,
  C.lUIMode,
  C.lAppState,
  C.lNonceGenerator,
  C.lArchState,
  C.lNonce,
  C.lAnalysisResult,
  lMinibuffer,
  lFunctionSelector,
  lBlockSelector,
  lBlockViewer,
  lFunctionViewer,
  C.lKeymap,
  C.lUIState
  ) where

import           GHC.Generics ( Generic )

import qualified Control.Lens as L
import qualified Data.Generics.Product as GL
import qualified Data.Text as T

import qualified Surveyor.Core as C
import           Surveyor.Brick.Names ( Names )
import qualified Surveyor.Brick.Widget.BlockSelector as BS
import qualified Surveyor.Brick.Widget.BlockViewer as BV
import qualified Surveyor.Brick.Widget.FunctionSelector as FS
import qualified Surveyor.Brick.Widget.FunctionViewer as FV
import qualified Surveyor.Brick.Widget.Minibuffer as MB

data BrickUIState arch s =
  BrickUIState { sFunctionSelector :: !(FS.FunctionSelector arch s)
               -- ^ Functions available in the function selector
               , sBlockSelector :: !(BS.BlockSelector arch s)
               , sBlockViewer :: !(BV.BlockViewer arch s)
               , sFunctionViewer :: !(FV.FunctionViewer arch s)
               }
  deriving (Generic)

data BrickUIExtension s =
  BrickUIExtension { sMinibuffer :: !(MB.Minibuffer (C.Events s) (Maybe (C.SomeNonce s)) (C.Argument (C.Events s) (Maybe (C.SomeNonce s)) s) C.TypeRepr T.Text Names)
                   -- ^ The persistent state of the minibuffer
                   }
  deriving (Generic)

lMinibuffer :: L.Lens' (BrickUIExtension s) (MB.Minibuffer (C.Events s) (Maybe (C.SomeNonce s)) (C.Argument (C.Events s) (Maybe (C.SomeNonce s)) s) C.TypeRepr T.Text Names)
lMinibuffer = GL.field @"sMinibuffer"

lFunctionSelector :: L.Lens' (C.ArchState BrickUIState arch s) (FS.FunctionSelector arch s)
lFunctionSelector = C.lUIState . GL.field @"sFunctionSelector"

lBlockSelector :: L.Lens' (C.ArchState BrickUIState arch s) (BS.BlockSelector arch s)
lBlockSelector = C.lUIState . GL.field @"sBlockSelector"

lBlockViewer :: L.Lens' (C.ArchState BrickUIState arch s) (BV.BlockViewer arch s)
lBlockViewer = C.lUIState . GL.field @"sBlockViewer"

lFunctionViewer :: L.Lens' (C.ArchState BrickUIState arch s) (FV.FunctionViewer arch s)
lFunctionViewer = C.lUIState . GL.field @"sFunctionViewer"


