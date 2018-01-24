{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.State (
  State(..),
  S(..),
  ArchState(..),
  BrickUIState(..),
  AppState(..),
  -- * Lenses
  lInputFile,
  lLoader,
  lDiagnosticLog,
  lEchoArea,
  lUIMode,
  lAppState,
  lNonceGenerator,
  lArchState,
  lNonce,
  lAnalysisResult,
  lMinibuffer,
  lFunctionSelector,
  lBlockSelector,
  lBlockViewer,
  lFunctionViewer,
  lKeymap,
  lUIState
  ) where

import           GHC.Generics ( Generic )

import qualified Control.Lens as L
import qualified Data.Generics.Product as GL
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Text as T

import           Surveyor.Core.State
import qualified Surveyor.Arguments as AR
import           Surveyor.Events ( Events )
import           Surveyor.Names ( Names )
import qualified Surveyor.Widget.BlockSelector as BS
import qualified Surveyor.Widget.BlockViewer as BV
import qualified Surveyor.Widget.FunctionSelector as FS
import qualified Surveyor.Widget.FunctionViewer as FV
import qualified Surveyor.Widget.Minibuffer as MB

data BrickUIState arch s =
  BrickUIState { sMinibuffer :: !(MB.Minibuffer (Events s) (Maybe (NG.Nonce s arch)) (AR.Argument arch (Events s) (Maybe (NG.Nonce s arch)) s) AR.TypeRepr T.Text Names)
               -- ^ The persistent state of the minibuffer
               , sFunctionSelector :: !(FS.FunctionSelector arch s)
               -- ^ Functions available in the function selector
               , sBlockSelector :: !(BS.BlockSelector arch s)
               , sBlockViewer :: !(BV.BlockViewer arch s)
               , sFunctionViewer :: !(FV.FunctionViewer arch s)
               }
  deriving (Generic)

lMinibuffer :: L.Lens' (ArchState BrickUIState arch s) (MB.Minibuffer (Events s) (Maybe (NG.Nonce s arch)) (AR.Argument arch (Events s) (Maybe (NG.Nonce s arch)) s) AR.TypeRepr T.Text Names)
lMinibuffer = lUIState . GL.field @"sMinibuffer"

lFunctionSelector :: L.Lens' (ArchState BrickUIState arch s) (FS.FunctionSelector arch s)
lFunctionSelector = lUIState . GL.field @"sFunctionSelector"

lBlockSelector :: L.Lens' (ArchState BrickUIState arch s) (BS.BlockSelector arch s)
lBlockSelector = lUIState . GL.field @"sBlockSelector"

lBlockViewer :: L.Lens' (ArchState BrickUIState arch s) (BV.BlockViewer arch s)
lBlockViewer = lUIState . GL.field @"sBlockViewer"

lFunctionViewer :: L.Lens' (ArchState BrickUIState arch s) (FV.FunctionViewer arch s)
lFunctionViewer = lUIState . GL.field @"sFunctionViewer"


