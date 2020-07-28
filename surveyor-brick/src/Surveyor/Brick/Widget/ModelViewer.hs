{-# LANGUAGE OverloadedStrings #-}
-- | A viewer for Crux models
module Surveyor.Brick.Widget.ModelViewer
  ( renderModelViewer,
  ) where

import qualified Brick as B
import qualified Crux.Types as CT
import qualified Surveyor.Brick.Names as SBN

renderModelViewer :: CT.ModelView -> B.Widget SBN.Names
renderModelViewer (CT.ModelView _) = B.txt "modelVals"
