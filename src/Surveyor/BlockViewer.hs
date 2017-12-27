module Surveyor.BlockViewer (
  BlockViewer,
  blockViewer,
  handleBlockViewerEvent,
  renderBlockViewer
  ) where

import qualified Brick as B
import qualified Graphics.Vty as V
import qualified Surveyor.Architecture as A
import           Surveyor.Names ( Names )

data BlockViewer arch s =
  BlockViewer { bvBlock :: A.Block arch s
              }

blockViewer :: A.Block arch s -> BlockViewer arch s
blockViewer = BlockViewer

handleBlockViewerEvent :: V.Event -> BlockViewer arch s -> B.EventM Names (BlockViewer arch s)
handleBlockViewerEvent = undefined

renderBlockViewer :: BlockViewer arch s -> B.Widget Names
renderBlockViewer = undefined
