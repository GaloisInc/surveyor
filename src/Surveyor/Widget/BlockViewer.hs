module Surveyor.Widget.BlockViewer (
  BlockViewer,
  emptyBlockViewer,
  blockViewer,
  handleBlockViewerEvent,
  renderBlockViewer
  ) where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Data.Vector as V
import qualified Graphics.Vty as V
import           Text.Printf ( printf )

import qualified Surveyor.Architecture as A
import           Surveyor.Names ( Names )

data BlockViewer arch s =
  BlockViewer { bvBlock :: Maybe (A.Block arch s)
              }

emptyBlockViewer :: BlockViewer arch s
emptyBlockViewer = BlockViewer Nothing

blockViewer :: A.Block arch s -> BlockViewer arch s
blockViewer b = BlockViewer { bvBlock = Just b }

handleBlockViewerEvent :: V.Event -> BlockViewer arch s -> B.EventM Names (BlockViewer arch s)
handleBlockViewerEvent _ bv = return bv

renderBlockViewer :: (A.Architecture arch s) => BlockViewer arch s -> B.Widget Names
renderBlockViewer bv =
  case bvBlock bv of
    Nothing -> B.emptyWidget
    Just b ->
      let header = B.str (printf "Basic Block %s" (A.prettyAddress (A.blockAddress b)))
          renderInstruction (addr, i) = B.hBox [ B.padRight (B.Pad 2) (B.txt (A.prettyAddress addr))
                                               , B.txt (A.prettyInstruction i)
                                               ]
      in B.borderWithLabel header (B.vBox (map renderInstruction (A.blockInstructions b)))

