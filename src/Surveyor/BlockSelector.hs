module Surveyor.BlockSelector (
  BlockSelector,
  emptyBlockSelector,
  blockSelector,
  handleBlockSelectorEvent,
  renderBlockSelector
  ) where

import qualified Brick as B
import qualified Data.Text as T
import qualified Data.Text.Zipper.Generic as ZG
import qualified Data.Vector as V
import qualified Graphics.Vty as V
import           Text.Printf ( printf )

import qualified Brick.Widget.FilterList as FL
import qualified Surveyor.Architecture as A
import           Surveyor.Names ( Names(..) )

data BlockSelector arch s = BlockSelector (A.Address arch s) (FL.FilterList Names T.Text (A.Block arch s))
                          | NoBlock

emptyBlockSelector :: BlockSelector arch s
emptyBlockSelector = NoBlock

blockSelector :: (A.Architecture arch s) => B.AttrName -> A.Address arch s -> [A.Block arch s] -> BlockSelector arch s
blockSelector focAttr addr blocks =
  case blocks of
    [] -> NoBlock
    bs -> BlockSelector addr (FL.filterList flcfg (V.fromList bs))
  where
    flcfg = FL.FilterListConfig { FL.flEditorName = BlockSelectEditor
                                , FL.flListName = BlockSelectList
                                , FL.flEditorPosition = FL.Bottom
                                , FL.flMaxListHeight = Nothing
                                , FL.flToText = blockToText
                                , FL.flRenderListItem = renderBlockItem focAttr
                                , FL.flRenderEditorContent = renderEditorContent
                                }

blockToText :: (A.Architecture arch s) => A.Block arch s -> T.Text
blockToText b = A.prettyAddress (A.blockAddress b)

renderBlockItem :: (A.Architecture arch s) => B.AttrName -> Bool -> A.Block arch s -> B.Widget Names
renderBlockItem focAttr isFocused b =
  let xfrm = if isFocused then B.withAttr focAttr else id
  in xfrm (B.txt (A.prettyAddress (A.blockAddress b)))

renderEditorContent :: (Monoid t, ZG.GenericTextZipper t) => [t] -> B.Widget Names
renderEditorContent txts = B.str (ZG.toList (mconcat txts))

handleBlockSelectorEvent :: V.Event -> BlockSelector arch s -> B.EventM Names (BlockSelector arch s)
handleBlockSelectorEvent evt bsel =
  case bsel of
    NoBlock -> return bsel
    BlockSelector addr fl -> do
      fl' <- FL.handleFilterListEvent return evt fl
      return (BlockSelector addr fl')

renderBlockSelector :: (A.Architecture arch s) => BlockSelector arch s -> B.Widget Names
renderBlockSelector bsel =
  case bsel of
    NoBlock -> B.emptyWidget
    BlockSelector addr fl ->
      B.vBox [ B.str (printf "Basic Block %s" (A.prettyAddress addr))
             , FL.renderFilterList (const B.emptyWidget) True fl
             ]
