-- | A list widget that allows for searching/filtering via user-programmable
-- matching criteria, as well as completion.
module Brick.Widget.FilterList (
  FilterListConfig(..),
  EditorPosition(..),
  FilterList,
  filterList,
  renderFilterList,
  handleFilterListEvent,
  updateList,
  selectedItem,
  editorContents,
  listContents,
  resetList
  ) where

import qualified Brick as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.Widgets.List as B
import qualified Control.Lens as L
import           Control.Monad ( guard )
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper.Generic as ZG
import qualified Data.Vector as V
import qualified Graphics.Vty as V

data EditorPosition = Top | Bottom

data FilterList n t e =
  FilterList { editor :: !(B.Editor t n)
             , allItems :: V.Vector e
             , matchingItems :: !(B.List n e)
             , editorPosition :: EditorPosition
             , maxListHeight :: Maybe Int
             , toText :: e -> t
             , renderListItem :: Bool -> e -> B.Widget n
             , renderEditorContent :: [t] -> B.Widget n
             }

data FilterListConfig n t e =
  FilterListConfig { flEditorName :: n
                   -- ^ The name assigned to the editor widget
                   , flListName :: n
                   -- ^ The name assigned to the list widget
                   , flEditorPosition :: EditorPosition
                   -- ^ The position of the editor relative to the list
                   , flMaxListHeight :: Maybe Int
                   -- ^ The maximum number of items to show in the list
                   , flToText :: e -> t
                   -- ^ A function to convert a list item into text displayed in the editor
                   , flRenderListItem :: Bool -> e -> B.Widget n
                   -- ^ A function to render an item in the filterable list
                   , flRenderEditorContent :: [t] -> B.Widget n
                   -- ^ A function to draw the content of the editor widget
                   }

filterList :: (ZG.GenericTextZipper t)
           => FilterListConfig n t e
           -> V.Vector e
           -> FilterList n t e
filterList cfg items =
  FilterList { editor = B.editor (flEditorName cfg) (Just 1) mempty
             , allItems = items
             , matchingItems = B.list (flListName cfg) items 1
             , editorPosition = flEditorPosition cfg
             , maxListHeight = flMaxListHeight cfg
             , toText = flToText cfg
             , renderListItem = flRenderListItem cfg
             , renderEditorContent = flRenderEditorContent cfg
             }

renderFilterList :: (Ord n, Show n, B.TextWidth t, ZG.GenericTextZipper t)
                 => (Int -> B.Widget n)
                 -> Bool
                 -> FilterList n t e
                 -> B.Widget n
renderFilterList renderPrompt hasFocus fl =
  case editorPosition fl of
    Top -> B.vBox [ B.vLimit 1 editorLine
                  , clampHeight matchesList
                  ]
    Bottom -> B.vBox [ clampHeight matchesList
                     , B.vLimit 1 editorLine
                     ]
  where
    clampHeight = maybe id (\n -> B.vLimit (min nMatches n)) (maxListHeight fl)
    matches = matchingItems fl L.^. B.listElementsL
    nMatches = V.length matches
    editorLine = B.hBox [ renderPrompt (V.length matches)
                        , B.renderEditor (renderEditorContent fl) hasFocus (editor fl)
                        ]
    matchesList =
      if V.length matches == 0
      then B.emptyWidget
      else B.renderList (renderListItem fl) False (matchingItems fl)

handleFilterListEvent :: ( Ord n, Monoid t, Eq t
                         , ZG.GenericTextZipper t
                         , B.DecodeUtf8 t
                         )
                      => V.Event
                      -> FilterList n t e
                      -> B.EventM n (FilterList n t e)
handleFilterListEvent evt fl =
  case evt of
    V.EvKey (V.KChar '\t') [] ->
      -- Tab completion
      case V.length (matchingItems fl L.^. B.listElementsL) == 1 of
        False -> return fl
        True -> do
          let cmpTxt = toText fl ((matchingItems fl L.^. B.listElementsL) V.! 0)
          let newZipper = Z.gotoEOL (ZG.textZipper [cmpTxt] Nothing)
          return fl { editor = B.applyEdit (const newZipper) (editor fl) }
    V.EvKey (V.KChar 'n') [V.MCtrl] ->
      return fl { matchingItems = B.listMoveDown (matchingItems fl) }
    V.EvKey V.KDown [] ->
      return fl { matchingItems = B.listMoveDown (matchingItems fl) }
    V.EvKey (V.KChar 'p') [V.MCtrl] ->
      return fl { matchingItems = B.listMoveUp (matchingItems fl) }
    V.EvKey V.KUp [] ->
      return fl { matchingItems = B.listMoveUp (matchingItems fl) }
    _ -> do
      editor' <- B.handleEditorEvent evt (editor fl)
      return fl { editor = editor' }

-- | Return the selected list item (if any)
selectedItem :: FilterList n t e -> Maybe e
selectedItem fl = do
  idx <- matchingItems fl L.^. B.listSelectedL
  guard (idx < V.length (matchingItems fl L.^. B.listElementsL))
  return ((matchingItems fl L.^. B.listElementsL ) V.! idx)

-- | Extract the current contents of the editor widget
editorContents :: (Monoid t) => FilterList n t e -> t
editorContents fl = mconcat (B.getEditContents (editor fl))

clearEditor :: (ZG.GenericTextZipper t) => B.Editor t n -> B.Editor t n
clearEditor = B.applyEdit (const (ZG.textZipper [] Nothing))

-- | Apply an update function to the current list
updateList :: (V.Vector e -> V.Vector e) -> FilterList n t e -> FilterList n t e
updateList f fl =
  fl { matchingItems = B.list (matchingItems fl L.^. B.listNameL) (f (allItems fl)) 1 }

-- | The current contents of the list weidget
listContents :: FilterList n t e -> V.Vector e
listContents fl = matchingItems fl L.^. B.listElementsL

-- | Clear the editor widget and reset the list to its default state
resetList :: (ZG.GenericTextZipper t) => FilterList n t e -> FilterList n t e
resetList fl =
  FilterList { editor = clearEditor (editor fl)
             , matchingItems = B.list (matchingItems fl L.^. B.listNameL) (allItems fl) 1
             , allItems = allItems fl
             , editorPosition = editorPosition fl
             , maxListHeight = maxListHeight fl
             , toText = toText fl
             , renderListItem = renderListItem fl
             , renderEditorContent = renderEditorContent fl
             }
