module Surveyor.Widget.FunctionSelector (
  FunctionSelector,
  functionSelector,
  handleFunctionSelectorEvent,
  renderFunctionSelector
  ) where

import qualified Brick as B
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Text as T
import qualified Data.Text.Zipper.Generic as ZG
import qualified Data.Vector as V
import qualified Graphics.Vty as V
import           Text.Printf ( printf )

import qualified Brick.Match.Subword as SW
import qualified Brick.Widget.FilterList as FL
import qualified Surveyor.Architecture as A
import           Surveyor.Names ( Names(..) )

data FunctionSelector arch s =
  FunctionSelector { callback :: A.FunctionHandle arch s -> IO ()
                   , selector :: !(FL.FilterList Names T.Text (A.FunctionHandle arch s))
                   }

functionSelector :: (A.Architecture arch s)
                 => (A.FunctionHandle arch s -> IO ())
                 -> B.AttrName
                 -> [A.FunctionHandle arch s]
                 -> FunctionSelector arch s
functionSelector cb focAttr funcs =
  FunctionSelector { callback = cb
                   , selector = FL.filterList flcfg (V.fromList funcs)
                   }
  where
    flcfg = FL.FilterListConfig { FL.flEditorName = FunctionSelectEditor
                                , FL.flListName = FunctionSelectList
                                , FL.flEditorPosition = FL.Bottom
                                , FL.flMaxListHeight = Nothing
                                , FL.flToText = funcToText
                                , FL.flRenderListItem = renderFuncItem focAttr
                                , FL.flRenderEditorContent = renderEditorContent
                                }

funcToText :: (A.Architecture arch s) => A.FunctionHandle arch s -> T.Text
funcToText f = T.pack (printf "%s (%s)" (A.fhName f) (A.prettyAddress (A.fhAddress f)))

renderFuncItem :: (A.Architecture arch s) => B.AttrName -> Bool -> A.FunctionHandle arch s -> B.Widget Names
renderFuncItem focAttr isFocused f =
  let xfrm = if isFocused then B.withAttr focAttr else id
  in xfrm (B.txt (funcToText f))

renderEditorContent :: (Monoid t, ZG.GenericTextZipper t) => [t] -> B.Widget Names
renderEditorContent txts = B.str (ZG.toList (mconcat txts))

handleFunctionSelectorEvent :: V.Event -> FunctionSelector arch s -> B.EventM Names (FunctionSelector arch s)
handleFunctionSelectorEvent evt fsel =
  case evt of
    V.EvKey V.KEnter [] ->
      case FL.selectedItem (selector fsel) of
        Nothing -> return fsel
        Just f -> do
          liftIO (callback fsel f)
          return fsel
    _ -> do
      let updater fl =
            case SW.matcher (ZG.toList (FL.editorContents fl)) of
              Nothing -> return fl
              Just matcher -> return (FL.updateList (V.filter (matchesFunction matcher)) fl)
      fl' <- FL.handleFilterListEvent updater evt (selector fsel)
      return fsel { selector = fl' }

renderFunctionSelector :: (A.Architecture arch s) => FunctionSelector arch s -> B.Widget Names
renderFunctionSelector fsel =
  FL.renderFilterList (const B.emptyWidget) True (selector fsel)

matchesFunction :: SW.Matcher -> A.FunctionHandle arch s -> Bool
matchesFunction matcher f =
  SW.matches matcher (A.fhName f)
