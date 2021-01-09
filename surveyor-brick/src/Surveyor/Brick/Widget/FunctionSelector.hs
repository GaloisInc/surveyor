{-# LANGUAGE MultiWayIf #-}
module Surveyor.Brick.Widget.FunctionSelector (
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
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PPT

import qualified Brick.Match.Subword as SW
import qualified Brick.Widget.FilterList as FL
import qualified Surveyor.Core as C
import           Surveyor.Brick.Names ( Names(..) )

data FunctionSelector arch s =
  FunctionSelector { callback :: C.FunctionHandle arch s -> IO ()
                   , selector :: !(FL.FilterList Names T.Text (C.FunctionHandle arch s))
                   }

functionSelector :: (C.Architecture arch s)
                 => (C.FunctionHandle arch s -> IO ())
                 -> B.AttrName
                 -> [C.FunctionHandle arch s]
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

docText :: PP.Doc ann -> T.Text
docText = PPT.renderStrict . PP.layoutCompact

funcToText :: (C.Architecture arch s) => C.FunctionHandle arch s -> T.Text
funcToText f =
  docText (PP.pretty (C.fhName f) PP.<+> PP.parens (C.prettyAddress (C.fhAddress f)))

renderFuncItem :: (C.Architecture arch s) => B.AttrName -> Bool -> C.FunctionHandle arch s -> B.Widget Names
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
      let edContents0 = FL.editorContents (selector fsel)
      fl' <- FL.handleFilterListEvent evt (selector fsel)
      if | edContents0 == FL.editorContents fl' -> return fsel { selector = fl' }
         | otherwise -> do
             fl'' <- case SW.matcher (ZG.toList (FL.editorContents fl')) of
                       Nothing -> return fl'
                       Just matcher -> return (FL.updateList (V.filter (matchesFunction matcher)) fl')
             return fsel { selector = fl'' }

renderFunctionSelector :: (C.Architecture arch s) => FunctionSelector arch s -> B.Widget Names
renderFunctionSelector fsel =
  FL.renderFilterList (const B.emptyWidget) True (selector fsel)

matchesFunction :: SW.Matcher -> C.FunctionHandle arch s -> Bool
matchesFunction matcher f =
  SW.matches matcher (C.fhName f)
