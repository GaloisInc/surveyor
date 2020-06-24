{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Surveyor.Brick.Widget.ValueSelector (
  ValueSelector,
  ValueSelectorForm,
  valueSelectorForm,
  selectedIndex,
  selectedValue,
  renderValueSelectorForm,
  handleValueSelectorFormEvent
  ) where

import qualified Brick as B
import           Brick.Forms ( (@@=) )
import qualified Brick.Forms as B
import           Control.Lens ( (^.) )
import qualified Control.Lens as L
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Lang.Crucible.Simulator.RegMap as LMCR

import qualified Surveyor.Brick.Names as SBN

-- | This is a type capturing the data necessary to render the value selector form
--
-- We have to carefully quantify type parameters so that we can mesh well with brick.
data ValueSelector sym ctx =
  ValueSelector { _values :: Ctx.Assignment (LMCR.RegEntry sym) ctx
                , _index :: Some (Ctx.Index ctx)
                }

L.makeLenses ''ValueSelector

selectedIndex :: ValueSelectorForm s sym ctx e -> Maybe (Some (Ctx.Index ctx))
selectedIndex vs =
  case vs of
    NoValues -> Nothing
    ValueSelectorForm f -> Just (f ^. L.to B.formState . index)

selectedValue :: ValueSelectorForm s sym ctx e -> Maybe (Some (LMCR.RegEntry sym))
selectedValue vs =
  case vs of
    NoValues -> Nothing
    ValueSelectorForm f ->
      case f ^. L.to B.formState . index of
        Some idx -> Just (Some ((f ^. L.to B.formState . values) Ctx.! idx))

-- | This wrapper quantifies out the ctx parameter from the form so that we can
-- return it
data ValueSelectorForm s sym ctx e where
  -- | A degenerate constructor in the case where the breakpoint captured no
  -- values.  We need this, as we cannot construct a value of type
  -- 'ValueSelector' if there are no entries in the list (since we would not be
  -- able to construct an 'Ctx.Index' into the empty 'Ctx.Assignment')
  NoValues :: ValueSelectorForm s sym Ctx.EmptyCtx e
  -- | A form that tracks the state of the currently selected value.  The data
  -- payload of the form is the 'ValueSelector', which tells us the currently
  -- selected index (which is a valid index into the list of 'WrappedViewer's in
  -- the 'StateExplorer')
  --
  -- We carry a proof that the context is not empty
  ValueSelectorForm :: (ctx ~ (ctx0 Ctx.::> tp)) => B.Form (ValueSelector sym ctx) e SBN.Names -> ValueSelectorForm s sym ctx e

valueSelectorForm :: forall s sym ctx e
                   . Ctx.Assignment (LMCR.RegEntry sym) ctx
                  -> ValueSelectorForm s sym ctx e
valueSelectorForm entries =
  case entries of
    Ctx.Empty -> NoValues
    vals@(_ Ctx.:> _) ->
      let idxList = reverse $ Ctx.forIndex (Ctx.size vals) (\acc idx -> idxEntry idx : acc) []
          vs = ValueSelector { _values = vals, _index = head idxList ^. L._1 }
          formCon = B.newForm [ (B.str "Value: " B.<+>) @@= B.radioField index idxList
                              ]
      in ValueSelectorForm (formCon vs)
  where
    idxEntry :: forall tp . Ctx.Index ctx tp -> (Some (Ctx.Index ctx), SBN.Names, T.Text)
    idxEntry idx = (Some idx, SBN.SelectedBreakpointValue (Ctx.indexVal idx), T.pack (show idx))

renderValueSelectorForm :: ValueSelectorForm s sym ctx e -> B.Widget SBN.Names
renderValueSelectorForm vsf =
  case vsf of
    NoValues -> B.txt "No values in frame"
    ValueSelectorForm f -> B.renderForm f

handleValueSelectorFormEvent :: B.BrickEvent SBN.Names e
                             -> ValueSelectorForm s sym ctx e
                             -> B.EventM SBN.Names (ValueSelectorForm s sym ctx e)
handleValueSelectorFormEvent evt vsf =
  case vsf of
    NoValues -> return NoValues
    ValueSelectorForm f -> ValueSelectorForm <$> B.handleFormEvent evt f
