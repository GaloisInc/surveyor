module Surveyor.Widget.Minibuffer (
  Minibuffer,
  minibuffer,
  handleMinibufferEvent,
  renderMinibuffer
  ) where

import qualified Brick as B
import qualified Brick.Widgets.Edit as B

data Minibuffer t n =
  Minibuffer { editor :: B.Editor t n
             }

minibuffer = undefined
handleMinibufferEvent = undefined
renderMinibuffer = undefined
