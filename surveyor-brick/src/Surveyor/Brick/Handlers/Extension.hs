module Surveyor.Brick.Handlers.Extension ( handleExtensionEvent ) where

import qualified Brick as B
import           Control.Lens ( (&), (.~) )
import qualified Surveyor.Core as C

import qualified Surveyor.Brick.Extension as SBE

handleExtensionEvent :: (C.Architecture arch s)
                     => C.S SBE.BrickUIExtension SBE.BrickUIState arch s
                     -> SBE.BrickUIEvent s st
                     -> B.EventM n (B.Next (C.State SBE.BrickUIExtension SBE.BrickUIState s))
handleExtensionEvent s0 evt =
  case evt of
    SBE.ShowSummary -> B.continue $! C.State (s0 & C.lUIMode .~ C.SomeUIMode C.Summary)
    SBE.ShowDiagnostics -> B.continue $! C.State (s0 & C.lUIMode .~ C.SomeUIMode C.Diags)
    SBE.OpenMinibuffer ->
      case C.sUIMode s0 of
        C.SomeMiniBuffer _ -> B.continue (C.State s0)
        C.SomeUIMode mode -> B.continue $! C.State (s0 & C.lUIMode .~ C.SomeMiniBuffer (C.MiniBuffer mode))
