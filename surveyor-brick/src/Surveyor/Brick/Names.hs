module Surveyor.Brick.Names (
  Names(..)
  ) where

import qualified Data.Text as T
import qualified Surveyor.Core as C

data Names = DiagnosticView
           | DiagnosticContent
           | FunctionList
           | BlockList
           | MinibufferEditor
           | MinibufferCompletionList
           | BlockSelectEditor
           | BlockSelectList
           | FunctionSelectEditor
           | FunctionSelectList
           | BlockViewerList Int
           | FunctionViewport
           | FunctionCFGViewer
           | InteractiveBlockViewer
           | SolverRadioSelection C.Solver
           | FloatModeRadioSelection T.Text
           | SolverInteractionFileEdit
           | SelectedBreakpointValue Int
  deriving (Eq, Ord, Show)
