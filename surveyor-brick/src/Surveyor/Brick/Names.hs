module Surveyor.Brick.Names (
  Names(..)
  ) where

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
  deriving (Eq, Ord, Show)
