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
  deriving (Eq, Ord, Show)
