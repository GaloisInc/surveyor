-- | A widget for displaying a set of nodes connected by edges
module Brick.Widget.Graph (
  Graph,
  graph,
  renderGraph
  ) where

import qualified Brick as B
import qualified Brick.Widgets.List as B
import           Control.Lens ( (&), (.~) )
import qualified Data.Graph.Haggle as H
import qualified Data.Vector as V

data Graph n nl el =
  Graph { gr :: H.PatriciaTree nl el
        , graphName :: n
        , nodeHeight :: Int
        , selectedNode :: Maybe H.Vertex
        }

graph :: n -> H.PatriciaTree nl el -> Maybe H.Vertex -> Int -> Graph n nl el
graph n g sel h = Graph { gr = g, graphName = n, nodeHeight = h, selectedNode = sel }

renderGraph :: (Ord n, Show n)
            => (Bool -> nl -> B.Widget n)
            -> (el -> B.Widget n)
            -> Graph n nl el
            -> B.Widget n
renderGraph renderNode _renderEdge g =
  B.renderList renderNode True (l0 & B.listSelectedL .~ mSelIdx)
  where
    elts = V.fromList (H.labeledVertices (gr g))
    mSelIdx = do
      selNode <- selectedNode g
      V.findIndex ((== selNode) . fst) elts
    l0 = B.list (graphName g) (fmap snd elts) (nodeHeight g)
