module Graph where 

import qualified Data.Set as Set

-- data Graph v e = Graph
--   { vertices :: Set.Set v
--   , edges :: [Edge v e] } 
--   deriving (Show)

-- type Edge v e = (v, v, e)

-- source :: Edge v e -> v
-- source (v, _, _) = v

-- target :: Edge v e -> v
-- target (_, v, _) = v 

-- edge :: Edge v e -> e
-- edge (_, _, e) = e

-- instance Ord v => Semigroup (Graph v e) where 
--   g1 <> g2 = flip (foldr addVertex) (vertices g2) -- merge vertices
--            . flip (foldr addEdge)   (edges g2)    -- merge edges
--            $ g1

-- instance Ord v => Monoid (Graph v e) where 
--   mempty = Graph { vertices = mempty, edges = mempty }

-- instance Functor (Graph v) where
--   fmap f g = g { edges = (\(v1, v2, e) -> (v1, v2, f e)) <$> edges g }

-- -- instance Foldable (Graph v) where 
-- --   foldr f b g = foldr f b (edge <$> edges g)

-- -- instance Traversable (Graph v) where 
-- --   traverse k g = fmap (\es -> g { edges = zipWith (\b (v1, v2, a) -> (v1, v2, _)) es (edges g) }) (traverse k (edge <$> edges g))

-- addVertex :: Ord v => v -> Graph v e -> Graph v e
-- addVertex v g = g { vertices = Set.insert v (vertices g)  }

-- addEdge :: Ord v => Edge v e -> Graph v e -> Graph v e
-- addEdge e@(v1, v2, _) g = g { edges = e : edges g, vertices = Set.insert v1 $ Set.insert v2 $ vertices g }

-- -- all edges of the form x -{e}-> y
-- edgesFrom :: Ord v => v -> Graph v e -> [Edge v e]
-- edgesFrom x = filter (\(x', _, _) -> x' == x') . edges

-- -- all edges of the form x -{e}-> y
-- edgesTo :: Ord v => v -> Graph v e -> [Edge v e]
-- edgesTo y = filter (\(_, y', _) -> y == y') . edges

-- -- liftOutEdge :: Graph v (M e) -> M (Graph v e)
-- -- liftOutEdge g = do
-- --   edges <- mapM (\(v1, v2, me) -> me >>= \e -> pure (v1, v2, e)) $ edges g
-- --   pure $ g { edges = edges }
