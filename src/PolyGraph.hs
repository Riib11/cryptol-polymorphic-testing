module PolyGraph where 

-- import qualified Data.Set as Set
-- import M

-- data PolyGraph v e = PolyGraph
--   { vertices :: Set.Set v
--   , edges :: [Edge v e] }
--   deriving (Show)

-- type Edge v e = (Set.Set v, e)

-- instance Ord v => Semigroup (PolyGraph v e) where 
--   g1 <> g2 = flip (foldr addVertex) (vertices g2) -- merge vertices
--            . flip (foldr addEdge)   (edges g2)    -- merge edges
--            $ g1

-- instance Ord v => Monoid (PolyGraph v e) where 
--   mempty = PolyGraph { vertices = mempty, edges = mempty }

-- instance Functor (PolyGraph v) where
--   fmap f g = g { edges = (\(vs, e) -> (vs, f e)) <$> edges g }

-- addVertex :: Ord v => v -> PolyGraph v e -> PolyGraph v e
-- addVertex v g = g { vertices = Set.insert v (vertices g)  }

-- addEdge :: Ord v => Edge v e -> PolyGraph v e -> PolyGraph v e
-- addEdge e@(vs, _) g = g { edges = e : edges g, vertices = vs <> vertices g }

-- liftOutEdge :: PolyGraph v (M e) -> M (PolyGraph v e)
-- liftOutEdge g = do
--   edges <- mapM (\(vs, me) -> me >>= \e -> pure (vs, e)) $ edges g
--   pure $ g { edges = edges }
