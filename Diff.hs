module Diff where
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

diffMaps :: (Eq a, Ord k) => Map k a -> Map k a -> (Set k, Map k a)
diffMaps t1 t2 = (deletes, inserts)
  where deletes = Map.keysSet (Map.difference t1 t2)
        inserts = Map.difference t2 t1 <>
                  Map.mapMaybe id (Map.intersectionWith combine t1 t2)
        combine x y | x == y    = Nothing
                    | otherwise = Just y

diffSets :: Ord a => Set a -> Set a -> (Set a, Set a)
diffSets t1 t2 = (deletes, inserts)
  where deletes = Set.difference t1 t2
        inserts = Set.difference t2 t1

diffUnorderedLists :: Ord a => [a] -> [a] -> ([a], [a])
diffUnorderedLists t1 t2 = (Set.toList deletes, Set.toList inserts)
  where (deletes, inserts) = diffSets (Set.fromList t1) (Set.fromList t2)

patchMap :: Ord k => Map k a -> (Set k, Map k a) -> Map k a
patchMap t (deletes, inserts) =
  inserts <> Map.difference t (setToMap () deletes)

patchSet :: Ord a => Set a -> (Set a, Set a) -> Set a
patchSet t (deletes, inserts) =
  inserts <> Set.difference t deletes

setToMap :: Ord k => a -> Set k -> Map k a
setToMap x t = Map.fromList [(k, x) | k <- Set.toList t]
