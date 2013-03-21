module AStar (aStar) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Set (Set)
import Data.PSQueue (PSQ, Binding(..))
import qualified Data.PSQueue as PQ

data AStar node cost = AStar { visited   :: !(Set node),
                               openSet   :: !(PSQ node cost),
                               distance  :: !(Map node cost),
                               heuristic :: !(Map node cost),
                               ancestor  :: !(Map node node),
                               final     :: !(Maybe node) }
    deriving Show

rootState start = AStar { visited   = Set.empty,
                          openSet   = PQ.singleton start 0,
                          distance  = Map.singleton start 0, 
                          heuristic = Map.empty,
                          ancestor  = Map.empty,
                          final     = Nothing }
                          
traverse :: (Ord n, Ord c, Num c) =>
         (n -> Set n)     -- function returning neighbours of a node
         -> (n -> n -> c) -- distance function
         -> (n -> c)      -- heuristic function
         -> (n -> Bool)   -- is final?
         -> n             -- root node
         -> AStar n c     -- final state
         
traverse neighbours g h isFinal root = traverse' (rootState root) 
  where traverse' state = 
          case PQ.minView (openSet state) of
            Nothing -> state
            Just (x :-> _, openSet') ->
              case (isFinal x) of
                True  -> state { final = Just x }
                False -> traverse' $ Set.foldl' (explore x) 
                                                (state { openSet = openSet',
                                                     visited = Set.insert x (visited state)})
                                                (Set.difference (neighbours x) (visited state))
        explore parent state child =
          let dist = ((distance state) ! parent) + (g parent child) in
              case PQ.lookup child (openSet state) of
                Nothing -> newState parent child dist (state { heuristic = Map.insert child (h child) (heuristic state)})
                Just _  -> 
                    case (dist < (distance state) ! child) of
                        True  -> newState parent child dist state
                        False -> state
        newState parent child dist state = 
            state { ancestor = Map.insert child parent (ancestor state),
                    distance = Map.insert child dist (distance state),
                    openSet = PQ.insert child (dist + ((heuristic state) ! child)) (openSet state) } 
                
 
aStar :: (Ord a, Ord c, Num c) =>
         (a -> Set a)     --  Function returning neighbours of a node
         -> (a -> a -> c) --  Distance function 
         -> (a -> c)      --  Heuristic funtion
         -> (a -> Bool)   --  Function determining whether a state is final
         -> a             --  Root node
         -> Maybe [a]     --  An optimal path if exists

aStar neighbours g h isFinal root =
    let state = traverse neighbours g h isFinal root in
        case (final state) of
            Nothing -> Nothing
            Just target -> Just (backtrack target (ancestor state) [])
  where backtrack n paths acc
          | n == root = acc
          | otherwise = backtrack (paths ! n) paths (n : acc)


