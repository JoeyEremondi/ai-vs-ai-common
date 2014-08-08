module AiVsAi.AIUtil where

import Prelude
import GameLib.GameData

import Control.Applicative
import GameLib.Util

--import Data.Graph.AStar

--import Game.GameState
import GameLib.UnitProperties

import Data.Map ((!), elems, keys, fromList, toList)
import Debug.Trace (trace)

import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.Graph

import Control.Applicative

import Data.Maybe (fromJust, mapMaybe )
import Data.Graph.Inductive.PatriciaTree
import Data.List (minimumBy)


import Control.Exception (assert)

endsWithGoal :: (Eq a) => a -> (Maybe [a]) -> Bool
endsWithGoal _goal Nothing = True
endsWithGoal goal (Just l) = goal == (last l)

--TODO how to know if no path?
aStarWithGoal :: TileID -> a -> GameState -> TileID -> Maybe [TileID]
aStarWithGoal goal _heuristic gs start = assert (endsWithGoal goal ret) $
    ret
    where 
        ret = maybeEmpty $ map (fromJust . lab graph)   path
        graph :: Gr TileID (TileID, TileID)
        (graph, nodeNum) = mapGraph start  gs
        path :: [Node]
        path = --trace ("Seaching with graph " ++ (show $ map (\(_,_,a)->a) $ labEdges graph) ) $
          esp (nodeNum start) (nodeNum goal) graph


validPath gs (Just (h:t)) = stepsNotEmpty t
    where
    stepsNotEmpty [] = True
    stepsNotEmpty [end] = True
    stepsNotEmpty (h : t) = (occupantAt h gs == Empty) && stepsNotEmpty t
validPath _gs _ = True

distanceToUnit :: GameState -> UnitID -> UnitID -> Maybe Int
distanceToUnit gs uid target = distanceToTile gs uid (getUnitPos gs target)

--TODO switch to line of site
euclidDistanceToUnit :: GameState -> UnitID -> UnitID -> Maybe Double
euclidDistanceToUnit gs uid target = Just $ euclidDist (getUnitPos gs uid) (getUnitPos gs target)

distanceToTile :: GameState -> UnitID -> TileID -> Maybe Int
distanceToTile gs uid tid = case path of
    Just l@(_:_) -> Just $ length l - 1
    _ ->  Nothing
    where 
        upos = getUnitPos gs uid
        path = shortestPath gs upos tid

--TODO point free?
shortestPath :: GameState ->TileID ->   TileID -> Maybe [TileID]
shortestPath gs start end = 
    --Precond
    --Postcond
    assert (validPath gs ret) $
    ret
    where ret = aStarWithGoal end (euclidDist end) gs start

--TODO how get ones that aren't enemy?
nearestEnemy :: GameState -> UnitID -> Maybe UnitID
nearestEnemy gs uid = case (nearestEnemy' (unitTeam ustate) gs (pos ustate)) of
        Nothing -> trace "No nearest enemy" $ Nothing
        Just [] -> Nothing
        Just l -> case (occupantAt (last l) gs ) of
            TileUnit enem -> Just enem
            _ -> error "Sanity check: found path to enemy that wasn't ending at enemy"
    where ustate = (gameUnits gs) ! uid

hasUnit gs tile = case (occupantAt tile gs) of
    TileUnit u -> True
    _ -> False
    
getJust [] = []
getJust (Just a : t) = a : (getJust t)
getJust (Nothing: t) = getJust t

--TODO too slow, cause look at all enemies?
nearestEnemy' :: Team -> GameState -> TileID -> Maybe [TileID]    
nearestEnemy' team gs start = assert (and $ map (hasUnit gs) posList) $
     --assert (and $ map (hasUnit . last . getJust) (mapMaybe paths posList ) ) $ 
     fmap (minimumBy lcomp) $ maybeEmpty $ mapMaybe paths posList
     where
        lcomp l1 l2
            | n1 < n2 = LT
            | n1 == n2 = EQ
            | otherwise = GT
            where (n1, n2) = (length l1, length l2)
        --tileHasEnemy (TileUnit u) = uidIsEnemy u
        --tileHasEnemy _ = False
        uidIsEnemy uid = (getUnitTeam gs uid) /= team
        --isEnemy u =  (unitTeam u) /= team
        --heuristic :: TileID -> Double
        heuristic a = 0 --minimum  $   ((map ((euclidDist a) . pos)) . (filter isEnemy)) (elems $ gameUnits gs)
        --goal tid = tileHasEnemy $ occupantAt tid gs
        enemies = filter (uidIsEnemy) ((keys . gameUnits) gs) 
        posList = map (getUnitPos gs) enemies
        paths end =  aStarWithGoal end heuristic gs start

graphDistanceTo :: GameState -> TileID -> TileID -> Maybe Int            
graphDistanceTo gs start end = length <$> (shortestPath gs start end)

{-
--TODO change to graph distance
enemyInRange :: GameState -> UnitID -> UnitID -> Bool
enemyInRange gs uid target =  (euclidDist upos tpos) <= movementRange utype
    where 
        ustate = (gameUnits gs) ! uid
        utype = unitType ustate
        targetState = (gameUnits gs) ! target
        upos = pos ustate
        tpos = pos targetState
-}


tileIsEmpty gs  tile = case occupantAt tile gs of
  Empty -> True
  _ -> False 

mapGraph :: TileID -> GameState -> (Gr TileID (TileID, TileID), TileID -> Node)
mapGraph start gs = (graph, nodeNum)
    where
        graph ::Gr TileID (TileID, TileID)
        graph = mkGraph nodes edges
        numNodes = length $ (keys . gameTiles) gs
        edgeDict = fromList $ zip ( (keys . gameTiles) gs) [1 .. numNodes ]
        nodeNum tid =  edgeDict ! tid
        allNeighbours :: TileID -> [TileID]
        allNeighbours (x,y) = [(x+1,y), (x-1,y), (x+1,y-1), (x,y-1), (x-1,y-1), (x+1,y+1), (x,y+1), (x-1,y+1) ]
        tileInRange :: TileID -> Bool
        tileInRange (x,y) = x > 0 && x <= (gameMapWidth gs) && y <= (gameMapHeight gs) && y > 0
        edgesLeaving :: TileID -> [LEdge (TileID, TileID)]
        edgesLeaving v = if (v == start) then map (toEdge v) $ filter tileInRange  $ filter (tileIsEmpty gs) $ allNeighbours v --TODO code reuse
            else 
                case (occupantAt v gs) of
                    Empty ->
                      --trace ("Noting that node " ++ (show v) ++ " is empty in graph") $ 
                      map (toEdge v) $ filter tileInRange  $ allNeighbours v
                    _ -> []
        toEdge :: TileID -> TileID -> LEdge (TileID, TileID)
        toEdge v x = (nodeNum v,  nodeNum x, (v,x)) 
        nodes :: [LNode TileID]
        nodes = map (\(x,y) -> (y,x)) $ toList edgeDict
        toNode:: TileID -> LNode TileID
        toNode v = (nodeNum v, v)
        edges :: [LEdge (TileID, TileID)]
        edges = concat [edgesLeaving v | v <- keys edgeDict]