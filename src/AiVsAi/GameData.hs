module AiVsAi.GameData where

import Prelude

import qualified Data.Map 
--import qualified Data.Map as Map
--import Language.Haskell.Pretty 



--import Data.Typeable.Internal (Typeable)

--import Data.Data (Data)


data Action =
    Move TileID
    |Fire UnitID
    |Pass 
    |ChaseOrFireAt UnitID
    |MoveTowards TileID
    |Retreat
    deriving (Eq, Show, Read)
    
type ActionChoice = UnitState -> [Action] -> GameState -> Action

type Strategy = UnitType -> ActionChoice




--copy of error function, used in liquidHaskell to make sure never called
--notImplemented = error

--data for the different teams
data Team = TeamF | TeamC
    deriving (Show, Read, Eq, Ord)

--Data for different types of games
data GameType = Deathmatch | Endurance Int
    deriving (Eq, Show, Read, Ord)

data Result = FWin | CWin | Draw
    deriving (Eq, Show, Read, Ord)


type TileID = (Int, Int)


data Tile = Tile {
    occupant :: TileOccupant,
    xcoord :: Int,
    ycoord :: Int{-,
    left :: Maybe TileID,
    right :: Maybe TileID,
    top :: Maybe TileID,
    bottom :: Maybe TileID,
    topleft :: Maybe TileID,
    topright :: Maybe TileID,
    bottomleft :: Maybe TileID,
    bottomright :: Maybe TileID-}
} deriving (Show, Read)




--Stores if this tile is empty, filled with unit, or filled with an obstacle
data TileOccupant = 
    TileUnit UnitID
    | TileEnv EnvUnit 
    | Empty
    deriving (Eq, Show, Read)




--TODO change to have actual type
data EnvUnit = Tree | Window
    deriving (Show, Read, Eq)



--Each unit has a unique ID number
type UnitID = Int
--    deriving (Eq, Show, Read, Ord)


type TileMap = Data.Map.Map TileID Tile
type UnitMap = Data.Map.Map UnitID UnitState


data UnitState = UnitState {
    idNum :: UnitID,
    pos :: (Int, Int),
    unitType :: UnitType,
    unitTeam :: Team,
    isAlive :: Bool,
    hp :: Int,
    lastFireTurn :: Int
} deriving (Show, Read)


data UnitType = Scout | Tank
    deriving (Show, Read, Eq, Ord)

    
--The game state is the state of each tile
data GameState = GameState 
    {
    gameTiles :: Data.Map.Map TileID Tile,
    gameUnits :: Data.Map.Map UnitID UnitState,
    turn :: Int, --TODO Integer? Shouldn't need really big numbers
    gameMapWidth :: Int,
    gameMapHeight :: Int,
    gameType :: GameType,
    unitQueue :: [(Team, UnitID)]
    }  deriving (Show, Read)


defaultGameState = GameState 
  {gameTiles = Data.Map.empty,
   gameUnits = Data.Map.empty,
   turn = 0,
   gameMapWidth = 0,
   gameMapHeight = 0,
   gameType = Deathmatch,
   unitQueue = [] }  


-- | Used to avoid !
justElem :: Ord a => Data.Map.Map a b -> a -> b
justElem dict key = case (Data.Map.lookup key dict) of
  (Just e) -> e
  _ -> error $ "Key not in map"    
    
    
occupantAt :: TileID -> GameState -> TileOccupant
occupantAt loc gs = occupant tile
    where 
        tiles = gameTiles gs
        tile = justElem tiles loc

tileEmpty :: TileID -> GameState -> Bool
tileEmpty tid gs = (occupantAt tid gs) == Empty

unitByID :: UnitID -> GameState -> UnitState
unitByID u gs=  justElem (gameUnits gs)  u    
    
    
getUnitState gs uid = justElem (gameUnits gs) uid


--easy getters and setters
getUnitPos gs = pos . getUnitState gs
getUnitTeam gs = unitTeam . getUnitState gs
getUnitType gs = unitType . getUnitState   gs 
getUnitHealth gs = hp . getUnitState   gs    


--TODO better place?
type GameTime = Float
