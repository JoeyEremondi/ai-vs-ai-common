module AiVsAi.UnitProperties where

import Prelude
import GameLib.GameData

--TODO implement this as json?

movementRange :: UnitType -> Int
movementRange Scout = 2
movementRange Tank = 1

attackRange :: UnitType -> Double
attackRange Scout = 1
attackRange Tank = 2

maxHealth :: UnitType -> Int
maxHealth Scout = 10
maxHealth Tank = 20

baseAttack :: UnitType -> Int
baseAttack Scout = 1
baseAttack Tank = 2

fireRechargeTime :: UnitType -> Int
fireRechargeTime Scout = 4
fireRechargeTime Tank = 2

--Initialize a new unit with default starting values
newUnit :: UnitID -> TileID -> UnitType -> Team -> UnitState
newUnit uid loc utype team = UnitState 
    {
    idNum = uid,
    pos = loc,
    unitType = utype,
    unitTeam = team,
    isAlive = True,
    hp = maxHealth utype,
    lastFireTurn = 0
    }
    

--TOOD implement rock-paper-scissors weaknesses?