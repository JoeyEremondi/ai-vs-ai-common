module AiVsAi.Graph where

import AiVsAi.GameData

mapGraph :: TileID -> GameState -> (GameState, TileID)
mapGraph start gs = (gs, start)


esp :: TileID -> TileID -> GameState -> [TileID]
esp = error "TODO implement"