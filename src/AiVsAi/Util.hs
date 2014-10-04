module AiVsAi.Util where

import Prelude

import Control.Monad.State

--Take from the end of a list
takeEnd :: Int -> [a] -> [a]
takeEnd n l = reverse $ take n $ reverse l

--Zip, but store the excess in a pair
zipWithLeftover :: [a] -> [b] -> ([(a,b)], [a], [b])
zipWithLeftover la lb = (zipList, fst leftover, snd leftover)
    where
        zipList = zip la lb
        leftover 
            | (length la) == (length lb) = ([], [])
            | (length la) >  (length lb) =  (takeEnd (length la - length lb) la, [])
            | otherwise =  ([], takeEnd (length lb - length la) lb)
            
toState :: a -> State a ()
toState a = do
    put a
    return ()
    
euclidDist :: (Int, Int) -> (Int, Int) -> Double
euclidDist (xi,yi) (xxi,yyi) = sqrt $ (x-xx)^2 + (y - yy)^2
    where 
        x = fromIntegral xi
        y = fromIntegral yi
        xx = fromIntegral xxi
        yy = fromIntegral yyi
        
concatPairs :: [(a,a)] -> [a]
concatPairs [] = []
concatPairs ((a,b): t) = [a,b] ++ concatPairs t

data InfInt = Infinity | Finite Int
    deriving (Eq, Show)
    
instance Ord InfInt where
    Infinity <= Infinity = True
    Finite _ <= Infinity = True
    Infinity <= Finite _  = False
    Finite a <= Finite b = a <= b

maybeEmpty :: [a] -> Maybe [a]    
maybeEmpty [] = Nothing
maybeEmpty l = Just l