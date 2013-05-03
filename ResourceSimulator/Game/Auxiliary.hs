module ResourceSimulator.Game.Auxiliary
where

import Data.List (nub, elem, delete)  

import ResourceSimulator.Game.Common
import ResourceSimulator.Game.Villager

interpole :: (Time, Float) -> (Time, Float) -> [(Time, Float)]
interpole (t2, v2) (t1, v1)
  = map (\x -> (x, v2 + spd x)) $ enumFromThenTo t2 (t2-1) t1
  where spd x = (dv/fI dt) * fI (x-t1-dt)
        dt    = t2 - t1 :: Int
        dv    = v2 - v1
        fI    = fromIntegral


interpole2 :: [(Time, Float)] -> [(Time, Float)]
interpole2 z@(_:z') = concat $ zipWith interpole z z'

stepInterpole :: [(Time, Float)] -> [(Time, Float)]
stepInterpole z@(_:z') = concat $ zipWith (\(t1,v1) (t2,v2) -> (t1,v1):interpole (t1,v2) (t2,v2)) z z'




