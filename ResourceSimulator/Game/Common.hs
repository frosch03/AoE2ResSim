module ResourceSimulator.Game.Common
where

import Data.List (elem)

import ResourceSimulator.Game.Resources


type HP = Int  

type FiftyResPer  = Float

type Time = Int


type Rate = (FiftyResPer, Time)

type Actual     = Int
type Maximal    = Int
type Population = (Actual, Maximal)


data DefMeta =
  DM { name      :: String
     , cost      :: Resources
     , hp        :: HP
     , buildtime :: (Time, Time)
     }
  deriving (Eq)

data Age
     = Dark
     | Feudal
     | Castle
     | Imperial


onFood, onWood, onGold, onStone :: Float -> Resources
onFood  f = R (f, 0, 0, 0)
onWood  w = R (0, w, 0, 0)
onGold  g = R (0, 0, g, 0)
onStone s = R (0, 0, 0, s)


gather :: (Float -> Resources)-> Rate -> Time -> Resources
gather ins (r, rbt) n
  = ins $ 50.0 / r * fromIntegral (n - rbt)

stdRate :: Int -> Rate
stdRate r = (fromIntegral r, 0)

