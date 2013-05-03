module ResourceSimulator.Game
where

import Control.Monad.State
import System.IO

import ResourceSimulator.Game.Gamestate
import ResourceSimulator.Game.Resources
import ResourceSimulator.Game.Villager
import ResourceSimulator.Game.Building
import ResourceSimulator.Game.Common
import ResourceSimulator.Game.Default
import ResourceSimulator.Game.Auxiliary



type Game = StateT [Gamestate] IO

create :: Villager -> Game ()
create v =
  do g@(game:_)         <- get
     -- () `either` () $ buildVillager game v
     let (startB, endB) = (buildVillager game v)
     put (endB:startB:g)

into :: Villager -> Villager -> Game ()
into f t =
  do g@(game:_) <- get
     put $ (transform game f t:g)

wait :: Int -> Game ()
wait n = 
  do g@(game:_) <- get
     put $ (game `simulateFor` n:g)

toBuild :: Villager -> Building -> Game ()
toBuild v b = 
  do g@(game:_) <- get
     return () `maybe` (\(s,e) -> put (e:s:g)) $ buildBuilding game b v

standardGame =
  State
    { ressources = R (200, 200, 100, 0)
    , villagers  = replicate 3 idle
    , buildings  = [towncenter]
    , population = (3, 5)
    , time       = 0
    } 
