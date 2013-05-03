module ResourceSimulator.Game.Gamestate
where

import Data.List (delete)  

import ResourceSimulator.Game.Resources
import ResourceSimulator.Game.Villager
import ResourceSimulator.Game.Building
import ResourceSimulator.Game.Common
import ResourceSimulator.Game.Auxiliary

data Gamestate =
  State
    { ressources :: Resources
    , villagers  :: [Villager]
    , buildings  :: [Building]
    , population :: Population
    , time       :: Time
    }

instance Show Gamestate where
  show (State (R (f,w,g,s)) vills builds _ t) = concat (map (\w -> show w ++ "\n") vills)
                                   ++ "(" ++ show t ++ "s) "
                                   ++ "[" ++ show (truncate f) ++ "F "
                                   ++        show (truncate w) ++ "W "
                                   ++        show (truncate g) ++ "G "
                                   ++        show (truncate s) ++ "S]\n"

emptyState :: Gamestate
emptyState =
  State
    { ressources = R (0, 0, 0, 0)
    , villagers  = []
    , buildings  = []
    , population = (0, 0)
    , time       = 0
    } 

actPop :: Gamestate -> Int
actPop (State _ vs _ _ _) = length vs

maxPop :: Gamestate -> Int
maxPop (State _ _ bs _ _)
  = 5 * (length $ filter (\(Building _ (US x _)) -> Support5Units `elem` x) bs)




buildBuilding :: Gamestate -> Building -> Villager -> Maybe (Gamestate, Gamestate)
buildBuilding gs b@(Building (DM _ _ _ (bt, btb)) _) v
  | v `elem` (villagers gs)
  = Just
    ( gs { ressources = gsRes
         , time = (time gs) + 1
         , villagers = builder:delete v (villagers gs)
         }
    , transform 
        gs { ressources = resSum (map (\x -> (gathered x) bt) $ villagers gs) gsRes
           , buildings  = b : (buildings gs)
           , time = (time gs) + (bt - btb)
           }
        builder
        idle
    ) 
  | otherwise = Nothing
  where gsRes = ressources gs - (cost.bldgMeta $ b)


buildVillager :: Gamestate -> Villager -> (Gamestate, Gamestate)
buildVillager gs w@(MkVillager (DM _ _ _ (bt, btb)) _)
  = ( gs { ressources = gsRes
         , time = (time gs) + 1
         }
    , gs { ressources = resSum (map (\x -> (gathered x) bt) $ villagers gs) gsRes
         , villagers = w : (villagers gs)
         , time = (time gs) + (bt - btb)
         }
    ) 
  where gsRes = ressources gs - (cost.villMeta $ w)


transform :: Gamestate -> Villager -> Villager -> Gamestate
transform gs fr to 
  = gs { villagers = switchOne fr (villagers gs) }
  where vs = villagers gs
        switchOne _ [] = []
        switchOne x (v:vs)
          | (name.villMeta $ x) == (name.villMeta $ v) = to:vs
          | otherwise        =  v:(switchOne x vs)
        
simulateFor :: Gamestate -> Time -> Gamestate
simulateFor gs t
  = gs { ressources = resSum (map (\x -> (gathered x) t) $ villagers gs) $ ressources gs
       , time = time gs + t }

