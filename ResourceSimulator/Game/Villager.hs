module ResourceSimulator.Game.Villager
where

import ResourceSimulator.Game.Resources
import ResourceSimulator.Game.Common
import ResourceSimulator.Game.Default


data Villager
  = MkVillager
    { villMeta  :: DefMeta
    , gathered  :: Time -> Resources
    }


instance Eq Villager where
  (MkVillager metaV1 _) == (MkVillager metaV2 _) = metaV1 == metaV2


instance Show Villager where
  show (MkVillager (DM s _ hp rt) _) = s ++ "(" ++ show hp ++ ")"


villager :: String -> (Float -> Resources) -> Rate -> Villager
villager s ins r = MkVillager (DM s vilCost vilHP (vilBT, 0)) (gather ins r)
 
shepard     = villager "Shepard"     onFood  $ stdRate 150
berrypicker = villager "Berrypicker" onFood  $ stdRate 165
lumberjack  = villager "Lumberjack"  onWood  $ stdRate 130
deerhunter  = villager "Deerhunter"  onFood  $ stdRate  85
boarhunter  = villager "Boarhunter"  onFood  $ stdRate 140
farmer      = villager "Farmer"      onFood  $ stdRate 171
stoneminer  = villager "Stoneminer"  onStone $ stdRate 135
goldminer   = villager "Goldminer"   onGold  $ stdRate 135
idle        = villager "Chiller"     onID    $ stdRate   0
  where onID = (\_ -> R (0.0, 0.0, 0.0, 0.0))
builder     = villager "Builder"     onID    $ stdRate   0
  where onID = (\_ -> R (0.0, 0.0, 0.0, 0.0))


