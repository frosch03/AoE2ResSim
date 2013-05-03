module ResourceSimulator.Game.Building
where

import ResourceSimulator.Game.Resources
import ResourceSimulator.Game.Common
import ResourceSimulator.Game.Default


data Upgrade
  = Support5Units
  | Loom
  | Handcart1
  | Handcart2
  deriving (Eq)


data UpgradeStatus =
  US { doneUps     :: [Upgrade]
     , possibleUps :: [( Age, Upgrade )]
     }

data Building =
  Building { bldgMeta :: DefMeta
           , upgrades :: UpgradeStatus
           }

house      = Building (DM "House" (onWood 30) 900 (25, 0)) (US [Support5Units] [])
towncenter = Building (DM "Towncenter" (onWood 275 + onStone 100) 1000 (300, 0))
                      (US [Support5Units]
                          [ ( Dark, Loom )
                          , ( Dark, Handcart1 )
                          , ( Feudal, Handcart2 )
                          ]
                      )


