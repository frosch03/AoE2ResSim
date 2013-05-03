{-# LANGUAGE EmptyDataDecls, FlexibleInstances, ExistentialQuantification #-}
module Test
where

data TownCenter =
  MkTC { age          :: Age
       , options      :: [Option]
       , possibleOpts :: [( Age, Option )]
       }

type Food  = Int
type Wood  = Int
type Gold  = Int
type Stone = Int

type HP = Int

type FiftyFoodPer  = Int
type FiftyWoodPer  = Int
type FiftyGoldPer  = Int
type FiftyStonePer = Int

type Time = Int


type Age = Bool
type Option = Bool


class Gatherable a

data F = F deriving (Show)
instance Gatherable F

data W = W deriving (Show)
instance Gatherable W

data G = G deriving (Show)
instance Gatherable G

data S = S deriving (Show)
instance Gatherable S

data X = X deriving (Show)
instance Gatherable X



data Worker =
  forall a . Gatherable a => 
  MkWorker
    { cost      :: (Food, Wood, Gold, Stone)
    , hp        :: HP                            -- HitPoints
    , ressource :: a
    , rate      :: (FiftyFoodPer, Time)          -- ((50 food / x sec),  BonusTime)
    , buildtime :: (Time, Time)                  -- (Original Buildtime, BonusTime)
    }


instance Show Worker where
  show w = showVillager w

showVillager :: Worker -> String
showVillager (MkWorker _ hp F (150, gb) (_, btb)) = "Shepard(" ++ show hp ++ ") " ++ (showBonus gb btb)
showVillager (MkWorker _ hp F (165, gb) (_, btb)) = "Berrypicker(" ++ show hp ++ ") " ++ (showBonus gb btb)
showVillager (MkWorker _ hp W (130, gb) (_, btb)) = "Lumberjack(" ++ show hp ++ ") " ++ (showBonus gb btb)
showVillager (MkWorker _ hp F ( 85, gb) (_, btb)) = "Deerhunter(" ++ show hp ++ ") " ++ (showBonus gb btb)
showVillager (MkWorker _ hp F (140, gb) (_, btb)) = "Boarhunter(" ++ show hp ++ ") " ++ (showBonus gb btb)
showVillager (MkWorker _ hp F (171, gb) (_, btb)) = "Farmer(" ++ show hp ++ ") " ++ (showBonus gb btb)
showVillager (MkWorker _ hp S (135, gb) (_, btb)) = "Stoneminer(" ++ show hp ++ ") " ++ (showBonus gb btb)
showVillager (MkWorker _ hp G (135, gb) (_, btb)) = "Goldminer(" ++ show hp ++ ") " ++ (showBonus gb btb)
showVillager (MkWorker _ hp X (  0, gb) (_, btb)) = "IdleWorker(" ++ show hp ++ ") " ++ (showBonus gb btb)

showBonus 0  btb = "[build +" ++ show btb ++ "sec]"
showBonus gb 0   = "[gather +" ++ show gb ++ "sec]"
showBonus gb btb = "[gather +" ++ show gb ++ "sec, build +" ++ show btb ++ "sec]"


vilCost = (50, 0, 0, 0)
vilHP   = 25
vilBT   = 25

villager :: (Gatherable a) => a -> FiftyFoodPer -> Worker
villager r t = MkWorker vilCost vilHP r (t, 0) (vilBT, 0) 

shepard     = villager F 150
berrypicker = villager F 165
lumberjack  = villager W 130
deerhunter  = villager F  85
boarhunter  = villager F 140
farmer      = villager F 171
stoneminer  = villager S 135
goldminer   = villager G 135
idle        = villager X   0

data Gamestate =
  State
    { ressources :: (Food, Wood, Gold, Stone)
    , villagers  :: [Worker]
    , time       :: Time
    }

standardState =
  State
    { ressources = (200, 200, 100, 0)
    , villagers  = replicate 3 idle
    , time       = 0
    } 

buildWorker :: Gamestate -> Worker -> Gamestate
buildWorker gs w
  = error "asdf"
