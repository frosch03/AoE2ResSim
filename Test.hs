{-# LANGUAGE EmptyDataDecls, FlexibleInstances, ExistentialQuantification #-}
module Test
where

import Control.Monad.State
import System.IO

import Data.List (nub, elem, delete)

-- Chart
import Graphics.Rendering.Chart 
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import Data.Colour 
import Data.Colour.Names hiding (gold)

--


type Food  = Float
type Wood  = Float
type Gold  = Float
type Stone = Float

type FiftyResPer  = Float

type Time = Int
data Resources = R (Food, Wood, Gold, Stone) deriving (Eq, Show)
type HP = Int
type Rate = (FiftyResPer, Time)

type Actual     = Int
type Maximal    = Int
type Population = (Actual, Maximal)

data Upgrade
  = Support5Units
  | Loom
  | Handcart1
  | Handcart2
  deriving (Eq)
       


food  (R (f, _, _, _)) = f
wood  (R (_, w, _, _)) = w
gold  (R (_, _, g, _)) = g
stone (R (_, _, _, s)) = s

data DefMeta =
  DM { name      :: String
     , cost      :: Resources
     , hp        :: HP
     , buildtime :: (Time, Time)
     }
  deriving (Eq)

data UpgradeStatus =
  US { doneUps     :: [Upgrade]
     , possibleUps :: [( Age, Upgrade )]
     }

data Building =
  Building { bldgMeta :: DefMeta
           , upgrades :: UpgradeStatus
           }

  

data Age
     = Dark
     | Feudal
     | Castle
     | Imperial

data Villager
  = MkVillager
    { villMeta  :: DefMeta
    , gathered  :: Time -> Resources
    }


instance Eq Villager where
  (MkVillager metaV1 _) == (MkVillager metaV2 _) = metaV1 == metaV2


instance Show Villager where
  show (MkVillager (DM s _ hp rt) _) = s ++ "(" ++ show hp ++ ")"

gather :: (Float -> Resources)-> Rate -> Time -> Resources
gather ins (r, rbt) n
  = ins $ 50.0 / r * fromIntegral (n - rbt)


onFood, onWood, onGold, onStone :: Float -> Resources
onFood  f = R (f, 0, 0, 0)
onWood  w = R (0, w, 0, 0)
onGold  g = R (0, 0, g, 0)
onStone s = R (0, 0, 0, s)

vilCost = R (50, 0, 0, 0)
vilHP   = 25
vilBT   = 25

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


house      = Building (DM "House" (onWood 30) 900 (25, 0)) (US [Support5Units] [])
towncenter = Building (DM "Towncenter" (onWood 275 + onStone 100) 1000 (300, 0))
                      (US [Support5Units]
                          [ ( Dark, Loom )
                          , ( Dark, Handcart1 )
                          , ( Feudal, Handcart2 )
                          ]
                      )


stdRate :: Int -> Rate
stdRate r = (fromIntegral r, 0)


popLimit :: Int
popLimit = 200

actPop :: Gamestate -> Int
actPop (State _ vs _ _ _) = length vs

maxPop :: Gamestate -> Int
maxPop (State _ _ bs _ _)
  = 5 * (length $ filter (\(Building _ (US x _)) -> Support5Units `elem` x) bs)


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

standardState =
  State
    { ressources = R (200, 200, 100, 0)
    , villagers  = replicate 3 idle
    , buildings  = [towncenter]
    , population = (3, 5)
    , time       = 0
    } 

emptyState =
  State
    { ressources = R (0, 0, 0, 0)
    , villagers  = []
    , buildings  = []
    , population = (0, 0)
    , time       = 0
    } 

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

resSum :: [Resources] -> Resources -> Resources
resSum rs r = foldl (\(R (f1, w1, g1, s1)) (R (f2, w2, g2, s2)) -> (R (f1 + f2, w1 + w2, g1 + g2, s1 + s2))) r rs

instance Num Resources where
  (R (f1,w1,g1,s1)) + (R (f2,w2,g2,s2)) = R (f1+f2, w1+w2, g1+g2, s1+s2)
  (R (f1,w1,g1,s1)) - (R (f2,w2,g2,s2)) = R (f1-f2, w1-w2, g1-g2, s1-s2)
  (R (f1,w1,g1,s1)) * (R (f2,w2,g2,s2)) = R (f1*f2, w1*w2, g1*g2, s1*s2)
  negate      (R (f,w,g,s))             = R (negate f, negate w, negate g, negate s)
  abs         (R (f,w,g,s))             = R (abs f, abs w, abs g, abs s)
  signum      (R (f,w,g,s))             = R (signum f, signum w, signum g, signum s)
  fromInteger i                         = R (fromInteger i, fromInteger i, fromInteger i, fromInteger i)


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

-- Buildorder : 

test = execStateT prog [standardState]
  where prog =
          do
             idle `toBuild` house
             idle `toBuild` house
             idle `into` shepard
             idle `into` shepard
             idle `into` shepard

             sequence $ replicate 7 (create shepard)

             shepard `toBuild` house
             idle  `into` shepard
             
             sequence $ replicate 10 (create lumberjack)

             lumberjack `toBuild` house
             idle `into` lumberjack
             
             sequence $ replicate  2 (create stoneminer)
             sequence $ replicate  2 (create goldminer)             
             wait 60
             wait 60

---------------




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


showPopulation :: [Gamestate] -> ([Time],[Float])
showPopulation games
  = let z     = nub $ map (\x -> (time x, fromIntegral . length $ villagers x)) games
    in  unzip $ reverse $ stepInterpole z

showSupportedPop :: [Gamestate] -> ([Time],[Float])
showSupportedPop games
  = let z     = nub $ map (\x -> (time x, fromIntegral . maxPop $ x)) games
    in  unzip $ reverse $ stepInterpole z

showRessource :: (Resources -> Float) -> [Gamestate] -> ([Time],[Float])
showRessource res games
  = let z      = nub $ map (\x -> (time x, res $ ressources x)) games
    in  unzip $ reverse $ interpole2 z

showFood, showWood, showGold, showStone :: [Gamestate] -> ([Time],[Float])
showFood  = showRessource food
showWood  = showRessource wood
showGold  = showRessource gold
showStone = showRessource stone

chart x = layout
  where minVal = 0
        maxVal = 250
        foodChart
          =   plot_lines_values ^= [ uncurry (zipWith (\x y -> ( fromIntegral x :: Double
                                                               , (fromRational $ toRational y) :: Double
                                                               )))
                                   $ showFood x
                                   ]
            $ plot_lines_style  .> line_color ^= opaque red
            $ plot_lines_title  ^= "food"
            $ defaultPlotLines
        woodChart
          =   plot_lines_values ^= [ uncurry (zipWith (\x y -> ( fromIntegral x :: Double
                                                               , (fromRational $ toRational y) :: Double
                                                               )))
                                   $ showWood x
                                   ]
            $ plot_lines_style  .> line_color ^= opaque green
            $ plot_lines_title  ^= "wood"
            $ defaultPlotLines
        goldChart
          =   plot_lines_values ^= [ uncurry (zipWith (\x y -> ( fromIntegral x :: Double
                                                               , (fromRational $ toRational y) :: Double
                                                               )))
                                   $ showGold x
                                   ]
            $ plot_lines_style  .> line_color ^= opaque orange
            $ plot_lines_title  ^= "gold"
            $ defaultPlotLines
        stoneChart
          =   plot_lines_values ^= [ uncurry (zipWith (\x y -> ( fromIntegral x :: Double
                                                               , (fromRational $ toRational y) :: Double
                                                               )))
                                   $ showStone x
                                   ]
            $ plot_lines_style  .> line_color ^= opaque gray
            $ plot_lines_title  ^= "stone"
            $ defaultPlotLines
        popChart
          =   plot_points_style ^= filledCircles 1 (opaque lightcoral)
            $ plot_points_values ^= ( uncurry (zipWith (\x y -> ( fromIntegral x :: Double
                                                                , (fromRational $ toRational y) :: Double
                                                                )))
                                    $ showSupportedPop x

                                    )
            $ plot_points_title  ^= "Supported Population"
            $ defaultPlotPoints
        suppChart
          =   plot_points_style ^= filledCircles 1 (opaque lightsteelblue)
            $ plot_points_values ^= ( uncurry (zipWith (\x y -> ( fromIntegral x :: Double
                                                                , (fromRational $ toRational y) :: Double
                                                                )))
                                    $ showPopulation x
                                    )
            $ plot_points_title  ^= "Actual Population"
            $ defaultPlotPoints
        layout
          =   layout1_title     ^= "AoE2 Resources Preview"
            $ layout1_plots     ^= [ Left (toPlot foodChart)
                                   , Left (toPlot woodChart)
                                   , Left (toPlot goldChart)
                                   , Left (toPlot stoneChart)
                                   , Right (toPlot popChart)
                                   , Right (toPlot suppChart)                                     
                                   ]
            $ layout1_left_axis ^= defaultLayoutAxis { laxis_generate_ = (\x -> (autoAxis (minVal:maxVal:x))) }
            $ defaultLayout1

main
  = do x <- test
       renderableToWindow (toRenderable  $ chart x) 640 480
       renderableToPNGFile (toRenderable $ chart x) 640 480 "/tmp/test.png"

