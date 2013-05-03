{-# LANGUAGE EmptyDataDecls, FlexibleInstances, ExistentialQuantification #-}
module Test
where

import Control.Monad.State
import System.IO

import Data.List (nub)

-- Chart
import Graphics.Rendering.Chart 
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import Data.Colour 
import Data.Colour.Names hiding (gold)

--

data TownCenter =
  MkTC { age          :: Age
       , options      :: [Option]
       , possibleOpts :: [( Age, Option )]
       }

type Food  = Float
type Wood  = Float
type Gold  = Float
type Stone = Float

type FiftyResPer  = Float

type Time = Int
data Ressources = R (Food, Wood, Gold, Stone) deriving (Show)
type HP = Int
type Rate = (FiftyResPer, Time)

type Option = Bool

food  (R (f, _, _, _)) = f
wood  (R (_, w, _, _)) = w
gold  (R (_, _, g, _)) = g
stone (R (_, _, _, s)) = s

data Age
     = Dark
     | Feudal
     | Castle
     | Imperial

data Villager
  = MkVillager
    { name      :: String
    , cost      :: Ressources
    , hp        :: HP                            -- HitPoints
    , gathered  :: Time -> Ressources
    , buildtime :: (Time, Time)                  -- (Original Buildtime, BonusTime)
    }

instance Show Villager where
  show (MkVillager s _ hp rt _) = s ++ "(" ++ show hp ++ ")"

gather :: (Float -> Ressources)-> Rate -> Time -> Ressources
gather ins (r, rbt) n
  = ins $ 50.0 / r * fromIntegral (n - rbt)


onFood, onWood, onGold, onStone :: Float -> Ressources
onFood  f = R (f, 0, 0, 0)
onWood  w = R (0, w, 0, 0)
onGold  g = R (0, 0, g, 0)
onStone s = R (0, 0, 0, s)

vilCost = R (50, 0, 0, 0)
vilHP   = 25
vilBT   = 25

villager :: String -> (Float -> Ressources) -> Rate -> Villager
villager s ins r = MkVillager s vilCost vilHP (gather ins r) (vilBT, 0) 
 
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

stdRate :: Int -> Rate
stdRate r = (fromIntegral r, 0)


data Gamestate =
  State
    { ressources :: Ressources
    , villagers  :: [Villager]
    , time       :: Time
    }

instance Show Gamestate where
  show (State (R (f,w,g,s)) vills t) = concat (map (\w -> show w ++ "\n") vills)
                                   ++ "(" ++ show t ++ "s) "
                                   ++ "[" ++ show (truncate f) ++ "F "
                                   ++        show (truncate w) ++ "W "
                                   ++        show (truncate g) ++ "G "
                                   ++        show (truncate s) ++ "S]\n"

standardState =
  State
    { ressources = R (200, 200, 100, 0)
    , villagers  = replicate 3 idle
    , time       = 0
    } 

emptyState =
  State
    { ressources = R (0, 0, 0, 0)
    , villagers  = []
    , time       = 0
    } 

buildVillager :: Gamestate -> Villager -> (Gamestate, Gamestate)
buildVillager gs w@(MkVillager _ _ _ _ (bt, btb))
  = ( gs { ressources = gsRes
         , time = (time gs) + 1
         }
    , gs { ressources = resSum (map (\x -> (gathered x) bt) $ villagers gs) gsRes
         , villagers = w : (villagers gs)
         , time = (time gs) + (bt - btb)
         }
    ) 
  where gsRes = ressources gs - (cost w)

transform :: Gamestate -> Villager -> Villager -> Gamestate
transform gs fr to 
  = gs { villagers = switchOne fr (villagers gs) }
  where vs = villagers gs
        switchOne _ [] = []
        switchOne x (v:vs)
          | name x == name v = to:vs
          | otherwise        =  v:(switchOne x vs)
        
simulateFor :: Gamestate -> Time -> Gamestate
simulateFor gs t
  = gs { ressources = resSum (map (\x -> (gathered x) t) $ villagers gs) $ ressources gs
       , time = time gs + t }

resSum :: [Ressources] -> Ressources -> Ressources
resSum rs r = foldl (\(R (f1, w1, g1, s1)) (R (f2, w2, g2, s2)) -> (R (f1 + f2, w1 + w2, g1 + g2, s1 + s2))) r rs

instance Num Ressources where
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



-- Buildorder : 

test = execStateT prog [standardState]
  where prog =
          do
             idle `into` shepard
             idle `into` shepard
             idle `into` shepard
             sequence $ replicate 5 (create shepard)
             sequence $ replicate 10 (create lumberjack)             
--             wait 60
--             wait 60

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

showRessource :: (Ressources -> Float) -> [Gamestate] -> ([Time],[Float])
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
        layout
          =   layout1_title     ^= "AoE2 Ressources Preview"
            $ layout1_plots     ^= [ Left (toPlot foodChart)
                                   , Left (toPlot woodChart)
                                   , Left (toPlot goldChart)
                                   , Left (toPlot stoneChart)                                     
                                   ]
            $ layout1_left_axis ^= defaultLayoutAxis { laxis_generate_ = (\x -> (autoAxis (minVal:maxVal:x))) }
            $ defaultLayout1

main
  = do x <- test
       renderableToWindow (toRenderable  $ chart x) 640 480
       renderableToPNGFile (toRenderable $ chart x) 640 480 "/tmp/test.png"

