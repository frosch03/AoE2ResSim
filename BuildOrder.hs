module Main
where

import Control.Monad.State
import System.IO

import Graphics.Rendering.Chart 
import Graphics.Rendering.Chart.Gtk

  
import ResourceSimulator
import ResourceSimulator.Game
import ResourceSimulator.Game.Villager
import ResourceSimulator.Game.Building
import ResourceSimulator.Chart


-- Buildorder : 

buildOrder = execStateT prog [standardGame]
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


main
  = do x <- buildOrder
       renderableToPNGFile (toRenderable $ chart x) 640 480 "../BuildOrder.png"
