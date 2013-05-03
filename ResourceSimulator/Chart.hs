module ResourceSimulator.Chart
where

import Graphics.Rendering.Chart 
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import Data.Colour 
import Data.Colour.Names hiding (gold)

import Data.List (nub)

import ResourceSimulator.Game
import ResourceSimulator.Game.Gamestate
import ResourceSimulator.Game.Auxiliary
import ResourceSimulator.Game.Common
import ResourceSimulator.Game.Resources

  
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


-- for Chart generation
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
