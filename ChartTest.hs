import Graphics.Rendering.Chart 
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import Data.Colour
import Data.Colour.Names

chart = layout
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = plot_lines_values ^= [[ (x,(am x)) | x <- [0,(0.5)..400]]]
              $ plot_lines_style  .> line_color ^= opaque blue
              $ plot_lines_title ^= "am"
              $ defaultPlotLines

    sinusoid2 = plot_points_style ^= filledCircles 2 (opaque red)
              $ plot_points_values ^= [ (x,(am x)) | x <- [0,7..400]]
              $ plot_points_title ^= "am points"
              $ defaultPlotPoints

    layout = layout1_title ^= "Amplitude Modulation"
           $ layout1_plots ^= [Left (toPlot sinusoid1),
                               Left (toPlot sinusoid2)]
           $ defaultLayout1

main = do
    renderableToWindow (toRenderable chart) 640 480
--    renderableToPNGFile (toRenderable chart) 640 480 "/tmp/test.png"
