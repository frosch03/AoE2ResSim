module ResourceSimulator.Game.Resources
where

data Resources = R (Food, Wood, Gold, Stone) deriving (Eq, Show)

type Food  = Float
type Wood  = Float
type Gold  = Float
type Stone = Float


instance Num Resources where
  (R (f1,w1,g1,s1)) + (R (f2,w2,g2,s2)) = R (f1+f2, w1+w2, g1+g2, s1+s2)
  (R (f1,w1,g1,s1)) - (R (f2,w2,g2,s2)) = R (f1-f2, w1-w2, g1-g2, s1-s2)
  (R (f1,w1,g1,s1)) * (R (f2,w2,g2,s2)) = R (f1*f2, w1*w2, g1*g2, s1*s2)
  negate      (R (f,w,g,s))             = R (negate f, negate w, negate g, negate s)
  abs         (R (f,w,g,s))             = R (abs f, abs w, abs g, abs s)
  signum      (R (f,w,g,s))             = R (signum f, signum w, signum g, signum s)
  fromInteger i                         = R (fromInteger i, fromInteger i, fromInteger i, fromInteger i)



food  (R (f, _, _, _)) = f
wood  (R (_, w, _, _)) = w
gold  (R (_, _, g, _)) = g
stone (R (_, _, _, s)) = s

resSum :: [Resources] -> Resources -> Resources
resSum rs r = foldl (\(R (f1, w1, g1, s1)) (R (f2, w2, g2, s2)) -> (R (f1 + f2, w1 + w2, g1 + g2, s1 + s2))) r rs

