{-# OPTIONS_GHC -Wall #-}

module Main where

import SpatialMath
import Vis

-- our state data
data State a = State (Xyz a) [Xyz a] a

-- attractor parameters
rho,sigma,beta :: Double
sigma = 10
beta = 8/3
rho = 28  -- chaotic
--rho = 14  -- not chaotic

-- attractor ODE
ddt :: Xyz Double -> Xyz Double
ddt (Xyz x y z) = Xyz x' y' z'
  where
    x' = sigma*(y-x)
    y' = x*(rho-z) - y
    z' = x*y- beta*z

-- simulation timestep
ts :: Double
ts = 0.01

-- length of display trails
trailLength :: Int
trailLength = 500

-- simple forward euler integration
forwardEulerStep :: Xyz Double -> Xyz Double
forwardEulerStep xyz = xyz + fmap (*ts) (ddt xyz)

rk4 :: Xyz Double -> Xyz Double
rk4 xyz = xyz + (1/6) * (k1 + 2*k2 + 2*k3 + k4)
            where k1 = fmap (*ts) (ddt xyz)
                  k2 = fmap (*ts) (ddt xyz + 0.5*k1)
                  k3 = fmap (*ts) (ddt xyz + 0.5*k2)
                  k4 = fmap (*ts) (ddt xyz + k3)

-- simulation function
simFun :: State Double -> State Double
simFun (State x trail' simTime0) = State xNext trail (simTime0 + ts)
  where
    xNext = rk4 x
    trail = take trailLength $ (fmap (*0.1) xNext - (Xyz 0 0 1)):trail'

simFun' :: Float -> ((State Double), (State Double)) -> ((State Double), (State Double))
simFun' _ state = (simFun (fst state), simFun (snd state))

-- drawing function
drawFun :: ((State Double), (State Double)) -> VisObject Double
drawFun ((State _ trail1 simTime), (State _ trail2 _)) = VisObjects [axes,line1,line2,text]
  where
    -- draw some axes so we know which way's up
    axes = Axes (0.5, 15)
    -- draw the trails with varying color and transparency
    line1 = Line' $ zip trail1 (map (\a -> makeColor a 0 (1-a) a) (linspace 1 0 (length trail1)))
    line2 = Line' $ zip trail2 (map (\a -> makeColor (1-a) a 0 a) (linspace 1 0 (length trail2)))
    -- draw the simulation time
    text = Text2d ("sim time: "++take 5 (show simTime)) (30,30) TimesRoman24 (makeColor 1 1 1 1)

-- utility function
linspace :: Fractional a => a -> a -> Int -> [a]
linspace x0 xf n = map (\k -> x0 + (xf - x0) * (fromIntegral k) / (fromIntegral n-1)) [0..(n-1)]


main :: IO ()
main = simulate Nothing "lorentz attractor" ts state0 drawFun simFun'
  where
    state0 = ((State (Xyz 1 1 1) [] 0), (State (Xyz 1.0001 1 1) [] 0))
