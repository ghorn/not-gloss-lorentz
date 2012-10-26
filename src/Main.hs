{-# OPTIONS_GHC -Wall #-}

module Main where

import SpatialMath
import Vis

-- our state data
data State a = State (Xyz a) [Xyz a] a

-- attractor parameters
rho,sigma,beta :: Double
rho = 28
sigma = 10
beta = 8/3

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
trailLength = 2000

-- simple forward euler integration
forwardEulerStep :: Xyz Double -> Xyz Double
forwardEulerStep xyz = xyz + fmap (*ts) (ddt xyz)

-- simulation function
simFun :: Float -> State Double -> State Double
simFun _ (State x trail' simTime0) = State xNext trail (simTime0 + ts)
  where
    xNext = forwardEulerStep x
    trail = take trailLength $ (fmap (*0.1) xNext - (Xyz 0 0 1)):trail'

-- drawing function
drawFun :: State Double -> VisObject Double
drawFun (State _ trail simTime) = VisObjects [axes,line,text]
  where
    -- draw some axes so we know which way's up
    axes = Axes (0.5, 15)
    -- draw the trails with varying color and transparency
    line = Line' $ zip trail (map (\a -> makeColor a 0 (1-a) a) (linspace 1 0 (length trail)))
    -- draw the simulation time
    text = Text2d ("sim time: "++take 5 (show simTime)) (30,30) TimesRoman24 (makeColor 1 1 1 1)

-- utility function
linspace :: Fractional a => a -> a -> Int -> [a]
linspace x0 xf n = map (\k -> x0 + (xf - x0) * (fromIntegral k) / (fromIntegral n-1)) [0..(n-1)]


main :: IO ()
main = simulate Nothing "lorentz attractor" ts state0 drawFun simFun
  where
    state0 = State (Xyz 1 1 1) [] 0
