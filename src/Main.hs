{-# OPTIONS_GHC -Wall #-}

module Main where

import SpatialMath

import Vis

data State a = State (Xyz a) [Xyz a] a

ts :: Double
ts = 0.01

rho,sigma,beta :: Double
rho = 28
sigma = 10
beta = 8/3

trailLength :: Int
trailLength = 2000

ddt :: Xyz Double -> Xyz Double
ddt (Xyz x y z) = Xyz x' y' z'
  where
    x' = sigma*(y-x)
    y' = x*(rho-z) - y
    z' = x*y- beta*z

integrate :: Xyz Double -> Double -> Xyz Double
integrate xyz dt = xyz + fmap (*dt) (ddt xyz)

simFun :: Float -> State Double -> State Double
simFun _ (State x trail' simTime0) = State xNext trail (simTime0 + dt)
  where
    dt = ts
    xNext = integrate x dt
    trail = take trailLength $ (fmap (*0.1) xNext - (Xyz 0 0 1)):trail'

drawFun :: State Double -> VisObject Double
drawFun (State _ trail simTime) = VisObjects [axes,line,text]
  where
    axes = Axes (0.5, 15)
    line = Line' $ zip trail (map (\a -> makeColor a 0 (1-a) a) (linspace 1 0 (length trail))) -- (makeColor 1 0 0 1)
    text = Text2d ("sim time: "++take 5 (show simTime)) (30,30) TimesRoman24 (makeColor 1 1 1 1)

linspace :: Fractional a => a -> a -> Int -> [a]
linspace x0 xf n = map (\k -> x0 + (xf - x0) * (fromIntegral k) / (fromIntegral n-1)) [0..(n-1)]

main :: IO ()
main = simulate Nothing "simulate test" ts state0 drawFun simFun
  where
    state0 = State (Xyz 1 1 1) [] 0
