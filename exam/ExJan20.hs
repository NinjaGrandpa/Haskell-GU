module ExJan20 where

-- * 1. Regions

type Point = (Double, Double)
type Region = Point -> Bool

rectangle :: Double -> Double -> Region
rectangle w h = \(x, y) -> and [x >= -w', x <= w', y >= -h', y <= h']
    where 
        w' = w / 2
        h' = h / 2

inside :: Point -> Region -> Bool
p `inside` r = r p

-- ** a) Define a function that calculates the euclidian distance between two points:
dist :: Point -> Point -> Double
dist (px, py) (qx, qy) =  sqrt $ (px - qx)^2 + (py - qy)^2

-- ** b)
type Radius = Double

circle :: Radius -> Region
circle r = \(x, y) -> and [x >= -r, x <= r, y >= -r, y <= r]
  where 
    a = pi * r^2

-- ** c)

