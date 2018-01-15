module Cosmo.EdS
( age
, angularDiameterDistance
, comovingDistance
, comovingDistanceZ1Z2
, comovingTransverseDistance
, distanceModulus
, lookbacktime
, luminosityDistance
) where

import Cosmo.Util

age :: Double -> Double -> Double
age h0 z = hubbleTime h0 * 2/3 * (1+z)**(-3/2)

lookbacktime :: Double -> Double -> Double
lookbacktime h0 z = age h0 0 - age h0 z

distanceModulus :: Double -> Double -> Double
distanceModulus h0 z = (+ 25.0) . (* 5.0) . logBase 10 $ luminosityDistance h0 z

luminosityDistance :: Double -> Double -> Double
luminosityDistance h0 z = (1+z) * comovingTransverseDistance h0 z

angularDiameterDistance :: Double -> Double -> Double
angularDiameterDistance h0 z = (comovingTransverseDistance h0 z) / (1+z)

comovingDistance :: Double -> Double -> Double
comovingDistance = comovingTransverseDistance

comovingTransverseDistance :: Double -> Double -> Double
comovingTransverseDistance h0 z = comovingTransverseDistanceZ1Z2 h0 0 z

comovingTransverseDistanceZ1Z2 :: Double -> Double -> Double -> Double
comovingTransverseDistanceZ1Z2 = comovingDistanceZ1Z2

comovingDistanceZ1Z2 :: Double -> Double -> Double -> Double
comovingDistanceZ1Z2 h0 z1 z2 =
    2 * hubbleDistance h0 * (1/sqrt(1+z1) - 1/sqrt(1+z2))

