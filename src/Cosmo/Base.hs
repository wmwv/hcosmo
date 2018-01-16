module Cosmo.Base
( FLRW(..)
)
where

class FLRW a where
    age :: a -> Double -> Double 
    lookbacktime :: a -> Double -> Double 
    comovingDistance :: a -> Double -> Double
    comovingDistanceZ1Z2 :: a -> Double -> Double -> Double
    comovingTransverseDistance :: a -> Double -> Double
    comovingTransverseDistanceZ1Z2 :: a -> Double -> Double -> Double
    angularDiameterDistance :: a -> Double -> Double
    luminosityDistance :: a -> Double -> Double
    distanceModulus :: a -> Double -> Double


-- data FlatLCDM = FlatLCDM {h0 :: Double, om0 :: Double} 
-- LambdaCDM {h0 :: Double, om0 :: Double, Ol0 :: Double}
-- WCDM Double {h0 :: Double, om0 :: Double, Ol0 :: Double, W0 :: Double}
-- W0WACDM Double {h0 :: Double, om0 :: Double, Ol0 :: Double, W0 :: Double, WA :: Double}
