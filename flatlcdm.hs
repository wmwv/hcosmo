module FlatLCDM
( age
, comovingTransverseDistance
, lookbacktime
) where

import Numeric.GSL.Special (ellint_RF, Precision(..))

hubbleTime :: Double -> Double
hubbleTime h0 = (1/h0) * kmPerMpc / secPerGyr
    where
        secPerGyr = 1e9 * 365.25 * 24 * 3600
        kmPerMpc = 3.08567758149137e19

age :: Double -> Double -> Double -> Double
age om0 h0 z
    | om0 == 1 = hubbleTime h0 * 2/3 * (1+z)**(-3/2)
    | otherwise = hubbleTime h0 * 2/3 / sqrt(1-om0) * asinh(sqrt((1/om0 -1)/(1+z)**3))

lookbacktime :: Double -> Double -> Double -> Double
lookbacktime om0 h0 z = age om0 h0 0 - age om0 h0 z

comovingTransverseDistance :: Double -> Double -> Double -> Double
comovingTransverseDistance om0 h0 z = comovingTransverseDistanceZ1Z2 om0 h0 0 z

comovingTransverseDistanceZ1Z2 :: Double -> Double -> Double -> Double -> Double
comovingTransverseDistanceZ1Z2 = comovingDistanceZ1Z2

comovingDistanceZ1Z2 :: Double -> Double -> Double -> Double -> Double
comovingDistanceZ1Z2 = comovingDistanceZ1Z2Elliptic

comovingDistanceZ1Z2Elliptic :: Double -> Double -> Double -> Double -> Double
comovingDistanceZ1Z2Elliptic om0 h0 z1 z2 =
    prefactor * (tElliptic (s / (1+z1)) - tElliptic (s/(1+z2)))
    where prefactor = (c / h0) * (1/sqrt(s*om0))
          c = 299792.458  -- km/s
          s = ((1-om0)/om0)**(1/3)

 -- tElliptic uses elliptic integral of the first kind in Carlson form
 -- to calculate the basic integral for cosmological distances
tElliptic :: Double -> Double
tElliptic s = 4 * ellint_RF x y z PrecDouble
    where m = (2 * sqrt(s*s-s+1) / s) + (2 / s) - 1
          x = m
          y = m + 3 - 2*sqrt(3)
          z = m + 3 + 2*sqrt(3)
