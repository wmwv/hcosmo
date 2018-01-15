module Cosmo.FlatLCDM
( age
, angularDiameterDistance
, comovingDistance
, comovingDistanceZ1Z2
, comovingTransverseDistance
, distanceModulus
, lookbacktime
, luminosityDistance
) where

import qualified Cosmo.EdS as EdS
import Numeric.GSL.Special (ellint_RF, Precision(..))
import Numeric.GSL.Integration
import Cosmo.Util

age :: Double -> Double -> Double -> Double
age om0 h0 z
    | om0 == 1 = EdS.age h0 z
    | otherwise = hubbleTime h0 * 2/3 / sqrt(1-om0) * asinh(sqrt((1/om0 -1)/(1+z)**3))

lookbacktime :: Double -> Double -> Double -> Double
lookbacktime om0 h0 z = age om0 h0 0 - age om0 h0 z

distanceModulus :: Double -> Double -> Double -> Double
distanceModulus om0 h0 z = (+ 25.0) . (* 5.0) . logBase 10 $ luminosityDistance om0 h0 z
-- Want to write something like this:
-- distanceModulus = 25 + 5 * (logBase 10) $ luminosityDistance

luminosityDistance :: Double -> Double -> Double -> Double
luminosityDistance om0 h0 z = (1+z) * comovingTransverseDistance om0 h0 z

angularDiameterDistance :: Double -> Double -> Double -> Double
angularDiameterDistance om0 h0 z = (comovingTransverseDistance om0 h0 z) / (1+z)

comovingDistance :: Double -> Double -> Double -> Double
comovingDistance = comovingTransverseDistance

comovingTransverseDistance :: Double -> Double -> Double -> Double
comovingTransverseDistance om0 h0 z = comovingTransverseDistanceZ1Z2 om0 h0 0 z

comovingTransverseDistanceZ1Z2 :: Double -> Double -> Double -> Double -> Double
comovingTransverseDistanceZ1Z2 = comovingDistanceZ1Z2

comovingDistanceZ1Z2 :: Double -> Double -> Double -> Double -> Double
comovingDistanceZ1Z2 om0 h0 z1 z2
    | om0 == 1 = EdS.comovingDistanceZ1Z2 h0 z1 z2
    | om0 < 1 = comovingDistanceZ1Z2Elliptic om0 h0 z1 z2
    | otherwise = comovingDistanceZ1Z2Integrate om0 h0 z1 z2

e :: Double -> Double -> Double -> Double
e om0 h0 z = sqrt((1+z)**3 * om0 + ol0)
    where ol0 = 1-om0

eInv :: Double -> Double -> Double -> Double
eInv om0 h0 z = 1 / e om0 h0 z

comovingDistanceZ1Z2Integrate :: Double -> Double -> Double -> Double -> Double
comovingDistanceZ1Z2Integrate om0 h0 z1 z2 = result
    where (result, err) = integrateQNG 1e-9 (eInv om0 h0) z1 z2

comovingDistanceZ1Z2Elliptic :: Double -> Double -> Double -> Double -> Double
comovingDistanceZ1Z2Elliptic om0 h0 z1 z2 =
    prefactor * (tElliptic (s / (1+z1)) - tElliptic (s/(1+z2)))
    where prefactor = hubbleDistance h0 * (1/sqrt(s*om0))
          s = ((1-om0)/om0)**(1/3)

 -- tElliptic uses elliptic integral of the first kind in Carlson form
 -- to calculate the basic integral for cosmological distances
tElliptic :: Double -> Double
tElliptic s = 4 * ellint_RF x y z PrecDouble
    where m = (2 * sqrt(s*s-s+1) / s) + (2 / s) - 1
          x = m
          y = m + 3 - 2*sqrt(3)
          z = m + 3 + 2*sqrt(3)
