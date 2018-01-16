module Cosmo.FlatLCDM
( age
, angularDiameterDistance
, comovingDistance
, comovingDistanceZ1Z2
, comovingTransverseDistance
, comovingTransverseDistanceZ1Z2
, distanceModulus
, lookbacktime
, luminosityDistance
) where

import Cosmo.Base( FLRW(..) )
import qualified Cosmo.EdS as EdS
import Numeric.GSL.Special (ellint_RF, Precision(..))
import Numeric.GSL.Integration
import Cosmo.Util

data FlatLCDM = FlatLCDM {h0 :: Double, om0 :: Double}

instance FLRW FlatLCDM where
    age flatlcdm z
        | om == 1 = EdS.age flatlcdm z
        | otherwise = hubbleTime h * 2/3 / sqrt(1-om) * asinh(sqrt((1/om -1)/(1+z)**3))
        where h = h0 flatlcdm
              om = om0 flatlcdm
    lookbacktime flatlcdm z = age flatlcdm 0 - age flatlcdm z
    distanceModulus flatlcdm z = (+ 25.0) . (* 5.0) . logBase 10 $ luminosityDistance flatlcdm z
    -- Want to write something like this:
    -- distanceModulus = 25 + 5 * (logBase 10) $ luminosityDistance
    luminosityDistance flatlcdm z = (1+z) * comovingTransverseDistance flatlcdm z
    angularDiameterDistance flatlcdm z = (comovingTransverseDistance flatlcdm z) / (1+z)
    comovingDistance = comovingTransverseDistance
    comovingTransverseDistance flatlcdm z = comovingTransverseDistanceZ1Z2 flatlcdm 0 z
    comovingTransverseDistanceZ1Z2 = comovingDistanceZ1Z2
    comovingDistanceZ1Z2 flatlcdm z1 z2
        | om == 1 = EdS.comovingDistanceZ1Z2 flatlcdm z1 z2
        | om < 1 = comovingDistanceZ1Z2Elliptic flatlcdm z1 z2
        | otherwise = comovingDistanceZ1Z2Integrate flatlcdm z1 z2
        where h = h0 flatlcdm
              om = om0 flatlcdm

e :: FlatLCDM -> Double -> Double
e flatlcdm z = sqrt((1+z)**3 * om + ol)
    where ol = 1-(om0 flatlcdm)
          om = om0 flatlcdm

eInv :: FlatLCDM -> Double -> Double
eInv flatlcdm z = 1 / e flatlcdm z

comovingDistanceZ1Z2Integrate :: FlatLCDM -> Double -> Double -> Double
comovingDistanceZ1Z2Integrate flatlcdm z1 z2 = result
    where (result, err) = integrateQNG 1e-9 (eInv flatlcdm) z1 z2

comovingDistanceZ1Z2Elliptic :: FlatLCDM -> Double -> Double -> Double
comovingDistanceZ1Z2Elliptic flatlcdm z1 z2 =
    prefactor * (tElliptic (s / (1+z1)) - tElliptic (s/(1+z2)))
    where prefactor = hubbleDistance h * (1/sqrt(s*om))
          s = ((1-om)/om)**(1/3)
          h = h0 flatlcdm
          om = om0 flatlcdm

 -- tElliptic uses elliptic integral of the first kind in Carlson form
 -- to calculate the basic integral for cosmological distances
tElliptic :: Double -> Double
tElliptic s = 4 * ellint_RF x y z PrecDouble
    where m = (2 * sqrt(s*s-s+1) / s) + (2 / s) - 1
          x = m
          y = m + 3 - 2*sqrt(3)
          z = m + 3 + 2*sqrt(3)
