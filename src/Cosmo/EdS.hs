module Cosmo.EdS
( EdS(..)
, age
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
import Cosmo.Util

data EdS = EdS {h0 :: Double} 

instance FLRW EdS where
    age eds z = hubbleTime (h0 eds) * 2/3 * (1+z)**(-3/2)
    lookbacktime eds z = age eds 0 - age eds z
    distanceModulus eds z = (+ 25.0) . (* 5.0) . logBase 10 $ luminosityDistance eds z
    luminosityDistance eds z = (1+z) * comovingTransverseDistance eds z
    angularDiameterDistance eds z = (comovingTransverseDistance eds z) / (1+z)
    comovingDistance = comovingTransverseDistance
    comovingTransverseDistance eds z = comovingTransverseDistanceZ1Z2 eds 0 z
    comovingTransverseDistanceZ1Z2 = comovingDistanceZ1Z2
    comovingDistanceZ1Z2 eds z1 z2 =
        2 * hubbleDistance (h0 eds) * (1/sqrt(1+z1) - 1/sqrt(1+z2))

