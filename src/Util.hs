module Util (
  hubbleTime
, hubbleDistance
) where

hubbleDistance :: Double -> Double
hubbleDistance h0 = c/h0  -- Mpc
    where c = 299792.458  -- km/s

hubbleTime :: Double -> Double
hubbleTime h0 = (1/h0) * kmPerMpc / secPerGyr
    where
        secPerGyr = 1e9 * 365.25 * 24 * 3600
        kmPerMpc = 3.08567758149137e19
