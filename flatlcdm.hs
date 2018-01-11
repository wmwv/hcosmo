module FlatLCDM
( age
, lookbacktime
) where

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
