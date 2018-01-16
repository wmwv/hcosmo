module Cosmo.FlatLCDMTest
( testRunner
, testRunnerZ1Z2
)
where

import Test.HUnit.Approx (assertApproxEqual)
import Cosmo.Base ( FLRW(..) )
import Cosmo.Base
import Cosmo.FlatLCDM
import Text.Printf

testFunction name func z expected tol = do
    assertApproxEqual (printf "FlatLCDM %s (z: %.2f)" name z)
        tol expected (func z)

testFunctionZ1Z2 name func z1 z2 expected tol = do
    assertApproxEqual (printf "FlatLCDM %s (z1: %.2f, z2: %2.f)" name z1 z2)
        tol expected (func z1 z2)

testRun tol (name, func, z, expected) = testFunction name func z expected tol
testRunZ1Z2 tol (name, func, z1, z2, expected) = testFunctionZ1Z2 name func z1 z2 expected tol

testTableFlatLCDM =
    [ ("LookbackTime", lookbacktime FlatLCDM{h0=70.0, om0=0.3}, 0.5, 5.04063793)
    , ("LookbackTimeEdS", lookbacktime FlatLCDM{h0=70.0, om0=1.0}, 0.5, 4.24332906)
    , ("Age", age FlatLCDM{h0=70.0, om0=0.3}, 0.5, 8.42634602)
    , ("AgeEdS", age FlatLCDM{h0=70.0, om0=1.0}, 0.5, 5.06897781)
    , ("ComovingTransverseDistance", comovingTransverseDistance FlatLCDM{h0=70.0, om0=0.3}, 0.5, 1888.62539593)
    , ("DistanceModulus", distanceModulus FlatLCDM{h0=70.0, om0=0.3}, 0.5, 42.26118542)
    , ("LuminosityDistance", luminosityDistance FlatLCDM{h0=70.0, om0=0.3}, 0.5, 2832.9380939)
    , ("ComovingDistanceEdS", comovingDistance FlatLCDM{h0=70.0, om0=1.0}, 0.5, 1571.79831586)
    , ("AngularDiameterDistance", angularDiameterDistance FlatLCDM{h0=70.0, om0=0.3}, 0.5, 1259.08359729)
    ]

testTableFlatLCDMZ1Z2 =
    [ ("ComovingDistanceZ1Z2Elliptic", comovingDistanceZ1Z2 FlatLCDM{h0=70.0, om0=0.3}, 0.0, 0.5, 1888.62539593)
    ]

testRunner = mapM_ (testRun tol) testTableFlatLCDM
    where tol = 1e-6

testRunnerZ1Z2 = mapM_ (testRunZ1Z2 tol) testTableFlatLCDMZ1Z2
    where tol = 1e-6
