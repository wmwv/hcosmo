module FlatLCDMTest where

import Test.HUnit.Approx (assertApproxEqual)
import FlatLCDM
import Text.Printf

testAge:: Double -> Double -> Double -> Double -> Double -> IO()
testAge om0 h0 z expected tol = do
    assertApproxEqual (printf "FlatLCDM Age(Om0: %.2f, H0: %.2f, z: %.2f)" om0 h0 z)
        tol expected (age om0 h0 z)

testAge' tol (om0, h0, z, expected) = testAge om0 h0 z expected tol

testAgeRunner = testAge 0.3 70.0 0.5 8.42634602 1e-6
testAgeRunner' = mapM_ (testAge' tol) testTableFlatLCDM
    where tol = 1e-6
          testTableFlatLCDM = [
            (0.3, 70.0, 0.5, 8.42634602),
            (1.0, 70.0, 0.5, 5.06897781)
            ]

testFunction name func om0 h0 z expected tol = do
    assertApproxEqual (printf "FlatLCDM %s(Om0: %.2f, H0: %.2f, z: %.2f)" name om0 h0 z)
        tol expected (func om0 h0 z)

testFunctionZ1Z2 name func om0 h0 z1 z2 expected tol = do
    assertApproxEqual (printf "FlatLCDM %s(Om0: %.2f, H0: %.2f, z1: %.2f, z2: %2.f)" name om0 h0 z1 z2)
        tol expected (func om0 h0 z1 z2)

testRun'' tol (name, func, om0, h0, z, expected) = testFunction name func om0 h0 z expected tol
testRunZ1Z2'' tol (name, func, om0, h0, z1, z2, expected) = testFunctionZ1Z2 name func om0 h0 z1 z2 expected tol

testTableFlatLCDM =
    [ ("LookbackTime", lookbacktime, 0.3, 70.0, 0.5, 5.04063793)
    , ("LookbackTimeEdS", lookbacktime, 1.0, 70.0, 0.5, 4.24332906)
    , ("Age", age, 0.3, 70.0, 0.5, 8.42634602)
    , ("AgeEdS", age, 1.0, 70.0, 0.5, 5.06897781)
    , ("ComovingTransverseDistance", comovingTransverseDistance, 0.3, 70.0, 0.5, 1888.62539593)
    , ("DistanceModulus", distanceModulus, 0.3, 70, 0.5, 42.26118542)
    , ("LuminosityDistance", luminosityDistance, 0.3, 70, 0.5, 2832.9380939)
    , ("ComovingDistanceEdS", comovingDistance, 1.0, 70, 0.5, 1571.79831586)
    , ("AngularDiameterDistance", angularDiameterDistance, 0.3, 70, 0.5, 1259.08359729)
    ]

testTableFlatLCDMZ1Z2 =
    [ ("ComovingDistanceZ1Z2Elliptic", comovingDistanceZ1Z2, 0.3, 70.0, 0.0, 0.5, 1888.62539593)
    ]

testRunner'' = mapM_ (testRun'' tol) testTableFlatLCDM
    where tol = 1e-6

testRunnerZ1Z2'' = mapM_ (testRunZ1Z2'' tol) testTableFlatLCDMZ1Z2
    where tol = 1e-6
