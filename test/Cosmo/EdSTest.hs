module Cosmo.EdSTest
( testRunner
, testRunnerZ1Z2
)
where

import Test.HUnit.Approx (assertApproxEqual)
import Cosmo.Base( FLRW(..) )
import Cosmo.Base
import Cosmo.EdS
import Text.Printf

testFunction name func z expected tol = do
    assertApproxEqual (printf "EdS %s (z: %.2f)" name z)
        tol expected (func z)

testFunctionZ1Z2 name func z1 z2 expected tol = do
    assertApproxEqual (printf "%s (z1: %.2f, z2: %2.f)" name z1 z2)
        tol expected (func z1 z2)

testRun tol (name, func, z, expected) = testFunction name func z expected tol
testRunZ1Z2 tol (name, func, z1, z2, expected) = testFunctionZ1Z2 name func z1 z2 expected tol

testTableEdS =
    [ ("LookbackTimeEdS", lookbacktime EdS{h0=70.0}, 0.5, 4.24332906)
    , ("AgeEdS", age EdS{h0=70.0}, 0.5, 5.06897781)
    , ("ComovingDistanceEdS", comovingDistance EdS{h0=70}, 0.5, 1571.79831586)
    ]

testTableEdSZ1Z2 =
    [ ("ComovingDistanceEdSZ1Z2", comovingDistanceZ1Z2 EdS{h0=70.0}, 0.0, 0.5, 1571.79831586)
    ]

testRunner = mapM_ (testRun tol) testTableEdS
    where tol = 1e-6

testRunnerZ1Z2 = mapM_ (testRunZ1Z2 tol) testTableEdSZ1Z2
    where tol = 1e-6
