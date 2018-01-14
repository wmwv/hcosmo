module EdSTest 
( testRunner
, testRunnerZ1Z2
)
where

import Test.HUnit.Approx (assertApproxEqual)
import EdS
import Text.Printf

testFunction name func h0 z expected tol = do
    assertApproxEqual (printf "EdS %s(H0: %.2f, z: %.2f)" name h0 z)
        tol expected (func h0 z)

testFunctionZ1Z2 name func h0 z1 z2 expected tol = do
    assertApproxEqual (printf "EdS %s(H0: %.2f, z1: %.2f, z2: %2.f)" name h0 z1 z2)
        tol expected (func h0 z1 z2)

testRun tol (name, func, h0, z, expected) = testFunction name func h0 z expected tol
testRunZ1Z2 tol (name, func, h0, z1, z2, expected) = testFunctionZ1Z2 name func h0 z1 z2 expected tol

testTableEdS =
    [ ("LookbackTimeEdS", lookbacktime, 70.0, 0.5, 4.24332906)
    , ("AgeEdS", age, 70.0, 0.5, 5.06897781)
    , ("ComovingDistanceEdS", comovingDistance, 70, 0.5, 1571.79831586)
    ]

testTableEdSZ1Z2 =
    [ ("ComovingDistanceEdSZ1Z2", comovingDistanceZ1Z2, 70.0, 0.0, 0.5, 1571.79831586)
    ]

testRunner = mapM_ (testRun tol) testTableEdS
    where tol = 1e-6

testRunnerZ1Z2 = mapM_ (testRunZ1Z2 tol) testTableEdSZ1Z2
    where tol = 1e-6
