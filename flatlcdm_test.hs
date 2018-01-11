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

testRun'' tol (name, func, om0, h0, z, expected) = testFunction name func om0 h0 z expected tol

testRunner'' = mapM_ (testRun'' tol) testTableFlatLCDM
    where tol = 1e-6
          testTableFlatLCDM = [
            ("Age", age, 0.3, 70.0, 0.5, 8.42634602),
            ("AgeEdS", age, 1.0, 70.0, 0.5, 5.06897781)
            ]
