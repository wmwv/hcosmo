import Test.HUnit.Approx (assertApproxEqual)
import FlatLCDM
import Text.Printf

testAge:: Double -> Double -> Double -> Double -> Double -> IO()
testAge om0 h0 z expected tol = do
    assertApproxEqual (printf "FlatLCDM Age(Om0: %.2f, H0: %.2f, z: %.2f)" om0 h0 z)
        tol expected (age om0 h0 z)

testAge' :: (Double, Double, Double, Double, Double) -> IO()
testAge' (om0, h0, z, expected, tol) = do
    assertApproxEqual (printf "FlatLCDM Age(Om0: %.2f, H0: %.2f, z: %.2f)" om0 h0 z)
        tol expected (age om0 h0 z)

testAge'' tol (om0, h0, z, expected) = testAge om0 h0 z expected tol

testAgeRunner = testAge 0.3 70.0 0.5 8.42634602 1e-6
testAgeRunner' = mapM_ (testAge'' tol) testTableFlatLCDM
    where tol = 1e-6
          testTableFlatLCDM = [(0.3, 70.0, 0.5, 8.42634602)]
