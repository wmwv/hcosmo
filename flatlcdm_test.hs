import Test.HUnit.Approx (assertApproxEqual)
import FlatLCDM

testAge = do
    assertApproxEqual "FlatLCDM Age." 1e-6 8.42634602 (age 0.5 0.3 70.0)
