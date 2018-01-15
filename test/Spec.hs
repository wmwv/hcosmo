import Cosmo.FlatLCDMTest as FlatLCDMTest
import Cosmo.EdSTest as EdSTest

main :: IO ()
main = do
    FlatLCDMTest.testRunner
    FlatLCDMTest.testRunnerZ1Z2
    EdSTest.testRunner
    EdSTest.testRunnerZ1Z2
