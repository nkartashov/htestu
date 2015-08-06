{-# LANGUAGE ForeignFunctionInterface #-}
module Test.HTestU
  ( TestResult
  , runBattery
  , toResults
  , runBatteryToResults
  , c_smallCrush
  , c_crush
  , c_bigCrush
  ) where

import System.Random (RandomGen)

import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peek)
import Foreign.Ptr (freeHaskellFunPtr)

import System.IO.Unsafe (unsafePerformIO)

import Test.HTestU.Wrapping (Battery, WrappedCallback, genToWrappedCallback, c_createGenerator, c_deleteGenerator)
import Test.HTestU.BatteryResult (BatteryResultStruct(..))
import Test.HTestU.Streaming (RandomStream)

-- | Type for presenting a result of a test, instead of a p-value
data TestResult = Fail | Suspect | OK deriving (Eq, Show)

-- | P-value serving as a border for a failure in TestU01, greater => failure or suspicious value
failurePvalue = 0.0000000001 -- 10^(-10)

-- | P-value serving as a border for a suspicious value in TestU01, greater => failure
suspectPvalue = 0.001 -- 10^(-3)

-- | Prettifying a p-value to a test result
pValueToResult :: Double -> TestResult
pValueToResult pvalue | pvalue < failurePvalue || pvalue > 1.0 - failurePvalue = Fail
                      | pvalue < suspectPvalue || pvalue > 1.0 - suspectPvalue = Suspect
                      | otherwise = OK

-- | Prettifying a list of result p-values
toResults :: [Double] -> [TestResult]
toResults = map pValueToResult

-- | Run a given battery and present pretty results
runBatteryToResults :: RandomGen g => (g -> RandomStream) -> g -> Battery -> [TestResult]
runBatteryToResults = ((toResults .) .) . runBattery

-- | Run a given battery and present resulting p-values
-- NOTE: returns [Double] instead of IO [Double], because it is transparent (for the same gen result should be the same)
runBattery :: RandomGen g => (g -> RandomStream) -> g -> Battery -> [Double]
runBattery streamer gen crush = unsafePerformIO $ do
  callback <- genToWrappedCallback streamer gen
  generatorPtr <- c_createGenerator callback
  batteryResult <- crush generatorPtr
  (BR pValues testNumber) <- peek batteryResult
  c_deleteGenerator generatorPtr
  freeHaskellFunPtr callback
  free batteryResult
  cDoublePvalues <- peekArray (fromIntegral testNumber) pValues
  return $ map realToFrac cDoublePvalues
{-# NOINLINE runBattery #-}

-- | Runs SmallCrush battery, 10 tests
foreign import ccall safe "bbattery_SmallCrush" c_smallCrush :: Battery

-- | Runs Crush battery, 96 tests
foreign import ccall safe "bbattery_Crush" c_crush :: Battery

-- | Runs BigCrush Battery, 106 tests
foreign import ccall safe "bbattery_BigCrush" c_bigCrush :: Battery
