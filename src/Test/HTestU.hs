{-# LANGUAGE ForeignFunctionInterface #-}
module Test.HTestU
( TestResult,
  runBattery,
  toResults,
  runBatteryToResults,
  c_smallCrush,
  c_crush,
  c_bigCrush,
  c_pseudoDIEHARD) where
import System.Random (RandomGen)

import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peek)
import Foreign.Ptr (freeHaskellFunPtr)

import System.IO.Unsafe (unsafePerformIO)

import Test.HTestU.Wrapping (Battery, WrappedCallback, genToWrappedCallback, c_createGenerator, c_deleteGenerator)
import Test.HTestU.BatteryResult (BatteryResultStruct(..))
import Test.HTestU.Streaming (RandomStream)

data TestResult = Fail | Suspect | OK deriving (Eq, Show)

failurePvalue = 0.0000000001 -- 10^(-10)
suspectPvalue = 0.001 -- 10^(-3)

pValueToResult :: Double -> TestResult
pValueToResult pvalue | pvalue < failurePvalue || pvalue > 1.0 - failurePvalue = Fail
                      | pvalue < suspectPvalue || pvalue > 1.0 - suspectPvalue = Suspect
                      | otherwise = OK

toResults :: [Double] -> [TestResult]
toResults = map pValueToResult

runBatteryToResults :: RandomGen g => (g -> RandomStream) -> g -> Battery -> [TestResult]
runBatteryToResults = ((toResults .) .) . runBattery

-- returns [Double] instead of IO [Double], because it is transparent (for the same gen result should be the same)
runBattery :: RandomGen g => (g -> RandomStream) -> g -> Battery -> [Double]
runBattery streamer gen crush = unsafePerformIO $ do
  callback <- genToWrappedCallback streamer gen
  return $ runBatteryOnCallback callback crush
{-# NOINLINE runBattery #-}

runBatteryOnCallback :: WrappedCallback -> Battery -> [Double]
runBatteryOnCallback callback crush = unsafePerformIO $ do
  generatorPtr <- c_createGenerator callback
  batteryResult <- crush generatorPtr
  (BR pValues testNumber) <- peek batteryResult
  c_deleteGenerator generatorPtr
  freeHaskellFunPtr callback
  free batteryResult
  cDoublePvalues <- peekArray (fromIntegral testNumber) pValues
  return $ map realToFrac cDoublePvalues
{-# NOINLINE runBatteryOnCallback #-}

foreign import ccall safe "bbattery_SmallCrush" c_smallCrush :: Battery
foreign import ccall safe "bbattery_Crush" c_crush :: Battery
foreign import ccall safe "bbattery_BigCrush" c_bigCrush :: Battery
foreign import ccall safe "bbattery_pseudoDIEHARD" c_pseudoDIEHARD :: Battery
