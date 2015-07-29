{-# LANGUAGE ForeignFunctionInterface #-}

module Test.HTestU
( TestResult,
  runBattery,
  toResults,
  runBatteryToResults,
  runBatteryOnStream,
  runBatteryOnCallback,
  c_smallCrush,
  c_crush,
  c_bigCrush,
  c_pseudoDIEHARD) where
import System.Random (RandomGen)

import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CUInt(..))
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr)
import Foreign.Storable

import System.IO.Unsafe (unsafePerformIO)

import Test.HTestU.Wrapping (WrappedGen, wrapForPassing)
import Test.HTestU.Streaming (RandomStream)
import Test.HTestU.BatteryResult (BatteryResultStruct(..))

data UniformGenerator
type BatteryResult = IO (Ptr BatteryResultStruct)
type Battery = Ptr UniformGenerator -> BatteryResult
data TestResult = Fail | Suspect | OK deriving (Eq, Show)

type WrappedCallback = FunPtr (IO CUInt)

foreign import ccall unsafe "unif01_CreateExternGenBits" c_createGenerator :: CString -> FunPtr (IO CUInt) -> IO (Ptr UniformGenerator)
foreign import ccall unsafe "unif01_DeleteExternGenBits" c_deleteGenerator :: Ptr UniformGenerator -> IO ()
foreign import ccall unsafe "wrapper" mkCallback :: IO CUInt -> IO WrappedCallback

defaultGeneratorName = "supplied_generator"

intToCUInt :: Int -> CUInt
intToCUInt = CUInt . fromIntegral

streamToCallback :: WrappedGen -> IO WrappedCallback
streamToCallback = mkCallback . fmap intToCUInt

genToCallback :: RandomGen g => (g -> RandomStream) -> g -> IO WrappedCallback
genToCallback streamer gen = wrapForPassing streamer gen >>= streamToCallback

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
  callback <- genToCallback streamer gen
  return $ runBatteryOnCallback callback crush
{-# NOINLINE runBattery #-}

runBatteryOnStream :: IO Int -> Battery -> [Double]
runBatteryOnStream gen crush = unsafePerformIO $ do
  callback <- streamToCallback gen
  return $ runBatteryOnCallback callback crush
{-# NOINLINE runBatteryOnStream #-}

runBatteryOnCallback :: WrappedCallback -> Battery -> [Double]
runBatteryOnCallback callback crush = unsafePerformIO $ do
  marshalledName <- newCString defaultGeneratorName
  generatorPtr <- c_createGenerator marshalledName callback
  batteryResult <- crush generatorPtr
  (BR pValues testNumber) <- peek batteryResult
  c_deleteGenerator generatorPtr
  freeHaskellFunPtr callback
  free marshalledName
  free batteryResult
  cDoublePvalues <- peekArray (fromIntegral testNumber) pValues
  return $ map realToFrac cDoublePvalues
{-# NOINLINE runBatteryOnCallback #-}

foreign import ccall safe "bbattery_SmallCrush" c_smallCrush :: Battery
foreign import ccall safe "bbattery_Crush" c_crush :: Battery
foreign import ccall safe "bbattery_BigCrush" c_bigCrush :: Battery
foreign import ccall safe "bbattery_pseudoDIEHARD" c_pseudoDIEHARD :: Battery