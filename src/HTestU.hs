{-# LANGUAGE ForeignFunctionInterface #-}

module HTestU
( TestResult,
  runBattery,
  runBatteryToResults,
  c_smallCrush,
  c_crush,
  c_bigCrush) where
import System.Random (RandomGen)

import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CUInt(..))
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr)
import Foreign.Storable

import System.IO.Unsafe (unsafePerformIO)

import HTestU.Wrapping (wrapForPassing)
import HTestU.Streaming (RandomStream)
import HTestU.BatteryResult (BatteryResultStruct(..))

data UniformGenerator
type BatteryResult = IO (Ptr BatteryResultStruct)
type Battery = Ptr UniformGenerator -> BatteryResult
data TestResult = Fail | Suspect | OK deriving (Eq, Show)

foreign import ccall safe "unif01_CreateExternGenBits" c_createGenerator :: CString -> FunPtr (IO CUInt) -> IO (Ptr UniformGenerator)
foreign import ccall safe "unif01_DeleteExternGenBits" c_deleteGenerator :: Ptr UniformGenerator -> IO ()
foreign import ccall safe "wrapper" mkCallback :: IO CUInt -> IO (FunPtr (IO CUInt))

defaultGeneratorName = "supplied_generator"

intToCUInt :: Int -> CUInt
intToCUInt = CUInt . fromIntegral

genToCallback :: RandomGen g => (g -> RandomStream) -> g -> IO (FunPtr (IO CUInt))
genToCallback streamer = mkCallback . (fmap intToCUInt) . wrapForPassing streamer

failurePvalue = 0.0000000001 -- 10^(-10)
suspectPvalue = 0.001 -- 10^(-3)

pValueToResult :: Double -> TestResult
pValueToResult pvalue | pvalue < failurePvalue || pvalue > 1.0 - failurePvalue = Fail
                      | pvalue < suspectPvalue || pvalue > 1.0 - suspectPvalue = Suspect
                      | otherwise = OK

runBatteryToResults :: RandomGen g => (g -> RandomStream) -> g -> Battery -> [TestResult]
runBatteryToResults = ((map pValueToResult .) .) . runBattery

-- returns [Double] instead of IO [Double], because it is transparent (for the same gen result should be the same)
runBattery :: RandomGen g => (g -> RandomStream) -> g -> Battery -> [Double]
runBattery streamer gen crush = unsafePerformIO $ do
  callback <- genToCallback streamer gen
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

foreign import ccall safe "bbattery_SmallCrush" c_smallCrush :: Battery
foreign import ccall safe "bbattery_Crush" c_crush :: Battery
foreign import ccall safe "bbattery_BigCrush" c_bigCrush :: Battery
foreign import ccall safe "bbattery_pseudoDIEHARD" c_prseudoDIEHARD :: Battery
