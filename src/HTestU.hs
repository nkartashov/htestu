{-# LANGUAGE ForeignFunctionInterface #-}

module HTestU
 (runBattery,
 c_smallCrush,
 c_crush,
 c_bigCrush) where
import System.Random (RandomGen)

import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CUInt(..))
import Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr)

import HTestU.Wrapping (wrapForPassing)
import HTestU.Streaming (RandomStream)

data UniformGenerator
type BatteryResult = IO (Ptr ())
type Battery = Ptr UniformGenerator -> BatteryResult

foreign import ccall safe "unif01_CreateExternGenBits" c_createGenerator :: CString -> FunPtr (IO CUInt) -> IO (Ptr UniformGenerator)
foreign import ccall safe "unif01_DeleteExternGenBits" c_deleteGenerator :: Ptr UniformGenerator -> IO ()
foreign import ccall safe "wrapper" mkCallback :: IO CUInt -> IO (FunPtr (IO CUInt))

defaultGeneratorName = "supplied_generator"

intToCUInt :: Int -> CUInt
intToCUInt = CUInt . fromIntegral

genToCallback :: RandomGen g => (g -> RandomStream) -> g -> IO (FunPtr (IO CUInt))
genToCallback streamer = mkCallback . (fmap intToCUInt) . wrapForPassing streamer

runBattery :: RandomGen g => (g -> RandomStream) -> g -> Battery -> BatteryResult
runBattery streamer gen crush = do
  callback <- genToCallback streamer gen
  marshalledName <- newCString defaultGeneratorName
  generatorPtr <- c_createGenerator marshalledName callback
  batteryResult <- crush generatorPtr
  c_deleteGenerator generatorPtr
  freeHaskellFunPtr callback
  return batteryResult

foreign import ccall safe "bbattery_SmallCrush" c_smallCrush :: Battery
foreign import ccall safe "bbattery_Crush" c_crush :: Battery
foreign import ccall safe "bbattery_BigCrush" c_bigCrush :: Battery
foreign import ccall safe "bbattery_pseudoDIEHARD" c_prseudoDIEHARD :: Battery
