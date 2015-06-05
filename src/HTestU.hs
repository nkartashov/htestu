{-# LANGUAGE ForeignFunctionInterface #-}

module HTestU where

import System.Random

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import HTestU.Wrapping

data UniformGenerator
foreign import ccall safe "unif01_CreateExternGenBits" c_createGenerator :: CString -> FunPtr (IO CUInt) -> IO (Ptr UniformGenerator)
foreign import ccall safe "unif01_DeleteExternGenBits" c_deleteGenerator :: Ptr UniformGenerator -> IO ()
foreign import ccall safe "wrapper" mkCallback :: IO CUInt -> IO (FunPtr (IO CUInt))

generatorName = "supplied_generator"

makeTestUGenerator :: RandomGen g => g -> IO (Ptr UniformGenerator)
makeTestUGenerator g = do
  callback <- wrappedGenToCallback g
  marshalledName <- newCString generatorName
  c_createGenerator marshalledName callback

-- TODO: free when not needed
wrappedGenToCallback :: RandomGen g => g -> IO (FunPtr (IO CUInt))
wrappedGenToCallback = mkCallback . (fmap intToCUInt) . wrapForPassing intStreamFromRandomGen

intToCUInt :: Int -> CUInt
intToCUInt = CUInt . fromIntegral

foreign import ccall safe "bbattery_SmallCrush" c_runSmallCrush :: Ptr UniformGenerator -> IO ()

runSmallCrush :: RandomGen g => g -> IO ()
runSmallCrush g = do
  testUGenerator <- makeTestUGenerator g
  c_runSmallCrush testUGenerator
  c_deleteGenerator testUGenerator
