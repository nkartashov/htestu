{-# LANGUAGE ForeignFunctionInterface #-}
module Test.HTestU.Wrapping
  ( Battery
  , WrappedCallback
  , c_createGenerator
  , c_deleteGenerator
  , genToWrappedCallback
  , streamToWrappedCallback
  , randomStreamActionToWrappedCallback) where

import System.Random (RandomGen)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr)
import Foreign.C.Types (CUInt(..))
import Foreign.Marshal.Array (pokeArray)

import Test.HTestU.Streaming (RandomStream)
import Test.HTestU.BatteryResult (BatteryResultStruct(..))

data UniformGenerator
type BatteryResult = IO (Ptr BatteryResultStruct)
type Battery = Ptr UniformGenerator -> BatteryResult

type Callback = CUInt -> Ptr CUInt -> IO ()
type WrappedCallback = FunPtr Callback

foreign import ccall unsafe "unif01_CreateExternGenBits" c_createGenerator :: WrappedCallback -> IO (Ptr UniformGenerator)
foreign import ccall unsafe "unif01_DeleteExternGenBits" c_deleteGenerator :: Ptr UniformGenerator -> IO ()
foreign import ccall unsafe "wrapper" mkCallback :: Callback -> IO WrappedCallback

genToWrappedCallback :: RandomGen g => (g -> RandomStream) -> g -> IO WrappedCallback
genToWrappedCallback streamer gen = streamToWrappedCallback $ streamer gen

streamToWrappedCallback :: RandomStream -> IO WrappedCallback
streamToWrappedCallback stream = wrapForPassing stream >>= mkCallback

randomStreamActionToWrappedCallback :: IO RandomStream -> IO WrappedCallback
randomStreamActionToWrappedCallback action = action >>= streamToWrappedCallback

wrapForPassing :: RandomStream -> IO Callback
wrapForPassing stream = newIORef stream >>= return . nextIntFromStreamRef

nextIntFromStreamRef :: IORef RandomStream -> Callback
nextIntFromStreamRef streamRef (CUInt values) arrayPtr = do
  randomNumbers <- readIORef streamRef
  let (toDump, rest) = splitAt (fromIntegral values) randomNumbers
  writeIORef streamRef rest
  pokeArray arrayPtr $ map fromIntegral toDump
