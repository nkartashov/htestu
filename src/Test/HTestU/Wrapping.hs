{-# LANGUAGE ForeignFunctionInterface #-}
module Test.HTestU.Wrapping
  ( Battery
  , WrappedCallback
  , c_createGenerator
  , c_deleteGenerator
  , genToWrappedCallback
  , streamToWrappedCallback
  , randomStreamActionToWrappedCallback
  ) where

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

-- | Function which fills the array with numbers
-- given size and a pointer to it
type Callback = CUInt -> Ptr CUInt -> IO ()

-- | Wrapped 'Callback' function for calling from C
type WrappedCallback = FunPtr Callback

-- | C function allocating a PRNG on C side, given a callback wrapping it
foreign import ccall safe "unif01_CreateExternGenBits" c_createGenerator :: WrappedCallback -> IO (Ptr UniformGenerator)

-- | Complimentary deallocating function to 'c_createGenerator'
foreign import ccall unsafe "unif01_DeleteExternGenBits" c_deleteGenerator :: Ptr UniformGenerator -> IO ()

-- | Wrapper function for porting callback to C
foreign import ccall unsafe "wrapper" mkCallback :: Callback -> IO WrappedCallback

-- | Wraps the given RandomGen into a C callback
genToWrappedCallback :: RandomGen g => (g -> RandomStream) -> g -> IO WrappedCallback
genToWrappedCallback streamer gen = streamToWrappedCallback $ streamer gen

-- | Wraps an stream of numbers into a C callback
streamToWrappedCallback :: RandomStream -> IO WrappedCallback
streamToWrappedCallback stream = newIORef stream >>= mkCallback . nextIntFromStreamRef

-- | Wraps an stream of numbers produced by a stream of numbers into a C callback
randomStreamActionToWrappedCallback :: IO RandomStream -> IO WrappedCallback
randomStreamActionToWrappedCallback action = action >>= streamToWrappedCallback

-- | Takes an allocated reference to a stream of numbers produces a Callback
-- for passing to C code
nextIntFromStreamRef :: IORef RandomStream -> Callback
nextIntFromStreamRef streamRef (CUInt values) arrayPtr = do
  randomNumbers <- readIORef streamRef
  let (toDump, rest) = splitAt (fromIntegral values) randomNumbers
  writeIORef streamRef rest
  pokeArray arrayPtr $ map fromIntegral toDump
