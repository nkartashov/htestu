module Test.HTestU.Wrapping
  (WrappedGen,
  wrapForPassing) where

import System.Random (RandomGen)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import Test.HTestU.Streaming (RandomStream)

type WrappedGen = IO Int

wrapForPassing :: RandomGen g => (g -> RandomStream) -> g  -> IO WrappedGen
wrapForPassing streamer gen = newIntStreamGen >>= return . nextIntFromStreamRef
  where newIntStreamGen = newIORef $ streamer gen

nextIntFromStreamRef :: IORef RandomStream -> WrappedGen
nextIntFromStreamRef streamRef = do
  (newRandomBits : newStream) <- readIORef streamRef
  writeIORef streamRef newStream
  return newRandomBits
