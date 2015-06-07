module HTestU.Wrapping
  (WrappedGen,
  wrapForPassing) where

import System.Random (RandomGen)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import HTestU.Streaming (RandomStream)

type WrappedGen = IO Int

wrapForPassing :: RandomGen g => (g -> RandomStream) -> g  -> WrappedGen
wrapForPassing streamer gen = nextIntFromStreamRef newIntStreamGen
  where newIntStreamGen = unsafePerformIO $ newIORef $ streamer gen

nextIntFromStreamRef :: IORef RandomStream -> WrappedGen
nextIntFromStreamRef streamRef = do
  (newRandomBits : newStream) <- readIORef streamRef
  writeIORef streamRef newStream
  return newRandomBits
