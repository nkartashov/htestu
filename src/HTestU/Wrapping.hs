module HTestU.Wrapping where

import System.Random
import Data.IORef
import System.IO.Unsafe

wrapForPassing :: RandomGen g => g -> IO Int
wrapForPassing gen = nextIntFromGenRef newRandomGenRef
  where newRandomGenRef = unsafePerformIO $ newIORef gen

nextIntFromGenRef :: RandomGen g => IORef g -> IO Int
nextIntFromGenRef genRef = do
  oldGen <- readIORef genRef
  let (newRandomBits, newGen) = next oldGen
  writeIORef genRef newGen
  return newRandomBits
