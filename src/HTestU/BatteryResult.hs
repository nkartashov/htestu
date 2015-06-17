{-# LINE 1 "BatteryResult.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "BatteryResult.hsc" #-}
{-# LANGUAGE CPP                      #-}

module HTestU.BatteryResult
( BatteryResultStruct(..)
)
  where

import Foreign.C.Types (CInt(..), CDouble(..))
import Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr)
import Foreign.Storable


{-# LINE 14 "BatteryResult.hsc" #-}

data BatteryResultStruct = BR {
  pValues :: Ptr CDouble,
  testNumber :: CInt
  } deriving (Eq, Show)


{-# LINE 21 "BatteryResult.hsc" #-}

instance Storable BatteryResultStruct where
  sizeOf _ = ((16))
{-# LINE 24 "BatteryResult.hsc" #-}
  alignment _ = 8
{-# LINE 25 "BatteryResult.hsc" #-}
  peek ptr = do
    pValues <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 27 "BatteryResult.hsc" #-}
    testNumber <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 28 "BatteryResult.hsc" #-}
    return $ BR pValues testNumber

  poke ptr (BR pValues testNumber) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr pValues
{-# LINE 32 "BatteryResult.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr testNumber
{-# LINE 33 "BatteryResult.hsc" #-}

