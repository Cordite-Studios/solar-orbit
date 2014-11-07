{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Mostly leftovers that may be used to fulfill the types that Orbiter needs
module Solar.Orbit.Base where


import            Data.Word
import            Data.Int
import            Data.Typeable
import            Data.Generics

-- | Genericized time data structure, in case you do not use
-- a direct method like year, day, hours, minutes or such.
data OrbiterTime = OrbiterTime
  { obtYear :: Int16
  , obtDay  :: Word16
  , obtUnit :: Word16
  , obtSub1 :: Word8
  , optSub2 :: Word8
  }
  deriving (Show, Eq, Typeable, Data)
