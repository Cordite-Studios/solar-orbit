{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Mostly leftovers that may be used to fulfill the types that Orbiter needs
module Solar.Orbit.Base where


import            Data.Word
import            Data.Int
import            Data.Typeable
import            Data.Generics
import qualified  Data.Map as M
import qualified  Data.ByteString as B
import            Solar.Orbit.Types

-- | Contexts may provide a hint of the hierarchy
-- that this 'Orbiter' is in.
data OrbiterContext = OrbiterContext
  { logicalHierarchy  :: M.Map OrbiterName OrbiterClimates
  -- ^ Chain of identiifers that can be used to possibly communicate to other
  -- Orbiters logically.
  , assignedPosition  :: OrbiterClimate B.ByteString
  -- ^ A binary-identifier that may determine where this Orbiter is or
  -- what this Orbiter owns
  }

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
