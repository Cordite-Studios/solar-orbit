{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | The basic types used in an Orbit machine
module Solar.Orbit.Types where

import qualified  Data.Text as T
import qualified  Data.Sequence as S
import qualified  Data.ByteString as B
import            Data.Typeable
import            Data.Generics

-- | Alias for textual types
type OrbiterText = T.Text

-- | Name for a log or human readable identifier
type OrbiterLogName = T.Text

-- | Prority of a log, in case the runner filters
data Priority
  = Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert
  | Emergency
  deriving (Show, Eq, Enum, Typeable, Data)

-- | an ADT for most configurations (1,2,n)
data OrbiterClimate a
  = OrbiterChain (S.Seq a)
  | OrbiterSingle a
  | OrbiterPair a a
  deriving (Show, Eq, Typeable, Data)

-- | Climate for names
type OrbiterCName = OrbiterClimate OrbiterText
-- | Climates for binary keys
type OrbiterCBinary = OrbiterClimate B.ByteString

-- | Similar to Either, but not with Left being considered negative
data OrbiterClimates
  = OrbitNamed OrbiterCName
  | OrbitBinary OrbiterCBinary
  deriving (Show, Eq, Typeable, Data)

-- | Shortcut for what 'S.Serialize' has when parsing
-- Only to be used when reading from a log, not when getting
-- a message.
type OrbitTry a = Either String a

-- | Message that comes from another 'Orbiter'.
-- The runner provides the 'dest' destinations, and
data Message sys dest a = Message
  { msgAddress :: Maybe dest
  -- ^ Runner provided, may be 'Nothing' if meant for the runner.
  , msgIndirectAddress :: Maybe dest
  -- ^ In case this message was recieved as if by proxy
  , msgBody :: Either sys a
  -- ^ 'sys' is Runner provided.
  -- 'a' is per this 'Orbiter's expected data type.
  }
  deriving (Show, Typeable, Data)

-- | Describes what happened when trying to rename a log.
data ReplaceStatus
  = RenameSuccess
  | ReplaceSuccess
  | LogNotFound
  deriving (Show, Eq, Typeable, Data)
