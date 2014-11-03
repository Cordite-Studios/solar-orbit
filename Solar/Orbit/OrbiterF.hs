{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE GADTs #-}

-- | A GADT to use in a Free Monad as the data structure
module Solar.Orbit.OrbiterF where

import            Data.Int
import qualified  Data.Serialize as S
import qualified  Data.ByteString as B
import qualified  Data.Text as T
import            Solar.Orbit.Types


-- | 'Free' 'Monad' specification for an 'Orbiter'
data OrbiterF sys dest time context next where
  -- Current Info
  GetContext  :: (context -> next)
              -> OrbiterF sys dest time context next

  GetTime     :: (time -> next)
              -> OrbiterF sys dest time context next

  -- File System
  -- - Returns
  LogExists   :: OrbiterLogName
              -> (Bool -> next)
              -> OrbiterF sys dest time context next

  LogDelete   :: OrbiterLogName
              -> (Bool -> next)
              -> OrbiterF sys dest time context next

  TellLogPosition :: OrbiterLogName
                  -> (Int64 -> next)
                  -> OrbiterF sys dest time context next

  TellLogSize :: OrbiterLogName
              -> (Int64 -> next)
              -> OrbiterF sys dest time context next

  CheckSumLog :: OrbiterLogName
              -> (B.ByteString -> next)
              -> OrbiterF sys dest time context next

  -- - Returns and has an effect
  ReadFromLog :: (S.Serialize a)
              => OrbiterLogName
              -> (OrbitTry a -> next)
              -> OrbiterF sys dest time context next

  RenameLog   :: OrbiterLogName
              -> OrbiterLogName
              -> (ReplaceStatus -> next)
              -> OrbiterF sys dest time context next

  -- - Effect only
  AppendLogData   :: (S.Serialize a)
                  => OrbiterLogName
                  -> a
                  -> next
                  -> OrbiterF sys dest time context next

  ResetLogPosition:: OrbiterLogName
                  -> next
                  -> OrbiterF sys dest time context next

  SeekLogAhead    :: OrbiterLogName
                  -> Int
                  -> next
                  -> OrbiterF sys dest time context next

  -- Communication
  ReceiveMessage  :: (S.Serialize a)
                  => (Message sys dest a -> next)
                  -> OrbiterF sys dest time context next

  SendMessage     :: (S.Serialize a)
                  => Message sys dest a
                  -> next
                  -> OrbiterF sys dest time context next
  -- Extras
  LogEmit     :: Priority
              -> T.Text
              -> next
              -> OrbiterF sys dest time context next

-- https://hadoop.apache.org/docs/current/api/org/apache/hadoop/fs/FileSystem.html
-- Look at this a bit.

-- | Needed for Free to work properly
instance Functor (OrbiterF sys dest time context) where
  fmap f (GetContext c) = GetContext (f . c)
  fmap f (GetTime t) = GetTime (f . t)
  fmap f (LogExists n b) = LogExists n (f . b)
  fmap f (LogDelete n b) = LogDelete n (f . b)
  fmap f (TellLogPosition n p) = TellLogPosition n (f . p)
  fmap f (TellLogSize n p) = TellLogSize n (f . p)
  fmap f (CheckSumLog n c) = CheckSumLog n (f . c)
  fmap f (ReadFromLog n d) = ReadFromLog n (f . d)
  fmap f (RenameLog n n' r) = RenameLog n n' (f . r)
  fmap f (AppendLogData n d ne) = AppendLogData n d (f ne)
  fmap f (ResetLogPosition n e) = ResetLogPosition n (f e)
  fmap f (SeekLogAhead n d e) = SeekLogAhead n d (f e)
  fmap f (ReceiveMessage m) = ReceiveMessage (f . m)
  fmap f (SendMessage m n) = SendMessage m (f n)
  fmap f (LogEmit p t n) = LogEmit p t (f n)
