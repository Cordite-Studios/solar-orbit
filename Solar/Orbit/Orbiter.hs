{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Provides the Orbiter Free Monad functions to write Orbit machines
module Solar.Orbit.Orbiter where

import            Control.Monad.Free
import            Solar.Orbit.Types
import            Solar.Orbit.OrbiterF
import qualified  Data.Serialize as S
import qualified  Data.ByteString as B
import qualified  Data.Text as T
import            Data.Int

-- | Type alias for OrbiterF within the 'Free' 'Monad'.
-- Use this when writing 'Orbiter's.
type Orbiter sys dest time context = Free (OrbiterF sys dest time context)

-- | Shorthand typeclass alias via ConstraintKinds extention
type OMF sys dest time context m = MonadFree (OrbiterF sys dest time context) m
-- | Same as 'OMF', just with Serialize too.
type OMFS sys dest time context a m =
  (S.Serialize a,
   MonadFree (OrbiterF sys dest time context) m)

-- | Yields an identifier chain that can be used to determine the logical
-- hierarchy of the current state machine and others it may try to contact
getContext :: OMF s d t c m => m c
getContext = liftF $ GetContext id

-- | Gives the time according to the runner
getTime :: OMF s d t c m => m t
getTime = liftF $ GetTime id

-- | Checks to see if a log, by name, currently exists in the append-only
-- log system
logExists :: OMF s d t c m => OrbiterName -> m Bool
logExists name = liftF $ LogExists name id

-- | Operation to delete a log. Will return True on successful deletion
logDelete :: OMF s d t c m => OrbiterName -> m Bool
logDelete name = liftF $ LogDelete name id

-- | Gives the log's current position in case to reference it elsewhere
tellLogPosition :: OMF s d t c m => OrbiterName -> m Int64
tellLogPosition name = liftF $ TellLogPosition name id

-- | Gives the count of bytes from beginning to end of the log by name
tellLogSize :: OMF s d t c m => OrbiterName -> m Int64
tellLogSize name = liftF $ TellLogSize name id

-- | Gives a checksum as calculated by the runner
checkSumLog :: OMF s d t c m => OrbiterName -> m B.ByteString
checkSumLog name = liftF $ CheckSumLog name id

-- | Attempts to read a Serializeable data type from the log.
-- Will not advance the log position if it fails.
-- A failure will always occur if the log does not have enough
-- bytes for what is trying to be read, or if there are zero bytes
-- left to process.
readFromLog :: OMFS s d t c a m => OrbiterName -> m (OrbitTry a)
readFromLog name = liftF $ ReadFromLog name id

-- | Atomically renames the log by name to another name.
renameLog :: OMF s d t c m => OrbiterName -> OrbiterName -> m ReplaceStatus
renameLog name name' = liftF $ RenameLog name name' id

-- | Atomically appends a Serializeable data type to a log.
-- The read position does not affect where we write.
-- The read position is not affected.
appendLogData :: OMFS s d t c a m => OrbiterName -> a -> m ()
appendLogData name content = liftF $ AppendLogData name content ()

-- | Seeks the log back to the start
resetLogPosition :: OMF s d t c m => OrbiterName -> m ()
resetLogPosition name = liftF $ ResetLogPosition name ()

-- | Seeks the log ahead by the byte count.
-- If we hit the end, we silently stay at the end.
seekLogAhead :: OMF s d t c m => OrbiterName -> Int -> m ()
seekLogAhead name jump = liftF $ SeekLogAhead name jump ()

-- | A blocking operation that recieves a typed message.
-- The runner provides the message contents with the source / destination.
-- The Orbiter __/may/__ be disposed of while waiting here
-- and lose all state accrued.
receiveMessage :: OMFS s d t c a m => m (Message s d a)
receiveMessage = liftF $ ReceiveMessage id

-- | Sends a message out via the runner to the source / destination.
sendMessage ::OMFS s d t c a m => Message s d a -> m ()
sendMessage message = liftF $ SendMessage message ()

-- | Emits a log entry to the log. Mostly for debugging.
logEmit :: OMF s d t c m => Priority -> T.Text -> m ()
logEmit pri output = liftF $ LogEmit pri output ()
