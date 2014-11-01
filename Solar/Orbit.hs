{-# OPTIONS_HADDOCK show-extensions #-}


{-| 'Orbiter's can be used to create small disposeable state machines,
which have access to an abstraction of an append-only file system.

Each 'Orbiter' has an isolated set of logs which are not shared
with other 'Orbiter's.
An 'Orbiter' may, accourding to the runner, message another 'Orbiter'
with a different context.

Runners also provide 'sys' messages (usually in the 'Left' of 'Either')
for things like:

> Got message from X, but couldn't deserialize

'sys' messages may also be used to tell that things are shutting down
so take appropriate actions.

'Orbiter's are __disposeable__.
Ideally an 'Orbiter' will only be /disposed/ when it is
waiting on 'receiveMessage'.
An 'Orbiter' can keep local state by recursing with the next state value.
However, local state should only be used for efficiency reasons,
as the runner has the right to drop 'Orbiter's when clearing resources.

It is suggested to always test to make sure that Orbiter behavior on
'sendMessage' and 'receiveMessage' remain the same if an Orbiter is disposed
on the subsequent 'receiveMessage'.

Communication between 'Orbiter's should not be considered reliable.
The intention of "Solar.Orbit" is for 'Orbiter's to communicate with other
'Orbiter's which may not be within the same /Solar/ system.

If a requirement is to communicate with an orbiter on the same system,
it is suggested to send a 'sys' message to the runner.

-}
module Solar.Orbit where

import Solar.Orbit.Types
import Solar.Orbit.Orbiter
