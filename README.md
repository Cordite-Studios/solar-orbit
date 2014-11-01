# Solar: Orbit

Orbit is a DSL for solar systems.
An Orbiter is a disposable state machine that is written in Orbit.

## Principles
Orbiters have access to an append-only "file system".
It is assumed that Orbiters have unique contexts and their respective logs
are not shared with any other Orbiter.

Orbiters are similar to actors, in that they can concurrently run and
communicate exclusively through messaging.
A major difference is that Orbiters cannot be created--at least by another
Orbiter.

## Conceptual example

It is up to the runner on how to deliver the message to the
requested destination.
For example, suppose there is an Orbiter defined to handle "comments".
Let us also imagine there is a set of blog posts, where each blog post has
an identifier.
Then, one may assign an orbiter with the context of "comments" and the
blog post identifier.

Inside of our defined Orbiter, we assume that the context never changes
for a given running Orbiter and we can store, retrieve, index, or otherwise
support domain operations for comments on a particular blog post--given that
the blog post identifier is part of the context.
Then, whenever a new comment comes in (via some other Orbiter or service
that communicates with the runner), that comment can be directed to the
respective Orbiter based upon the blog post identifier.
Additionally, the "comments" orbiter may keep state in memory to reduce I/O
to react to incoming messages.

However, if our runner limits to some count of live Orbiters or watches the
memory usage, any Orbiters which are waiting on a potentially indefinitely
blocking operation, namely `receiveMessage`, that Orbiter is candidate for
eviction and garbage collection without further Orbiter processing.

*This seems to follow along the philosophy of 'crash-only software'.*

## Motivation

Solar Orbit is made as a part to implement distributed systems
(which may exist in the same or multiple processes or hosts).
With distributed systems being notoriously challenging for creating robust
dependable and simple software, a DSL that can be executed both for
production use and for in-memory testing as well as chaos testing may be a
useful tool to finding and resolving issues in the software implementation.

Solar Orbit is designed to have Orbiters compiled as their own libraries,
which ideally can be loaded at run-time on a production runner.
This can be accomplished with a tool like
[hint](http://hackage.haskell.org/package/hint)
with the Orbiters already compiled, the runner compiled, and with minimal
interpretation overhead to combine them at run time.
It is also designed such that executing Orbiters may be done entirely within
memory and with a random seed such that problems can be reproduced efficiently
for analyzation and correction.

Further, I believe that [conduits](http://hackage.haskell.org/package/conduit)
may be used to efficiently implement runners, since `yield` and `await`
map to `sendMessage` and `receiveMessage` respectively.
Also, many of the actions or requests can also be implemented similarly
for processing via conduit.

## Special thanks

Special thanks to the following people for their help in my understanding
or implementation of Solar Orbit.

* [Samuel Gélineau](https://github.com/gelisam)
