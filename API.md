Currently, very much a work in progress.

# Protocol

The API can be accessed as:
- a "regular" gen_server (for Erlang clients, or others that can implement a distributed node)
- a TCP endpoint, using Erlang terms in external format (or UBF) as data
- (not yet) a HTTP endpoint, using REST to access the functionality

