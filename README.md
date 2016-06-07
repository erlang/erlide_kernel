Erlang engine for source code indexing and manipulation, that can be used by code editors and IDES to provide advanced .

Different IDEs implement clients that use this kernel to do the real work. Currently, only an Eclipse client is implemented (see the [erlide_eclipse](https://github.com/erlide/erlide_eclipse) repository).

# Important note

**The content of this repository is highly volatile at the moment**. I am working on stabilizing it (structure and release engineering) and will remove this notice when it is safe to use by third parties.

Initially, the content is the same as it was in the erlide repository.

# Description (vision)

The erlide kernel will implement the core functionality of an IDE:
- keep track of projects and their respective files, watch or get notified of any changes
- parse the code while keeping track of the exact source text and the dependencies between files and projects
- build a database of the projects' entities
- expose APIs to query and update this database, for services such as
  - syntactic and semantic code annotations (to be used by highlighting editors, for example)
  - code completion
  - cross references (declarations, references, call graphs)
  - extracting documentation
  - refactoring
- read/edit the projects' build configuration and launch build tools
- launch the projects' code and interact with it (debug, trace, profile)
- launch, examine and interact with Erlang nodes

The API will be accessible via the network, using serialized Erlang terms, but in the future a REST API could be useful (for clients that don't talk Erlang). Parts of the API will be asynchronous, so that progress information and partial results can be returned to the client when available; these parts will be marked as such.

The API is meant to be generic i.e. without Eclipse-related leaking abstractions, but we might borrow some of the concepts (like for example, the workspace as a container for several projects).

## Notes

* The OTP version specified in the rebar.config files for each subproject MUST be used for compilation.
