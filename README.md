Generic Erlang engine for source code indexing and manipulation, that can be used by code editors and IDES to provide advanced functionality. This functionality will be accessible via VSCode's [Language Server Protocol](https://github.com/Microsoft/language-server-protocol) and [Debug Protocol](https://code.visualstudio.com/docs/extensionAPI/api-debugging), possibly with some extensions. The `server` application is the endpoint.


# Important note

**The content of this repository is highly volatile at the moment**. I am working on stabilizing it (structure, release engineering, APIs) and I will remove this notice when it is safe to use by third parties.

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

## Notes

* The OTP version specified in the rebar.config files for each subproject MUST be used for compilation. The `build` script installs those using `kerl` in `~/erlide_tools` (in the future the location should be configurable).
* As transport for the communication protocols we will implement TCP and stdio, but others are possible.
