# kool - Distributed Cached C++ Compilation

distcc-style distributed C++ compilation with integrated object file cache.

This project is a work-in-progress and aims to provide an easy-to-use and understandable alternative to distcc.

## Flow

* User invokes g++ -o file.o file.cc
* kool intercepts the invoction and parses the arguments
* Local g++ is used to preprocess the source file
* Tries to reserve a build slot on any of the available build nodes (by supplying a hash of source and flags)
* If the hash is a cache hit, an object file is returned, and compilation is done
* Otherwise submits the source with all flags to be built
* Client retrieves the result and saves the object file locally

## Ideas

The hash->object_file cache will be per-build node to begin with. This can be improved in the future.
Support for multiple g++ versions would be great, and this structure doesn't require multiple chroots.
