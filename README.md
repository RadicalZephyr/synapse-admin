# Synapse-Admin

A Clojure library designed to provide a desktop application for doing
basic administrative tasks on [Synapse], as well as being able to
gather some statistics about Synapse.

[Synapse]: http://synapse.sagebase.org

## Usage

Currently the GUI portion will only popup a login window.  However,
from the repl the library can be used to generate some statistics
about Synapse (must use an administrative account).

The key functions to look at are synapse-stats, which should be fed
from the functions get-all-projects, get-all-profiles and
get-all-project-acls.  These are high through-put calls to Synapse
that should probably be throttled, so don't try to call them all at
once.

## License

Copyright © 2012 Geoff Shannon

Distributed under the Eclipse Public License, the same as Clojure.
