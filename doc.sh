#!/bin/bash

if [ `hostname` = "wanderlust.local" ]; then
  open /Users/arjun/local/share/doc/CouchDB-0.8.0.4/html/index.html
elif [ `hostname` = "peabody" ]; then
 firefox /pro/plt/ghc/latest/share/doc/CouchDB-0.8.0.4/html/index.html &
else
  echo "ERROR: Don't know how to build on " `hostname`
fi
