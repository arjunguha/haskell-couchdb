#!/bin/bash

if [ `hostname` = "wanderlust.local" ]; then
  open /Users/arjun/local/share/doc/CouchDB-0.8.0/html/index.html
elif [ `hostname` = "peabody" ]; then
 firefox /pro/plt/ghc/latest/share/doc/CouchDB-0.8.0.1/html/index.html &
else
  echo "ERROR: Don't know how to build on " `hostname`
fi
