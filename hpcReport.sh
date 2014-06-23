#!/bin/bash

set -e

if [[ ! ( -e HList.cabal && -e dist/hpc/tix/properties/properties.tix ) ]]; then
  echo "run in the same dir as HList.cabal after having run

      cabal configure --enable-tests --enable-library-coverage; cabal test examples properties

        "
  exit 1
fi


propsExclude=$(find examples/ -name '*.hs' \
        | sed -e 's_/_._g' -e 's_.hs$__' -e 's_^tests._--exclude=_' )

hpcFlags="
  --hpcdir=dist/hpc/mix/
  --exclude=Main
  dist/hpc/tix/properties/properties.tix
  "


if [[ ! (-e dist/hpc/mix/Main.mix) ]]; then
  mv dist/hpc/mix/properties/* dist/hpc/mix/
  mv dist/hpc/mix/examples/* dist/hpc/mix/
## for whatever reason hpc does not like doctests
## and it seems that examples do not contribute
#  mv dist/hpc/mix/doctests/* dist/hpc/mix/
  mv dist/hpc/mix/HList-*/HList-*/* dist/hpc/mix/HList-*/
fi


hpc markup --destdir=dist/hpc $hpcFlags > /dev/null
echo "see dist/hpc/hpc_index.html
"
hpc report $hpcFlags
