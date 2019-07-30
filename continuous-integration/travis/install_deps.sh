#!/bin/sh
set -ex

echo "Manually installing re2"

git clone https://code.googlesource.com/re2
cd re2
make
make test
make install
make testinstall
cd ..
