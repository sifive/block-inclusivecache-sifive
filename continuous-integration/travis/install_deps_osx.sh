#!/bin/sh

echo "Installing Wake"

git clone https://github.com/sifive/wake.git
cd wake
git checkout v0.15.1
make && ./bin/wake 'install "/usr/local"'
cd ..

echo "Installing Wit"

git clone https://github.com/sifive/wit.git
export PATH=$PATH:$PWD/wit
