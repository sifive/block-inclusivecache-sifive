#!/bin/sh -x

echo "Installing Wake"

git clone --branch old-osx https://github.com/sifive/wake.git
make -C wake
export PATH=$PATH:$PWD/wake/bin


echo "Installing Wit"

git clone https://github.com/sifive/wit.git
export PATH=$PATH:$PWD/wit
