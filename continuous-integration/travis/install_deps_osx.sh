#!/bin/sh
set -ex

echo "Installing Wake"

export USE_FUSE_WAKE=0
git clone https://github.com/sifive/wake.git
git -C wake checkout v0.15.1
make -C wake
export PATH=$PATH:$PWD/wake/bin


echo "Installing Wit"

git clone https://github.com/sifive/wit.git
export PATH=$PATH:$PWD/wit
