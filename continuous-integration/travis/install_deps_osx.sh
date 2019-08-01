#!/bin/sh -x

echo "Installing Wake"

export USE_FUSE_WAKE=0
git clone --branch mac-use-fuse-wake https://github.com/sifive/wake.git
make -C wake
export PATH=$PATH:$PWD/wake/bin


echo "Installing Wit"

git clone https://github.com/sifive/wit.git
export PATH=$PATH:$PWD/wit
