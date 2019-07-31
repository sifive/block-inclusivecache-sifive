#!/bin/sh -x

echo "Install Wit/Wake"

export PATH=$PATH:$PWD/repos/wit

export USE_FUSE_WAKE=0
git clone https://github.com/sifive/wake.git
cd wake
git checkout v0.15.1
make
export PATH=$PATH:$PWD/bin

echo "Initialize Workspace"

cd ..
wit --repo-path $PWD/repos init workspace -a block-inclusivecache-sifive
cd workspace/
wit add-pkg https://github.com/sifive/environment-example-sifive.git
wit update

wake --init .

echo "Compile Scala"

wake -j1 -v compileScalaModule inclusiveCacheScalaModule
