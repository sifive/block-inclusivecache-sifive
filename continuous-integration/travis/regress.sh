#!/bin/sh -x

echo "Install Wit/Wake"

git clone https://github.com/sifive/wit.git
export PATH=$PATH:$PWD/wit

export USE_FUSE_WAKE=0
rm -r wake # temporary solution to duplicate naming issue
git clone https://github.com/sifive/wake.git
cd wake
git checkout v0.15
make
export PATH=$PATH:$PWD/bin

echo "Initialize Workspace"

cd ..
wit --repo-path $PWD/repos init workspace -a block-inclusivecache-sifive
cd workspace/
wit add-pkg https://github.com/sifive/environment-example-sifive.git
wit update

wake --init .

cd block-inclusivecache-sifive

echo "compile scala"

wake -j1 -v compileScalaModule inclusiveCacheScalaModule
