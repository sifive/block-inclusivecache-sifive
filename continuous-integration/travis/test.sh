#!/bin/sh -x

export PATH=$PATH:$PWD/repos/wit

echo "Initialize Workspace"

wit --repo-path $PWD/repos init workspace -a block-inclusivecache-sifive
cd workspace/
wit add-pkg https://github.com/sifive/environment-example-sifive.git
wit update

wake --init .

echo "Compile Scala"

wake -j1 -v compileScalaModule inclusiveCacheScalaModule
