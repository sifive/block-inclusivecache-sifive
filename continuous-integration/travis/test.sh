#!/bin/sh -x

set -e

export WAKE_PATH=$PATH

echo "Initialize Workspace"

git config --global url."https://github.com/".insteadOf 'git@github.com:'
wit --repo-path $PWD/.. init workspace -a block-inclusivecache-sifive
cd workspace/

echo "Compile Scala"

wake --init .
wake --no-tty -j1 -dv 'compileScalaModule inclusiveCacheScalaModule | getPathResult'
