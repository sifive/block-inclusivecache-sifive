#!/bin/sh -x

set -e

export WAKE_PATH=$PATH

echo "Initialize Workspace"

git config --global url."https://github.com/".insteadOf 'git@github.com:'
wit --repo-path $PWD/.. init workspace -a block-inclusivecache-sifive
cd workspace/
wit add-pkg https://github.com/sifive/environment-example-sifive.git
wit update

wake --init .

echo "Compile Scala"

wake --no-tty -j1 -v 'compileScalaModule inclusiveCacheScalaModule | getPathResult'
