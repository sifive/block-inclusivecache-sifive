#!/bin/sh -x

export WAKE_PATH=$PATH

echo "Initialize Workspace"

commit=$(git rev-parse HEAD)

git config --global url."https://github.com/".insteadOf 'git@github.com:'
wit init workspace -a git@github.com:sifive/block-inclusivecache-sifive.git::$commit
cd workspace/
wit add-pkg https://github.com/sifive/environment-example-sifive.git
wit update

wake --init .

echo "Compile Scala"

wake --no-tty -j1 -v 'compileScalaModule inclusiveCacheScalaModule | getPathResult'
