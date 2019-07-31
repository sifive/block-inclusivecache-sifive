#!/bin/sh
set -ex

wget https://github.com/sifive/wake/releases/download/v0.15.1/ubuntu-16.04-wake_0.15.1-1_amd64.deb
sudo dpkg -i ubuntu-16.04-wake_0.15.1-1_amd64.deb

mkdir protoc
cd protoc
wget https://github.com/protocolbuffers/protobuf/releases/download/v3.7.1/protoc-3.7.1-linux-x86_64.zip
unzip protoc-3.7.1-linux-x86_64.zip
export PATH=$PATH:$PWD/bin
cd ..
