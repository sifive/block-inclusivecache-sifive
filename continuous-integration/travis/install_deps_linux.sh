#!/bin/sh
set -ex

wget https://github.com/sifive/wake/releases/download/v0.15.1/ubuntu-16.04-wake_0.15.1-1_amd64.deb
sudo dpkg -i ubuntu-16.04-wake_0.15.1-1_amd64.deb
