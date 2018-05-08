#!/bin/bash
git pull
cabal build
cp dist/build/MensaCampusNord/MensaCampusNord /usr/local/bin/MensaCampusNord
cp mensacampusnord.* /etc/systemctl/system/