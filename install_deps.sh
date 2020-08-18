#!/bin/sh

set -e
apt-get update
apt-get -y install libxml2 libxml2-dev libglpk40 libssl-dev libcurl4-openssl-dev

su -c "cd /srv/shiny-server/progeny && R -f bootstrap.R" - shiny
