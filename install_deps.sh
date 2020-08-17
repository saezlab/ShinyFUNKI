#!/bin/sh

set -e
apt-get update
apt-get -y install libxml2 libglpk40

su -c "cd /srv/shiny-server/dorothea && R -f bootstrap.R" - shiny



