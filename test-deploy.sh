#!/bin/bash
SRC_DIR=/srv/shiny-server/test-consumption-elec/
DATA_DIR=/srv/shiny-server/data/

sudo rm -rf $SRC_DIR

if [ ! -d $SRC_DIR ]; then
    sudo mkdir $SRC_DIR
fi
if [ ! -d $DATA_DIR ]; then
    sudo mkdir $DATA_DIR
fi
sudo cp -rf consumption-analysis/src/* $SRC_DIR
sudo cp -rf consumption-analysis/data/* $DATA_DIR
