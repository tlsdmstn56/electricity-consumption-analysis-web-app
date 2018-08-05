#!/bin/bash
SRC_DIR=/srv/shiny-server/consumption-elec/
DATA_DIR=/srv/shiny-server/data/

if [ ! -d $SRC_DIR ]; then
    sudo make $SRC_DIR
fi
if [ ! -d $DATA_DIR ]; then
    sudo make $DATA_DIR
fi
sudo cp -rf ./src/* $SRC_DIR
sudo cp -rf ./data/* $DATA_DIR
