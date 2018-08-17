#!/bin/bash
source config.conf
SRC_DIR=$PRODUCTION_DIR/consumption-elec/
DATA_DIR=$PRODUCTION_DIR/data/

sudo rm -rf $SRC_DIR
sudo rm -rf $DATA_DIR

if [ ! -d $SRC_DIR ]; then
    sudo mkdir $SRC_DIR
fi
if [ ! -d $DATA_DIR ]; then
    sudo mkdir $DATA_DIR
fi

sudo cp -rf src/* $SRC_DIR
sudo cp -rf data/* $DATA_DIR
