#!/bin/bash
echo "readaing config.conf ..."
source config.conf

SRC_DIR=$STAGE_DIR/consumption-elec/
DATA_DIR=$STAGE_DIR/data/

sudo rm -rf $SRC_DIR
sudo rm -rf $DATA_DIR

if [ ! -d $SRC_DIR ]; then
    sudo mkdir $SRC_DIR
fi
if [ ! -d $DATA_DIR ]; then
    sudo mkdir $DATA_DIR
fi

sudo cp -rf consumption-analysis/src/* $SRC_DIR
sudo cp -rf consumption-analysis/data/* $DATA_DIR
