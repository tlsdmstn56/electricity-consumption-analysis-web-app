#!/bin/bash
USER=rstudio
PASSWORD=$USER
DIR=../
ROOT=TRUE
PORT=8787
NAME=rstudio-docker
for i in "$@"
do
case $i in
    -p=*|--port=*)
    PORT="${i#*=}"
    shift # past argument=value
    ;;
    -d=*|--dir=*)
    DIR="${i#*=}"
    shift # past argument=value
    ;;
    -u=*|--user=*)
    USER="${i#*=}"
    shift # past argument=value
    ;;
    -pw=*|--password=*)
    PASSWORD="${i#*=}"
    shift # past argument=value
    ;;
    -r=*|--root=*)
    ROOT="${i#*=}"
    shift # past argument=value
    ;;
    -n=*|--name=*)
    NAME="${i#*=}"
    shift # past argument=value
    ;;
    *)
    echo "invalid option: $i, ignored"      # unknown option
    ;;
esac
done

sudo docker run -d -v $DIR:/home/$USER/ \
-p $PORT:8787 \
--name $NAME \
-e PASSWORD=$PASSWORD \
-e ROOT=$ROOT \
-e USER=$USER \
rocker/rstudio-stable && {
  echo "docker runs successfully by following configuration"
  echo "RSTUDIO_DIR: $DIR"
  echo "RSTUDIO_USER: $USER"
  echo "RSTUDIO_PORT: $PORT"
  echo "RSTUDIO_ROOT: $ROOT"
  echo "RSTUDIO_PASSWORD: $PASSWORD"
  exit 0
}
echo "docker run failed"






