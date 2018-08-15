#!/bin/bash
for i in "$@"
do
case $i in
    --shinypath=*)
    SHINYPATH="${i#*=}"
    shift # past argument=value
    ;;
    -p=*|--port=*)
    PORT="${i#*=}"
    shift # past argument=value
    ;;
    -n=*|--name=*)
    NAME="${i#*=}"
    shift # past argument=value
    ;;
    *)
    echo "invalid option: $i"      # unknown option
    exit 1
    ;;
esac
done
if ! [[ -v SHINYPATH ]];then
    echo "Missing Option: shinypath"
    exit 2
fi
if ! [[ -v NAME ]];then
    echo "Missing Option: name"
    exit 2
fi
if ! [[ -v PORT ]];then
    echo "Missing Option: port"
    exit 2
fi
sudo docker run -p $PORT:3838 \
-v $SHINYPATH:/srv/shiny-server \
--name $NAME \
-d shiny-server 
