#!/bin/bash
echo "********************************"
echo "*          Easy Setup          *"
echo "********************************"
echo "it might take more than 20 mins depending on your machine."
echo -n "Do you want to proceed it? [y/n]"
read ANS

if [ "$ANS" = "n" ]; then
exit 0
fi

# build docker
cat config.conf
echo -n n"please check your config file. Continue? [y/n]"
read ANS
if [ "$ANS" = "n" ]; then
exit 0
fi

source config.conf

if [ 4? -ne 0 ]; then
  echo "error occured, abort"
fi

echo "building shiny-docker image"
sudo docker build --pull ./docker/shiny-server/
echo "pulling rstudio image"
sudo docker pull rocker/rstudio-stable

bash docker/run-rstudio-docker.sh --dir=$RSTUDIO_DIR
bash docker/run-shiny-docker.sh --shinypath=$STAGE_DIR --port=$STAGE_DOCKER_PORT --name=$STAGE_DOCKER_NAME
bash docker/run-shiny-docker.sh --shinypath=$PRODUCTION_DIR --port=$PRODUCTION_DOCKER_PORT --name=$PRODUCTION_DOCKER_NAME
echo "dockers run successfully!"
