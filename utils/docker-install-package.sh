#!/bin/bash
if [ "$EUID" -ne 0 ]
  then echo "Please run as root or sudo"
  exit 1
fi

for i in "$@"
do
case $i in
    -n=*|--name=*)
    NAME="${i#*=}"
    shift # past argument=value
    ;;
    -p=*|--package=*)
    PACKAGE="${i#*=}"
    shift # past argument=value
    ;;
    *)
    echo "invalid option: $i, ignored"      # unknown option
    ;;
esac
done
if [[ -z $NAME ]] ; then
  echo -n "Docker name? "
  read NAME
fi
if [[ -z $PACKAGE ]] ; then
  echo -n "Package name to install? "
  read PACKAGE
fi
echo "Installing $PACKAGE in $NAME..."
echo
sudo docker exec $NAME sudo su - -c \
  "R -q -e \"if(!('$PACKAGE' %in% rownames(installed.packages()))){ \
  print(\"true??\"); \
  install.packages('$PACKAGE', repos='http://cran.rstudio.com/')}\""
  