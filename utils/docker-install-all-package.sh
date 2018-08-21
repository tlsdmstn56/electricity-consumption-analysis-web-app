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
    *)
    echo "invalid option: $i, ignored"      # unknown option
    ;;
esac
done
if [[ -z $NAME ]] ; then
  echo -n "Docker name? "
  read NAME
fi
sudo apt install libdev-ssl
bash find-packages.sh | \
while read PKG;
do
sudo bash docker-install-package.sh -n=$NAME -p=$PKG
done