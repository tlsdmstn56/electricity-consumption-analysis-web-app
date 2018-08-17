#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "1 Missing Arguments: package name"
else
  sudo su - -c "R -q -e \"install.packages('$1', repos='http://cran.rstudio.com/')\""
fi