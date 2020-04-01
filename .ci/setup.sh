#!/bin/bash

set -eo pipefail

##### Debian #####
# update package list
apt-get update -y
apt-get upgrade -y

while IFS= read -r package;
do
  apt-get install -y $package
done < ".ci/apt-requirements.txt"

##### R #####
# install BiocManager
Rscript -e 'install.packages("BiocManager", repos="http://cran.r-project.org")'
# install r dependencies
# https://www.cyberciti.biz/faq/unix-howto-read-line-by-line-from-file/
while IFS= read -r package;
do
  Rscript -e "BiocManager::install('"$package"', update = TRUE, ask = FALSE)";
done < ".ci/r-requirements.txt"
