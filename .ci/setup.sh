#!/bin/bash

set -eo pipefail

##### Debian #####
# update package list
apt-get update -y
apt-get dist-upgrade -y

while IFS= read -r package;
do
  apt-get install -y $package
done < ".ci/apt-requirements.txt"

##### R #####
# check if Rdevel is available
if test -z $(which RD)
then
  # install BiocManager
  Rscript -e 'install.packages("BiocManager", repos="http://cran.r-project.org")'
  # install r dependencies
  # https://www.cyberciti.biz/faq/unix-howto-read-line-by-line-from-file/
  while IFS= read -r package;
  do
    Rscript -e "BiocManager::install('"$package"', update = TRUE, ask = FALSE)";
  done < ".ci/r-requirements.txt"
else
  # install BiocManager
  RDscript -e 'install.packages("BiocManager", repos="http://cran.r-project.org")'
  # update BiocManager to devel
  RDscript -e 'BiocManager::install(version = "devel", update = TRUE, ask = FALSE)'
  # reinstall all packages to avoid "Error: package X was installed before R 4.0.0: please re-install it"
  RDscript -e 'BiocManager::install(installed.packages()[,1])'
  # install r dependencies
  # https://www.cyberciti.biz/faq/unix-howto-read-line-by-line-from-file/
  while IFS= read -r package;
  do
    RDscript -e "BiocManager::install('"$package"', update = TRUE, ask = FALSE)";
  done < ".ci/r-requirements.txt"
fi
