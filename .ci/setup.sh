#!/bin/bash

set -eo pipefail

##### Debian #####
# update package list
echo "==================== apt-get update & dist-upgrade ===================="
apt-get update -y

DEBIAN_FRONTEND=noninteractive \
apt-get \
-o Dpkg::Options::="--force-confnew" \
-o Dpkg::Options::="--force-confdef" \
--allow-downgrades \
--allow-remove-essential \
--allow-change-held-packages \
--fix-broken \
--show-upgraded \
--yes \
dist-upgrade
# Dpkg::Options = if a config changed install default version and fall back to new version
echo "================================ done ================================"

echo "==================== apt-get install requirements ===================="
while IFS= read -r package;
do
  echo "------------ installing $package ------------"
  apt-get install -y $package
  echo "---------- done installing $package ----------"
done < ".ci/apt-requirements.txt"
echo "================================ done ================================"

##### R #####
# check if Rdevel is available
if test -z $(which RD)
then
  echo "==================== install r-base requirements ===================="
  # install BiocManager
  Rscript -e 'install.packages("BiocManager", repos="http://cran.r-project.org")'
  # install r dependencies
  # https://www.cyberciti.biz/faq/unix-howto-read-line-by-line-from-file/
  while IFS= read -r package;
  do
    echo "------------ installing R $package ------------"
    Rscript -e "BiocManager::install('"$package"', update = TRUE, ask = FALSE)";
    echo "---------- done installing R $package ----------"
  done < ".ci/r-requirements.txt"
else
  echo "==================== install r-devel requirements ===================="
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
    echo "------------ installing R $package ------------"
    RDscript -e "BiocManager::install('"$package"', update = TRUE, ask = FALSE)";
    echo "---------- done installing R $package ----------"
  done < ".ci/r-requirements.txt"
fi
echo "================================ done ================================"
