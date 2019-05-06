#!/bin/bash

set -eo pipefail

#
# Set up correct channel order
#
conda config --system --add channels defaults
conda config --system --add channels bioconda
conda config --system --add channels conda-forge

#
# Install packages from requirements
#
conda install -q -y --file .ci/requirements.txt

conda clean -q -y --all
