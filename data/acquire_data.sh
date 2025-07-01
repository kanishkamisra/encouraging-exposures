#!/bin/bash

mkdir -p data/corpora

curl -L https://github.com/babylm/babylm.github.io/raw/main/babylm_data.zip -o data/corpora/babylm_data.zip

unzip data/corpora/babylm_data.zip -d data/corpora/
