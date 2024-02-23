#!/bin/bash

declare -a seeds=(111 222 333 444 555 666 777 888 999 1709)

for seed in ${seeds[@]}
do
    python src/realverbs.py --model kanishka/smolm-autoreg-bpe-seed_$seed --device cuda:0
done