#!/bin/bash

declare -a seeds=(6 28 221 394 496 1024 1102 1729 2309 8128)

for seed in ${seeds[@]}
do
    python src/naba-nana.py --model_name kanishka/smolm-autoreg-bpe-seed_$seed --device cuda:2
done