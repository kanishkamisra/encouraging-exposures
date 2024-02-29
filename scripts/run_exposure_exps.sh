#!/bin/bash

EPOCHS=100
declare -a lrs=(0.1 0.01 0.05 0.001 0.005)
declare -a seeds=(111 222 333 444 555 666 777 888 999 1709)

for seed in ${seeds[@]}
do
    for lr in ${lrs[@]}
    do
        python src/simulation.py --model_name kanishka/smolm-autoreg-bpe-seed_$seed --gaussian --lr $lr --experiment_name single_stimuli_dative_simulation_valtest_vbd
        python src/simulation.py --model_name kanishka/smolm-autoreg-bpe-seed_$seed --gaussian --lr $lr
    done
done