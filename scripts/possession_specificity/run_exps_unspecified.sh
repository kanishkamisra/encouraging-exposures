#!/bin/bash

EPOCHS=70
declare -a lrs=(0.1 0.01)
declare -a seeds=(6 28 221 394 496 1024 1102 1729 2309 8128)
# declare -a seeds=(1024 1102 1729 2309 8128)


for seed in ${seeds[@]}
do
    for lr in ${lrs[@]}
    do
        python src/simulation.py --model_name kanishka/smolm-autoreg-bpe-seed_$seed --gaussian --lr $lr --experiment_name single_stimuli_dative_simulation_valtest_vbd_possession_unspecified
    done
    python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_possession_unspecified/smolm-autoreg-bpe-seed_$seed/raw --experiment_name single_stimuli_dative_simulation_valtest_vbd_possession_unspecified
done
