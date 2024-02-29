#!/bin/bash

# declare -a seeds=(111 222 333 444 555 666 777 888 999 1709)
declare -a seeds=(111 222 333 444)

for seed in ${seeds[@]}
do 
    # python src/prune_results.py --results_dir data/results/single_stimuli_dative_simulation/smolm-autoreg-bpe-seed_$seed/raw
    # python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation/smolm-autoreg-bpe-seed_$seed/raw

    python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd/smolm-autoreg-bpe-seed_$seed/raw
done