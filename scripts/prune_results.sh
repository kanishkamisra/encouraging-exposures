#!/bin/bash

# declare -a seeds=(111 222 333 444 555 666 777 888 999 1709)
declare -a seeds=(6 28 221 394 496 1024 1102 1729 2309 8128)
# declare -a seeds=(333 444 555 666)
# declare -a seeds=(1102)

for seed in ${seeds[@]}
do 
    # python src/prune_results.py --results_dir data/results/single_stimuli_dative_simulation/smolm-autoreg-bpe-seed_$seed/raw
    # python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation/smolm-autoreg-bpe-seed_$seed/raw

    # python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation/smolm-autoreg-bpe-seed_$seed/raw --experiment_name single_stimuli_dative_simulation

    # python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd/smolm-autoreg-bpe-seed_$seed/raw --experiment_name single_stimuli_dative_simulation_valtest_vbd
    # python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_no_discourse/smolm-autoreg-bpe-seed_$seed/raw --experiment_name single_stimuli_dative_simulation_valtest_vbd_no_discourse
    # python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_no_discourse2/smolm-autoreg-bpe-seed_$seed/raw --experiment_name single_stimuli_dative_simulation_valtest_vbd_no_discourse2
    # python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_no_discourse3/smolm-autoreg-bpe-seed_$seed/raw --experiment_name single_stimuli_dative_simulation_valtest_vbd_no_discourse3
    # python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_discourse_control/smolm-autoreg-bpe-seed_$seed/raw --experiment_name single_stimuli_dative_simulation_valtest_vbd_discourse_control
    python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_no_markedness_discourse_control/smolm-autoreg-bpe-seed_$seed/raw --experiment_name single_stimuli_dative_simulation_valtest_vbd_no_markedness_discourse_control
    # python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_no_markedness_no_discourse/smolm-autoreg-bpe-seed_$seed/raw --experiment_name single_stimuli_dative_simulation_valtest_vbd_no_markedness_no_discourse
done
