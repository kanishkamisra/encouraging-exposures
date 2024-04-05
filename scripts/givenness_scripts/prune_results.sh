#!/bin/bash

# declare -a seeds=(111 222 333 444 555 666 777 888 999 1709)
# declare -a seeds=(6 28 221 394 496 1024 1102 1729 2309 8128)
# declare -a seeds=(333 444 555 666)
declare -a seeds=(6 28 221)

for seed in ${seeds[@]}
do 
    python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_discourse_theme_given_template_1/smolm-autoreg-bpe-seed_$seed/raw --experiment_name single_stimuli_dative_simulation_valtest_vbd_discourse_theme_given_template_1

    python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_discourse_recipient_given_template_1/smolm-autoreg-bpe-seed_$seed/raw --experiment_name single_stimuli_dative_simulation_valtest_vbd_discourse_recipient_given_template_1
done
