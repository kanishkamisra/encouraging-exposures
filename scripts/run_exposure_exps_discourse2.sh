#!/bin/bash

EPOCHS=100
declare -a lrs=(0.1 0.01)
# declare -a seeds=(111 222 333 444 555 666 777 888 999 1709)
declare -a seeds=(1024 1102 1729 2309 8128)

for seed in ${seeds[@]}
do
    for lr in ${lrs[@]}
    do

        # python src/simulation.py --model_name kanishka/smolm-autoreg-bpe-seed_$seed --gaussian --lr $lr --experiment_name single_stimuli_dative_simulation_valtest_vbd_no_markedness_no_discourse
        # python src/simulation.py --model_name kanishka/smolm-autoreg-bpe-seed_$seed --gaussian --lr $lr --experiment_name single_stimuli_dative_simulation_valtest_vbd_discourse_control
        python src/simulation.py --model_name kanishka/smolm-autoreg-bpe-seed_$seed --gaussian --lr $lr --experiment_name single_stimuli_dative_simulation_valtest_vbd_no_markedness_discourse_control
        # python src/simulation.py --model_name kanishka/smolm-autoreg-bpe-seed_$seed --gaussian --lr $lr --experiment_name single_stimuli_dative_simulation_valtest_vbd_no_markedness_neutral_control
        # python src/simulation.py --model_name kanishka/smolm-autoreg-bpe-seed_$seed --gaussian --lr $lr
    done
done
