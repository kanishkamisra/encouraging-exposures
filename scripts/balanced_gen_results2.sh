#!/bin/bash

python src/balanced_genset_results.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_discourse_control --gen_path data/balanced_generalization_set_four_features.csv --device cuda:0

python src/balanced_genset_results.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_no_discourse --gen_path data/balanced_generalization_set_four_features.csv --device cuda:0

python src/balanced_genset_results.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_no_discourse2 --gen_path data/balanced_generalization_set_four_features.csv --device cuda:0

python src/balanced_genset_results.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_no_discourse3 --gen_path data/balanced_generalization_set_four_features.csv --device cuda:0

# python src/balanced_genset_results.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_discourse_theme_given_template_1 --gen_path data/balanced_generalization_set_four_features.csv --device cuda:0

# python src/balanced_genset_results.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_discourse_recipient_given_template_1 --gen_path data/balanced_generalization_set_four_features.csv --device cuda:0

python src/balanced_genset_results.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_discourse_theme_given_template_2 --gen_path data/balanced_generalization_set_four_features.csv --device cuda:0

python src/balanced_genset_results.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_discourse_recipient_given_template_2 --gen_path data/balanced_generalization_set_four_features.csv --device cuda:0
