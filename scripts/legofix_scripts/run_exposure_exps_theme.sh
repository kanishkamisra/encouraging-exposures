EPOCHS=70
declare -a lrs=(0.1 0.01)
declare -a seeds=(6 28 221 394 496 1024 1102 1729 2309 8128)
declare -a templates=(1 2 3)

for template in ${templates[@]}
do
    for seed in ${seeds[@]}
    do
        for lr in ${lrs[@]}
        do
            python src/simulation.py --model_name kanishka/smolm-autoreg-bpe-seed_$seed --gaussian --lr $lr --experiment_name single_stimuli_dative_simulation_valtest_vbd_discourse_theme_given_legofix_template_$template
        done
    done
done

for template in ${templates[@]}
do
    for seed in ${seeds[@]}
    do
        python src/prune_results_new.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_discourse_theme_given_legofix_template_$template/smolm-autoreg-bpe-seed_$seed/raw --experiment_name single_stimuli_dative_simulation_valtest_vbd_discourse_theme_given_legofix_template_$template
    done

    python src/balanced_genset_results.py --results_dir data/results/single_stimuli_dative_simulation_valtest_vbd_discourse_theme_given_legofix_template_$template --gen_path data/balanced_generalization_set_four_features.csv --device cuda:0
done