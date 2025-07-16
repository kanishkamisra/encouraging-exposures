declare -a templates=(1 2 3)
declare -a seeds=(1709 1024 42 211 2409)

for seed in ${seeds[@]}
do
    for template in ${templates[@]}
    do
        python src/learning-trials/simulation.py \
            --model kanishka/smolm-aochildes-vocab_8192-layers_8-attn_8-hidden_256-inter_1024-lr_1e-3-seed_${seed} \
            --validation data/experiments/verbhood.json \
            --generalization data/experiments/generalization.jsonl \
            --training data/experiments/final/givenness_template_${template}.jsonl \
            --experiment_name givenness_template_${template} \
            --results_dir data/results/simulation-results/final/
    done
done