declare -a seeds=(1709 1024 42 211 2409)

for seed in ${seeds[@]}
do
    python src/learning-trials/simulation.py \
        --model kanishka/smolm-aochildes-vocab_8192-layers_8-attn_8-hidden_256-inter_1024-lr_1e-3-seed_${seed} \
        --validation data/experiments/verbhood.json \
        --generalization data/experiments/generalization.jsonl \
        --training data/experiments/arunachalam-final.jsonl \
        --experiment_name arunachalam-final \
        --results_dir data/results/simulation-results/
done