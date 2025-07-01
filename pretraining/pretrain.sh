
# bash pretraining/train_autoreg.sh 1e-3 8192 256 1024 8 8 models/final


# conda activate dev for this one
model_dir=models/final
vocab=8192
layer=8
attn=8
hidden=256
int=1024
lr=1e-3

# declare -a seeds=(1024 42 211 2409 1709 210 924 1102 1729 7)
declare -a seeds=(1709)

for seed in ${seeds[@]}
do

    python src/prereqs/zorro.py --model $model_dir/smolm-aochildes-vocab_${vocab}-layers_${layer}-attn_${attn}-hidden_${hidden}-inter_${int}-lr_${lr}-seed_${seed} --device cuda:0 --out_dir data/zorro_results/final
                         
    # python src/zorro.py --model $model_dir/smolm-aochildes-vocab_${vocab}-layers_${layer}-attn_${attn}-hidden_${hidden}-inter_${int}-lr_${lr}-seed_${seed} --device cuda:0 --zorro data/qin-zorro --out_dir data/qin-zorro_results

    python src/prereqs/nabanana.py --model $model_dir/smolm-aochildes-vocab_${vocab}-layers_${layer}-attn_${attn}-hidden_${hidden}-inter_${int}-lr_${lr}-seed_${seed} --device cuda:0 --results_dir data/nabanana/final 
done