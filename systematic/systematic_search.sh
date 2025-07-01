#!/bin/bash

declare -a lrs=(1e-3 1e-4 3e-3 3e-4)
declare -a vocabs=(8192 16384)
# declare -a vocabs=(8192)
# declare -a layers=(8 16)
declare -a layers=(8)
# declare -a attns=(8 16)
declare -a attns=(8)
declare -a hiddens=(256 512)
# declare -a hiddens=(512)
declare -a intermediates=(1024 2048)
# declare -a intermediates=(2048)

model_dir=models/systematic

for lr in ${lrs[@]}
do
    for vocab in ${vocabs[@]}
    do 
        for layer in ${layers[@]}
        do
            for attn in ${attns[@]}
            do
                for hidden in ${hiddens[@]}
                do
                    for int in ${intermediates[@]}
                    do
                        # bash systematic/train_autoreg.sh ${lr} ${vocab} ${hidden} ${int} ${layer} ${attn} $model_dir
                        
                        # python src/zorro.py --model $model_dir/smolm-aochildes-vocab_${vocab}-layers_${layer}-attn_${attn}-hidden_${hidden}-inter_${int}-lr_${lr}-seed_1024 --device cuda:0
                        
                        # python src/zorro.py --model $model_dir/smolm-aochildes-vocab_${vocab}-layers_${layer}-attn_${attn}-hidden_${hidden}-inter_${int}-lr_${lr}-seed_1024 --device cuda:0 --zorro data/qin-zorro --out_dir data/qin-zorro_results

                        python src/nabanana.py --model $model_dir/smolm-aochildes-vocab_${vocab}-layers_${layer}-attn_${attn}-hidden_${hidden}-inter_${int}-lr_${lr}-seed_1024 --device cuda:0 --results_dir data/nabanana 
                    done
                done
            done
        done
    done
done
