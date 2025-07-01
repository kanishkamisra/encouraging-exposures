#!/bin/bash

# declare -a seeds=(111 222 333 444 555 666 777 888 999 1709)
# declare -a seeds=(1102 1024 394 221 1729 2309 6 28 496 8128)

CHILDES_TRAIN=/scratch/corpora/babylm_data/babylm_100M/aochildes.train
CHILDES_DEV=/scratch/corpora/babylm_data/babylm_dev/aochildes.dev

dataset="aochildes"

declare -a seeds=(1024)

LEARNING_RATE=$1
VOCAB_SIZE=$2
HIDDEN_SIZE=$3
INTERMEDIATE_SIZE=$4
LAYERS=$5
ATTENTION_HEADS=$6
MODEL_DIR=$7


for seed in ${seeds[@]}
do

    MODEL_NAME=smolm-${dataset}-vocab_${VOCAB_SIZE}-layers_${LAYERS}-attn_${ATTENTION_HEADS}-hidden_${HIDDEN_SIZE}-inter_${INTERMEDIATE_SIZE}-lr_${LEARNING_RATE}-seed_${seed}

    python src/lming/tokenizer_and_config.py \
    -m $MODEL_NAME \
    --bpe \
    --vocab $VOCAB_SIZE \
    --hidden_size $HIDDEN_SIZE \
    --intermediate_size $INTERMEDIATE_SIZE \
    --train_file $CHILDES_TRAIN \
    --model_dir $MODEL_DIR

    python src/lming/train_autoreg.py \
        --config_name $MODEL_DIR/$MODEL_NAME/config.json \
        --tokenizer_name $MODEL_DIR/$MODEL_NAME \
        --per_device_train_batch_size 16 \
        --per_device_eval_batch_size 128 \
        --do_train \
        --do_eval \
        --seed ${seed} \
        --train_file $CHILDES_TRAIN \
        --validation_file $CHILDES_DEV \
        --evaluation_strategy epoch \
        --save_strategy epoch \
        --output_dir $MODEL_DIR/$MODEL_NAME \
        --overwrite_output_dir \
        --learning_rate ${LEARNING_RATE} \
        --save_total_limit 1 \
        --num_train_epochs 20 \
        --logging_steps 500 \
        --add_prefix_space \
        --weight_decay 0.1 \
        --warmup_steps 24000 \
        --early_stopping \
        --early_stopping_patience 3 \
        --metric_for_best_model eval_loss \
        --load_best_model_at_end
        # --torch_dtype bfloat1
        # --bf16 \
        # \
        # --push_to_hub \
        # --hub_model_id $MODEL_NAME
done