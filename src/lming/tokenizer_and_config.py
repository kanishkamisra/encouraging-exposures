import argparse
import json
import os
import pathlib
import tokenization

from datasets import load_dataset
from transformers import AutoConfig, AutoTokenizer
from tokenizers import models


def mlm_config(
    model_name,
    tokenizer,
    vocab=8192,
    hidden_size=256,
    attention=8,
    layers=8,
    intermediate=1024,
    max_len=130,
):
    return AutoConfig.from_pretrained(
        "roberta-base",
        name_or_path=model_name,
        vocab_size=vocab,
        hidden_size=hidden_size,
        num_attention_heads=attention,
        num_hidden_layers=layers,
        intermediate_size=intermediate,
        max_position_embeddings=max_len,
        bos_token_id=tokenizer.special_tokens["bos"]["id"],
        pad_token_id=tokenizer.special_tokens["pad"]["id"],
        eos_token_id=tokenizer.special_tokens["eos"]["id"],
        unk_token_id=tokenizer.special_tokens["unk"]["id"],
        mask_token_id=tokenizer.special_tokens["mask"]["id"],
        bos_token="<s>",
        eos_token="</s>",
        unk_token="<unk>",
        pad_token="<pad>",
        mask_token="<mask>",
    )


def autoreg_config(
    model_name,
    tokenizer,
    vocab=8192,
    hidden_size=256,
    attention=8,
    layers=8,
    intermediate=1024,
    max_len=130,
):
    return AutoConfig.from_pretrained(
        "facebook/opt-125m",
        name_or_path=model_name,
        vocab_size=vocab,
        hidden_size=hidden_size,
        ffn_dim=intermediate,
        num_attention_heads=attention,
        num_hidden_layers=layers,
        max_position_embeddings=max_len,
        word_embed_proj_dim=hidden_size,
        bos_token_id=tokenizer.special_tokens["bos"]["id"],
        eos_token_id=tokenizer.special_tokens["eos"]["id"],
        pad_token_id=tokenizer.special_tokens["pad"]["id"],
        prefix=tokenizer.special_tokens["bos"]["token"],
    )


def main(args):
    train_file = args.train_file
    mlm = args.mlm
    bpe = args.bpe
    model_name = args.model_name
    model_dir = args.model_dir

    if not mlm:
        eos_token = "<s>"

    if bpe:
        tokenizer = tokenization.BPETokenizer(mask=mlm, eos_token=eos_token)
    else:
        tokenizer = tokenization.SentencePieceUnigramTokenizer(mask=mlm)

    if args.from_iterator:
        dataset = load_dataset(train_file, split="train")
        tokenizer.train_from_iterator(dataset['text'], vocab_size=args.vocab)
    else:
        tokenizer.train(files=train_file, vocab_size=args.vocab)

    tokenizer

    if mlm:
        cfg = mlm_config(
            model_name,
            tokenizer,
            args.vocab,
            args.hidden_size,
            args.attention_heads,
            args.layers,
            args.intermediate_size,
            args.max_len,
        )
    else:
        cfg = autoreg_config(
            model_name,
            tokenizer,
            args.vocab,
            args.hidden_size,
            args.attention_heads,
            args.layers,
            args.intermediate_size,
            args.max_len,
        )

    pathlib.Path(f"{model_dir}/{model_name}").mkdir(parents=True, exist_ok=True)
    tokenizer.save(f"{model_dir}/{model_name}/tokenizer.json")
    tokenizer.save_model(f"{model_dir}/{model_name}")
    cfg._name_or_path = model_name
    cfg.save_pretrained(f"{model_dir}/{model_name}")

    # if not mlm:
    # # remove unncessary token forcefully added by hf, just because.
    #     tokenizer = AutoTokenizer.from_pretrained(f"models/{model_name}")
    #     unwanted = "<|endoftext|>"
    #     model_state = json.loads(tokenizer.backend_tokenizer.model.__getstate__())
    #     del model_state["vocab"][unwanted]
    #     model_class = getattr(models, model_state.pop("type"))
    #     tokenizer.backend_tokenizer.model = model_class(**model_state)
    #     # tokenizer.eos_token = "<s>"
    #     # tokenizer.eos_token = "<s>"
    #     tokenizer.save(f"models/{model_name}/tokenizer.json")
    #     tokenizer.save_model(f"models/{model_name}")





if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--model_dir", type=str, default="models")
    parser.add_argument("--model_name", "-m", type=str, default="smolm-mlm")
    parser.add_argument("--train_file", "-t", type=str)
    parser.add_argument("--mlm", action="store_true")
    parser.add_argument("--bpe", action="store_true")
    parser.add_argument("--vocab", "-v", type=int, default=8192)
    parser.add_argument("--hidden_size", "-hs", type=int, default=256)
    parser.add_argument("--intermediate_size", "-i", type=int, default=1024)
    parser.add_argument("--max_len", "-l", type=int, default=128)
    parser.add_argument("--layers", "-y", type=int, default=8)
    parser.add_argument("--attention_heads", "-a", type=int, default=8)
    parser.add_argument("--from_iterator", "-f", action="store_true")
    args = parser.parse_args()

    print(args)

    main(args)

"""
when loading tokenizer for autoregressive:

reset pad, eos, bos, unk tokens
tokenizer.<special_token_id> = 0,1,2,3,...
do not resize embedding layer.
"""
