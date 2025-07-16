import argparse
import csv
import json
import pathlib
import random
import torch
import utils

import numpy as np

from collections import defaultdict, Counter
from minicons import scorer
from torch import optim
from torch.utils.data import DataLoader
from tqdm import tqdm, trange
from transformers import (
    AdamW,
    get_constant_schedule,
    set_seed,
)

from experiment import Learner, Trainer


def main(args):

    model = args.model
    model_name = model.replace("kanishka/", "")

    validation = utils.read_json(args.validation)
    generalization = utils.read_jsonl(args.generalization)

    # lrs = [0.01, 0.02, 0.05]
    lrs = [0.05]
    # decays = [0.0, 0.01, 0.1]
    decays = [0.0]
    NUM_EPOCHS = 70

    # sents = ["do you see lucy and me ?\n<s> lucy [verb] a red ball to me .", "do you see lucy and me ?\n<s> lucy [verb] me a red ball ."]
    training_data = utils.read_jsonl(args.training)

    full_results = []
    embeddings = []
    for item in tqdm(training_data):
        stimulus = item["stimulus"]
        results = defaultdict(tuple)
        for lr in tqdm(lrs):
            for wd in decays:
                set_seed(1024)

                model = Learner(
                    args.model,
                    device=args.device,
                )

                trainer = Trainer(
                    model,
                    stimulus,
                    generalization,
                    validation,
                    learning_rate=lr,
                    weight_decay=wd,
                )
                trainer.train(
                    num_epochs=NUM_EPOCHS, generalization_batch_size=128
                ), len(trainer.metrics["val_performance"])

                results[(lr, wd)] = (
                    trainer.metrics["val_performance"][trainer.best_epoch - 1],
                    trainer.agg_gen_results,
                    trainer.best_embs,
                )

        (best_lr, best_wd), (val, best_result, best_embs) = sorted(
            results.items(), key=lambda x: x[1][0]
        )[-1]

        full_results.append(
            (
                item["idx"],
                item["dative"],
                best_lr,
                best_wd,
                trainer.best_epoch,
                val,
                best_result[("best", "do")],
                best_result[("best", "pp")],
            )
        )
        embeddings.append(best_embs)

    embeddings = [e.cpu() for e in embeddings]

    save_dir = f"{args.results_dir}/{args.experiment_name}/{model_name}/"
    pathlib.Path(save_dir).mkdir(parents=True, exist_ok=True)
    utils.write_csv(
        full_results,
        f"{save_dir}/results.csv",
        header=[
            "idx",
            "dative",
            "best_lr",
            "best_wd",
            "best_epoch",
            "verbhood_diff",
            "do",
            "pp",
        ],
    )
    torch.save(embeddings, f"{save_dir}/embeddings.pt")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--model",
        type=str,
        default="kanishka/smolm-aochildes-vocab_8192-layers_8-attn_8-hidden_256-inter_1024-lr_1e-3-seed_1709",
    )
    parser.add_argument(
        "--validation", type=str, default="data/experiments/verbhood.json"
    )
    parser.add_argument(
        "--generalization", type=str, default="data/experiments/generalization.jsonl"
    )
    parser.add_argument(
        "--training", type=str, default="data/experiments/givenness_template_1.jsonl"
    )
    parser.add_argument("--experiment_name", type=str, default="givenness_template_1")
    parser.add_argument("--device", type=str, default="cuda:0")
    parser.add_argument("--results_dir", type=str, default="data/results/final")
    args = parser.parse_args()
    main(args)
