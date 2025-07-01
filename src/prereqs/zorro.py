import argparse
import csv
import itertools
import os
import pathlib

import numpy as np

from collections import defaultdict
from minicons import scorer
from torch.utils.data import DataLoader
from tqdm import tqdm


def read_zorro(file):
    zorro = []
    with open(file, "r") as f:
        for line in f:
            zorro.append(line.strip())

    zorro_paired = []
    for bad, good in itertools.zip_longest(zorro[0::2], zorro[1::2]):
        zorro_paired.append({"sentence_good": good, "sentence_bad": bad})

    assert len(zorro_paired) % 2 == 0

    return zorro_paired


def main(args):
    model = args.model
    model_name = model.replace("models/final/", "").replace("models/systematic/", "").replace("/", "_")

    ZORRO_PATH = args.zorro
    zorro_files = os.listdir(ZORRO_PATH)

    lm = scorer.IncrementalLMScorer(model, device=args.device)

    results = defaultdict(float)
    for file in tqdm(zorro_files):
        if ".txt" in file:
            phenomenon_name = file.strip(".txt")
            in_path = f"{ZORRO_PATH}/{file}"
            out_path = f"{ZORRO_PATH}/{phenomenon_name}.jsonl"

            sents = read_zorro(in_path)

            dl = DataLoader(sents, batch_size=128)

            good_scores = []
            bad_scores = []
            for batch in dl:
                good_scores.extend(lm.sequence_score(batch["sentence_good"]))
                bad_scores.extend(lm.sequence_score(batch["sentence_bad"]))

            correctness = []
            for g, b in zip(good_scores, bad_scores):
                correctness.append(int(g > b))
            accuracy = np.mean(correctness)
            results[phenomenon_name] = accuracy

    results["overall"] = np.mean(list(results.values()))

    full_results = [["model", "phenomenon", "accuracy"]]

    for phenomenon, accuracy in results.items():
        full_results.append([model, phenomenon, accuracy])

    pathlib.Path(args.out_dir).mkdir(exist_ok=True, parents=True)
    with open(f"{args.out_dir}/{model_name}.csv", "w") as f:
        writer = csv.writer(f)
        writer.writerows(full_results)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", type=str)
    parser.add_argument("--device", type=str, default="cpu")
    parser.add_argument("--zorro", type=str, default="data/zorro")
    parser.add_argument("--out_dir", type=str, default="data/zorro_results/systematic")
    args = parser.parse_args()

    main(args)
