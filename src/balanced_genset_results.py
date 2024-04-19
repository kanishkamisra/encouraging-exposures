"""
Given a result directory, this script will first load the model,
then given the item_id, hypothesis_id, hypothesis_instance, lr, adaptation_dative from the best_results file,
it will load the corresponding verb embedding and then run generalization on the passed gen set.
"""

import argparse
import os
import pathlib
import torch
import utils

from experiment import Learner
from torch.utils.data import DataLoader

from tqdm import tqdm

def main(args):
    results_dir = args.results_dir
    gen_path = args.gen_path

    gen_set = utils.read_csv_dict(gen_path)

    gen_dl = DataLoader(gen_set, batch_size=128, shuffle=False)

    for dir in os.listdir(results_dir):
        model = f"kanishka/{dir}"

        print(f"Running generalization for {model}")

        results_file = f"{results_dir}/{dir}/best_lr_results_hypwise.csv"
        results = utils.read_csv_dict(results_file)

        unique_results = set()
        for res in results:
            item_id = res["item_id"]
            hyp_id = res["hypothesis_id"]
            hyp_instance = res["hypothesis_instance"]
            lr = res["lr"]
            adaptation_dative = res["adaptation_dative"]

            unique_results.add((item_id, hyp_id, hyp_instance, lr, adaptation_dative))

        unique_results = sorted(list(unique_results), key=lambda x: x[0])

        lm = Learner(model, device=args.device)
        lm.add_tokens()
        lm.freeze_full()

        gen_results = []

        for res in tqdm(unique_results):
            # item_id = res["item_id"]
            # hyp_id = res["hypothesis_id"]
            # hyp_instance = res["hypothesis_instance"]
            # lr = res["lr"]
            # adaptation_dative = res["adaptation_dative"]
            item_id, hyp_id, hyp_instance, lr, adaptation_dative = res

            scores = {"do": [], "pp": []}

            emb = torch.load(
                f"{results_dir}/{dir}/raw/{item_id}_{hyp_id}_{hyp_instance}_{lr}_{adaptation_dative}_results/verb_embedding.pt"
            ).to(args.device)

            lm.reinitialize(emb)

            for batch in gen_dl:
                # keys, constructions, sentences = batch
                # print(batch)
                logprobs = lm.sequence_score(batch['sentence'])
                for c, l in zip(batch['dative'], logprobs):
                    scores[c].append(l)

            avg_scores = {k: sum(v) / len(v) for k, v in scores.items()}

            for k, v in avg_scores.items():
                gen_results.append(
                    {
                        "item_id": item_id,
                        "hypothesis_id": hyp_id,
                        "hypothesis_instance": hyp_instance,
                        "lr": lr,
                        "adaptation_dative": adaptation_dative,
                        "generalization_dative": k,
                        "logprob": v,
                    }
                )

        # write gen results to csv
        # with open(
        #     f"{results_dir}/{dir}/best_lr_results_hypwise_balanced_gen.csv", "w"
        # ) as f:
        #     writer = csv.DictWriter(f, fieldnames=gen_results[0].keys())
        #     writer.writeheader()
        utils.write_csv_dict(
            gen_results, f"{results_dir}/{dir}/best_lr_results_hypwise_balanced_gen.csv"
        )


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--results_dir", type=str, required=True, help="Path to the results directory"
    )
    parser.add_argument(
        "--gen_path", type=str, required=True, help="Path to the gen set"
    )
    parser.add_argument("--device", type=str, default="cpu")
    args = parser.parse_args()
    main(args)
