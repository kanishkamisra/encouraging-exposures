import argparse
import csv
import json
import os
import pathlib
import re
import utils

from collections import defaultdict


def get_generalization_results(path):
    results = utils.read_csv_dict(path)
    logprobs = {
        "initial": {"do": [], "pp": []},
        "final": {"do": [], "pp": []},
        "best": {"do": [], "pp": []},
    }

    for entry in results:
        logprobs[entry["model_state"]]["do"].append(float(entry["do"]))
        logprobs[entry["model_state"]]["pp"].append(float(entry["pp"]))

    # get proportion of time do is greater than pp, and also the diff between do and pp for each model state
    generalization_results = defaultdict(float)
    for model_state in logprobs:
        do = logprobs[model_state]["do"]
        pp = logprobs[model_state]["pp"]
        generalization_results[f"{model_state}_delta"] = (sum(do) - sum(pp)) / len(do)
        generalization_results[f"{model_state}_do_pref"] = sum(
            [1 for d, p in zip(do, pp) if d > p]
        ) / len(do)
        generalization_results[f"{model_state}_do_mean"] = sum(do) / len(do)
        generalization_results[f"{model_state}_pp_mean"] = sum(pp) / len(pp)

    return dict(generalization_results)


def main(args):
    results_dir = args.results_dir
    model = results_dir.split("/")[-1]

    lr_wise = defaultdict(list)

    # generalization_results = defaultdict()

    for directory in sorted(os.listdir(results_dir), key=lambda x: int(x.split("_")[0])):
        if "_results" in directory:
            item, hypothesis, hypothesis_instance, lr, dative, _ = directory.split("_")
            lr = float(lr)
            summary = utils.read_json(
                f"{results_dir}/{directory}/training_summary.json"
            )
            val_performance = summary["best_val_performance"]
            generalization_results = get_generalization_results(
                f"{results_dir}/{directory}/generalization_results.csv"
            )
            lr_wise[(int(item), int(hypothesis), int(hypothesis_instance), dative)].append(
                (lr, val_performance, summary['best_epoch'], generalization_results)
            )

    # kepe only the best lr, max val performance
    best_lr_wise = {}
    for key, lr_perf_list in lr_wise.items():
        best_lr_wise[key] = max(lr_perf_list, key=lambda x: x[1])

    # save to csv (thanks copilot)
    with open(f"{results_dir.replace('/raw', '')}/best_lr_results.csv", "w") as f:
        writer = csv.writer(f)
        writer.writerow(
            [
                "lm",
                "item",
                "hypothesis_id",
                "hypothesis_instance",
                "adaptation_dative",
                "best_lr",
                "best_val_performance",
                "best_epoch",
                "initial_delta",
                "initial_do_pref",
                "initial_do_mean",
                "initial_pp_mean",
                "final_delta",
                "final_do_pref",
                "final_do_mean",
                "final_pp_mean",
                "best_delta",
                "best_do_pref",
                "best_do_mean",
                "best_pp_mean",
            ]
        )
        for key, (lr, val_performance, epoch, generalization_results) in best_lr_wise.items():
            writer.writerow(
                [
                    model,
                    key[0],
                    key[1],
                    key[2],
                    key[3],
                    lr,
                    val_performance,
                    epoch,
                    generalization_results["initial_delta"],
                    generalization_results["initial_do_pref"],
                    generalization_results["initial_do_mean"],
                    generalization_results["initial_pp_mean"],
                    generalization_results["final_delta"],
                    generalization_results["final_do_pref"],
                    generalization_results["final_do_mean"],
                    generalization_results["final_pp_mean"],
                    generalization_results["best_delta"],
                    generalization_results["best_do_pref"],
                    generalization_results["best_do_mean"],
                    generalization_results["best_pp_mean"]
                ]
            )


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--results_dir",
        type=str,
        default="data/results/single_stimuli_dative_simulation/smolm-autoreg-bpe-seed_111/raw",
    )
    args = parser.parse_args()

    main(args)
