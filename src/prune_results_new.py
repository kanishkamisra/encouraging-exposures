import argparse
import csv
import json
import os
import pathlib
import re
import utils

from collections import defaultdict
from tqdm import tqdm

def metadata(dir):
    exp_dir = dir.split("/")[-1]
    item_id, hyp_id, hyp_instance, lr, adaptation_dative, _ = exp_dir.split('_')
    training_summary = utils.read_json(f"{dir}/training_summary.json")
    return {
        'item_id': int(item_id),
        'hypothesis_id': int(hyp_id),
        'hypothesis_instance': int(hyp_instance),
        'lr': float(lr),
        'adaptation_dative': adaptation_dative,
        'val_performance': training_summary['best_val_performance'],
        'best_epoch': training_summary['best_epoch'],
        'train_loss': training_summary['train_loss']
    }

def compile_generalization_results(dir, genset, adapt2fc, forcsv=False):
    exp_metadata = metadata(dir)
    # print(exp_metadata)
    # exp_metadata['adaptation_dative']
    adaptation_feature_config = adapt2fc[exp_metadata['hypothesis_id']]

    generalization_results = utils.read_csv_dict(f"{dir}/generalization_results.csv")

    compiled_results = {
        'initial': {'do': defaultdict(list), 'pp': defaultdict(list)},
        'final': {'do': defaultdict(list), 'pp': defaultdict(list)},
        'best': {'do': defaultdict(list), 'pp': defaultdict(list)}
    }

    # return generalization_results
    for gen, parse in zip(genset * 3, generalization_results):
        acronym = utils.generate_acronym(gen)
        compiled_results[parse['model_state']][gen['dative']][acronym].append(float(parse['score']))

    # get means
    

    if forcsv:
        means = []
        for state, datives in compiled_results.items():
            for dative, hyps in datives.items():
                for hyp, scores in hyps.items():
                    mean = sum(scores)/len(scores)
                    means.append(tuple(exp_metadata.values()) + (adaptation_feature_config, state, dative, hyp, mean))
    else:
        means = {
            **exp_metadata,
            'adaptation_feature_config': adaptation_feature_config,
            'initial': {'do': {}, 'pp': {}},
            'final': {'do': {}, 'pp': {}},
            'best': {'do': {}, 'pp': {}}
        }
        for state, datives in compiled_results.items():
            for dative, hyps in datives.items():
                for hyp, scores in hyps.items():
                    means[state][dative][hyp] = sum(scores) / len(scores)

    return means

def flatten_results(result):
    # item_id, hypothesis_id, hypothesis_instance, lr, adaptation_dative, val_performance, best_epoch, train_loss, adaptation_feature_config, state, dative, hypothesis, mean
    flattened = []
    for state in ['initial', 'final', 'best']:
        for dative, hyps in result[state].items():
            for hyp, mean in hyps.items():
                flattened.append((result['item_id'], result['hypothesis_id'], result['hypothesis_instance'], result['lr'], result['adaptation_dative'], result['val_performance'], result['best_epoch'], result['train_loss'], result['adaptation_feature_config'], state, dative, hyp, mean))

    return flattened


def main(args):
    generalization = utils.read_jsonl("data/experiments/single_stimuli_dative_simulation/generalization.jsonl")
    adaptation = utils.read_jsonl("data/experiments/single_stimuli_dative_simulation/adaptation.jsonl")

    adapt2acro = {}
    for entry in adaptation:
        adapt2acro[entry['hypothesis_id']] = utils.generate_acronym(entry) 

    results_dir = args.results_dir

    results = defaultdict(list)

    i = 0
    j = 0
    for dir in tqdm(pathlib.Path(results_dir).glob("*")):
        if dir.is_dir():
            # print(dir)
            try:
                parsed = compile_generalization_results(str(dir), generalization, adapt2acro, False)
                results[(parsed['item_id'], parsed['hypothesis_id'], parsed['hypothesis_instance'])].append((parsed['lr'], parsed['val_performance'], parsed))
                # parsed = compile_generalization_results(str(dir), forcsv=True)
                # results[(parsed[0][0], parsed[0][1], parsed[0][2])].append((parsed[0][3], parsed[0][5], parsed))
                i += 1
            except:
                j += 1

    results = dict(results)

    print(f"Found {i} results")
    # print(f"Failed to parse {j} results")

    best_results = []
    for key, value in results.items():
        lr, val, gen_res = max(value, key=lambda x: x[1])
        best_results.extend(flatten_results(gen_res))
    
    print(f"Total results: {len(best_results)}")

    with open(f"{results_dir.replace('/raw', '')}/best_lr_results_hypwise.csv", "w") as f:
        writer = csv.writer(f)
        writer.writerow(
            [
                "item_id",
                "hypothesis_id",
                "hypothesis_instance",
                "lr",
                "adaptation_dative",
                "val_performance",
                "best_epoch",
                "train_loss",
                "adaptation_feature_config",
                "state",
                "generalization_dative",
                "generalization_feature_config",
                "logprob"
            ]
        )
        writer.writerows(best_results)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--results_dir",
        type=str,
        default="data/results/single_stimuli_dative_simulation/smolm-autoreg-bpe-seed_111/raw",
    )
    args = parser.parse_args()

    main(args)