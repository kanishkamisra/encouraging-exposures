"""
Experiment logic for testing the extent to which LMs trained on one form of dative are able to generalize to the other form.
"""

import argparse
import json
import pathlib
import utils

from transformers import set_seed

from experiment import Learner, Trainer


def main(args):
    seed = args.seed
    set_seed(seed)

    adaptation_set = utils.read_jsonl(
        "data/experiments/single_stimuli_dative_simulation/adaptation.jsonl"
    )
    generalization_set = utils.read_jsonl(
        "data/experiments/single_stimuli_dative_simulation/generalization.jsonl"
    )
    validation_set = utils.read_json(args.validation_path)

    pathlib.Path(args.results_dir).mkdir(parents=True, exist_ok=True)

    model_name = args.model_name.replace("kanishka/", "")

    # load adaptation
    # for each instance, run training --> save model.

    for instance in adaptation_set:
        # do, pp = instance["do"], instance["pp"]
        dative = instance["dative"]
        experiment_id = utils.generate_acronym(instance)
        # for condition, training_sentence in zip(["do", "pp"], [do, pp]):
        # for training_sentence in instance["sentence"]:
        training_sentence = instance["sentence"]
        print("====" * 20)
        print(
            f"Experiment: {experiment_id}, Instance: {instance['hypothesis_instance']}, Condition: {dative}"
        )
        print("====" * 20)
        learner = Learner(args.model_name, args.device, args.gaussian)
        learner.add_tokens()

        trainer = Trainer(
            learner,
            [training_sentence],
            generalization_set,
            validation_set,
            "diff",
            args.lr,
        )
        trainer.train(args.epochs, args.batch_size)

        # create results_dir
        results_dir = f"{args.results_dir}/{model_name}/raw/{instance['item']}_{instance['hypothesis_id']}_{instance['hypothesis_instance']}_{args.lr}_{dative}_results"
        pathlib.Path(results_dir).mkdir(parents=True, exist_ok=True)
        trainer.save_results(results_dir)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument(
        "--model_name", type=str, default="kanishka/smolm-autoreg-bpe-seed_111"
    )
    parser.add_argument("--device", type=str, default="cuda:0")
    parser.add_argument("--gaussian", action="store_true")
    parser.add_argument(
        "--results_dir",
        type=str,
        default="data/results/single_stimuli_dative_simulation/",
    )
    parser.add_argument(
        "--validation_path", type=str, default="data/experiments/validation.json"
    )
    parser.add_argument("--batch_size", type=int, default=32)
    parser.add_argument("--epochs", type=int, default=70)
    parser.add_argument("--lr", type=float, default=0.001)

    args = parser.parse_args()
    main(args)


# load training data.

# add novel token(s)? specified by user.

# set up training. adaptation, generalization set

# get development set performance -- some scalar based on 2 inputs. either minimal pairwise setting or diff betweeen avg log prob on both inputs. report this for all runs.

# test set metrics, based on condition specified in the arguments. basically, given a corpus, return the avg log prob of the corpus under w/ the model. report this for (1) prior to adding novel token, (2) best model based on dev set, (3) state of the model at the end of training.
