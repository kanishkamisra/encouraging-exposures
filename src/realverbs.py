import argparse
import csv
import json
import pathlib
import pyinflect
import spacy
import utils

from minicons import scorer
from torch.utils.data import DataLoader
from tqdm import tqdm


def conjugate(verb, to="VBD", model=None):
    if model is not None:
        return model(verb)[0]._.inflect(to)
    else:
        raise ValueError("No model provided")


def main(args):
    verb_lemma_path = args.verb_lemma_path
    generalization_path = args.generalization_path
    model = args.model

    model_name = args.model.replace("kanishka/", "")

    lm = scorer.IncrementalLMScorer(model, args.device)

    generalization_set = utils.read_jsonl(generalization_path)

    nlp = spacy.load("en_core_web_sm")

    verb_lemmas = []
    with open(verb_lemma_path, "r") as f:
        reader = csv.DictReader(f)
        for row in reader:
            # verb_lemmas.append(row["lemma"])
            lemma = row["lemma"]
            verb = conjugate(lemma, to="VBD", model=nlp)
            verb_lemmas.append((lemma, verb))

    print("Infilling dative generalization set with real verbs")
    corpus = []
    for lemma, verb in tqdm(verb_lemmas):
        for instance in generalization_set:
            sentence = instance["sentence"].replace("[verb]", verb)
            corpus.append(
                {
                    "hypothesis_id": instance["hypothesis_id"],
                    "hypothesis_instance": instance["hypothesis_instance"],
                    "dative": instance["dative"],
                    "sentence": sentence,
                    "lemma": lemma,
                    "verb": verb,
                }
            )

    corpus_dl = DataLoader(corpus, batch_size=args.batch_size, shuffle=False)

    scores = []
    for batch in tqdm(corpus_dl):
        batch_scores = lm.sequence_score(batch["sentence"])
        scores.extend(batch_scores)

    for i, instance in enumerate(corpus):
        instance["score"] = scores[i]

    save_path = f"data/results/real_verbs/{model_name}"
    pathlib.Path(save_path).mkdir(parents=True, exist_ok=True)

    with open(f"{save_path}/logprobs.jsonl", "w") as f:
        for line in corpus:
            f.write(json.dumps(line) + "\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--verb_lemma_path", type=str, default="data/dative_lemmas.csv")
    parser.add_argument(
        "--generalization_path",
        type=str,
        default="data/experiments/single_stimuli_dative_simulation/generalization.jsonl",
    )
    parser.add_argument(
        "--model", type=str, default="kanishka/smolm-autoreg-bpe-seed_111"
    )
    parser.add_argument("--device", type=str, default="cpu")
    parser.add_argument("--batch_size", type=int, default=100)
    args = parser.parse_args()
    main(args)
