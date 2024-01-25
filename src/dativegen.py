import argparse
import config
# import inflect
import json
# import math
# import os
import random
import pathlib

from string import Template
from dataclasses import dataclass
from itertools import product
from ordered_set import OrderedSet
# from collections import defaultdict


@dataclass
class Dative:
    dative: str
    verb: str
    agent: str
    theme: str
    recipient: str

    def generate(self, marked_theme=False, marked_recipient=False):
        if self.dative == "do":
            template = Template("$agent $verb $recipient $theme.")
        elif self.dative == "pp":
            template = Template("$agent $verb $theme to $recipient.")

        if marked_theme:
            self.theme = f"the {self.theme}"

        if marked_recipient:
            self.recipient = f"the {self.recipient}"

        self.sentence = template.substitute(
            agent=self.agent, verb=self.verb, theme=self.theme, recipient=self.recipient
        )
        return self.sentence

    def givenness(self, discourse_sentence=None):
        return NotImplementedError


def read_lexicon(path):
    with open(path, "r") as f:
        lexicon = json.load(f)
        lexicon = {k: OrderedSet(v) for k, v in lexicon.items()}
        long = OrderedSet(
            [
                x
                for x in lexicon["animate"].union(lexicon["inanimate"])
                if len(x.split(" ")) > 2
            ]
        )
        short = OrderedSet(
            [
                x
                for x in lexicon["animate"].union(lexicon["inanimate"])
                if len(x.split(" ")) <= 2
            ]
        )
        nominals = OrderedSet(
            [
                x
                for x in lexicon["animate"].union(lexicon["inanimate"])
                - lexicon["pronoun"]
            ]
        )
        lexicon.update({"long": long, "short": short, "nominal": nominals})
    return lexicon


def generate_feature_combinations(lex, features):
    feature_combinations = []
    for fc in product(features, features):
        theme_features, recipient_features = fc
        theme_features = [lex[feature] for feature in theme_features]
        recipient_features = [lex[feature] for feature in recipient_features]
        theme_features = OrderedSet.intersection(*theme_features)
        recipient_features = OrderedSet.intersection(*recipient_features)
        if len(theme_features) >= 1 and len(recipient_features) >= 1:
            if len(theme_features) == 1 and len(recipient_features) == 1:
                continue
            else:
                feature_combinations.append(fc)
    return feature_combinations


def generate_feature_space(feature_combo, lex):
    theme_features, recipient_features = feature_combo
    theme_features = [lex[feature] for feature in theme_features] + [lex["theme"]]
    recipient_features = [lex[feature] for feature in recipient_features] + [
        lex["recipient"]
    ]
    return (
        lex["agent"],
        OrderedSet.intersection(*theme_features),
        OrderedSet.intersection(*recipient_features),
    )


def sample_items(agents, themes, recipients, N):
    sampled_agents, sampled_themes, sampled_recipients = [], [], []
    for i in range(N):
        sampled_theme = random.choice(list(themes))

        conflict_set = OrderedSet(
            config.CONFLICTS[sampled_theme]
            if sampled_theme in config.CONFLICTS.keys()
            else []
        )
        # print(sampled_theme, conflict_set)
        recipient_space = recipients - OrderedSet([sampled_theme]) - conflict_set
        # print(recipient_space)
        sampled_recipient = random.choice(list(recipient_space))

        if sampled_theme in config.CONFLICTS.keys():
            conflict_set = conflict_set.union(
                OrderedSet(config.CONFLICTS[sampled_theme])
            )
        # print(sampled_theme, conflict_set)
        agent_space = (
            agents - OrderedSet([sampled_theme] + [sampled_recipient]) - conflict_set
        )
        sampled_agent = random.choice(list(agent_space))

        sampled_agents.append(sampled_agent)
        sampled_themes.append(sampled_theme)
        sampled_recipients.append(sampled_recipient)
        # print("")
    return sampled_agents, sampled_themes, sampled_recipients


def generate_dative_set(lexicon, feature_combinations, N):
    dative_set = []
    for i, fc in enumerate(feature_combinations):
        feature_space = generate_feature_space(fc, lexicon)
        sampled_items = sample_items(*feature_space, N)

        for a, t, r in zip(*sampled_items):
            do_dative = Dative("do", "[verbed]", a, t, r).generate()
            pp_dative = Dative("pp", "[verbed]", a, t, r).generate()
            dative_set.append(
                {
                    "item": len(dative_set) + 1,
                    "hypothesis_id": i + 1,
                    "theme_pronominality": fc[0][0],
                    "theme_animacy": fc[0][1],
                    "theme_length": fc[0][2],
                    "recipient_pronominality": fc[1][0],
                    "recipient_animacy": fc[1][1],
                    "recipient_length": fc[1][2],
                    "agent": a,
                    "theme": t,
                    "recipient": r,
                    "do": do_dative,
                    "pp": pp_dative,
                }
            )
    return dative_set


def main(args):
    seed = args.seed
    random.seed(seed)

    adaptation_lexicon = read_lexicon(args.adaptation_lexicon)
    generalization_lexicon = read_lexicon(args.generalization_lexicon)

    # full_lexicon = {
    #     k: adaptation_lexicon[k].union(generalization_lexicon[k])
    #     for k in adaptation_lexicon.keys()
    # }

    pronominality = ["pronoun", "nominal"]
    animacy = ["animate", "inanimate"]
    length = ["long", "short"]

    # generate all possible combinations of features for theme and recipient and then prune
    features = list(product(pronominality, animacy, length))

    feature_combinations = generate_feature_combinations(adaptation_lexicon, features)

    # generate generalization set -- 10 items per feature combination? = 350 items
    generalization_set = generate_dative_set(
        generalization_lexicon, feature_combinations, args.generalization_size
    )

    # adaptation set time
    adaptation_set = generate_dative_set(
        adaptation_lexicon, feature_combinations, args.adaptation_size
    )

    # make exp dir
    pathlib.Path("data/experiments").mkdir(parents=True, exist_ok=True)

    # write to jsonl file
    with open("data/experiments/adaptation.jsonl", "w") as f:
        for item in adaptation_set:
            f.write(json.dumps(item) + "\n")

    with open("data/experiments/generalization.jsonl", "w") as f:
        for item in generalization_set:
            f.write(json.dumps(item) + "\n")

    # TODO: save by hypothesis_id


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--adaptation_lexicon",
        type=str,
        default="data/lexicon/adaptation.json",
    )
    parser.add_argument(
        "--generalization_lexicon",
        type=str,
        default="data/lexicon/generalization.json",
    )
    parser.add_argument("--adaptation_size", type=int, default=5)
    parser.add_argument("--generalization_size", type=int, default=20)
    parser.add_argument("--seed", type=int, default=42)
    
    args = parser.parse_args()
    
    main(args)
