import argparse
import config

# import inflect
import json
import math

# import os
import random
import utils
import pathlib

from collections import defaultdict
from string import Template
from dataclasses import dataclass
from itertools import product
from ordered_set import OrderedSet


@dataclass
class Dative:
    dative: str
    verb: str
    agent: str
    theme: str
    recipient: str

    def generate(self, marked_theme=False, marked_recipient=False):
        if self.dative == "do":
            template = Template("$agent $verb $recipient $theme .")
        elif self.dative == "pp":
            template = Template("$agent $verb $theme to $recipient .")

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


def plausibility_splits(
    feature_combinations,
    implausible_combinations=config.IMPLAUSIBLE,
    adaptation=True,
):
    plausibility = {
        "do": {"plausible": [], "implausible": []},
        "pp": {"plausible": [], "implausible": []},
    }
    for dative in ["do", "pp"]:
        for fc in feature_combinations:
            fc_id = utils.generate_acronym_tuple(fc)
            if not adaptation:
                if fc_id in implausible_combinations[dative]:
                    plausibility[dative]["implausible"].append(fc_id)
                else:
                    plausibility[dative]["plausible"].append(fc_id)
            else:
                plausibility[dative]["plausible"].append(fc_id)
    return plausibility


def specify_sample_size(plausibility, N=20):
    sample_sizes = {"do": defaultdict(int), "pp": defaultdict(int)}
    amts = []
    for dative, splits in plausibility.items():
        n_plausible, n_implausible = len(splits["plausible"]), len(
            splits["implausible"]
        )

        addition = n_implausible * N / n_plausible
        plausible_amt = int(n_plausible * math.floor(N + addition))
        amts.append(plausible_amt)

        # print(dative, plausible_amt)

        for acronym in splits["plausible"]:
            sample_sizes[dative][acronym] = math.floor(N + addition)
        for acronym in splits["implausible"]:
            sample_sizes[dative][acronym] = 0
        sample_sizes[dative] = dict(sample_sizes[dative])
    return sample_sizes, amts


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
        if sampled_recipient in config.CONFLICTS.keys():
            conflict_set = conflict_set.union(
                OrderedSet(config.CONFLICTS[sampled_recipient])
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

    # generate agents separately
    # remove all themes and recipients, then the conflict set
    agent_conflicts = OrderedSet()
    for sampled_theme, sampled_recipient in zip(sampled_themes, sampled_recipients):
        conflict_set = OrderedSet(
            config.CONFLICTS[sampled_theme]
            if sampled_theme in config.CONFLICTS.keys()
            else []
        )
        conflict_set = conflict_set.union(
            OrderedSet(config.CONFLICTS[sampled_recipient])
            if sampled_recipient in config.CONFLICTS.keys()
            else []
        )
        agent_conflicts.update(
            OrderedSet([sampled_theme] + [sampled_recipient]).union(conflict_set)
        )

    agent_space = agents - agent_conflicts
    # print(
    #     "themes",
    #     sampled_themes,
    #     "recipients",
    #     sampled_recipients,
    #     "agents space",
    #     agent_space,
    # )
    if len(agent_space) < N:
        sampled_agents = random.choices(list(agent_space), k=N)
    else:
        sampled_agents = random.sample(list(agent_space), N)

    return sampled_agents, sampled_themes, sampled_recipients


# def generate_dative_set(lexicon, feature_combinations, N):
#     dative_set = []
#     for i, fc in enumerate(feature_combinations):
#         feature_space = generate_feature_space(fc, lexicon)
#         sampled_items = sample_items(*feature_space, N)

#         for j, (a, t, r) in enumerate(zip(*sampled_items)):
#             do_dative = Dative("do", "[verb]", a, t, r).generate()
#             pp_dative = Dative("pp", "[verb]", a, t, r).generate()
#             dative_set.append(
#                 {
#                     "item": len(dative_set) + 1,
#                     "hypothesis_id": i + 1,
#                     "hypothesis_instance": j + 1,
#                     "theme_pronominality": fc[0][0],
#                     "theme_animacy": fc[0][1],
#                     "theme_length": fc[0][2],
#                     "recipient_pronominality": fc[1][0],
#                     "recipient_animacy": fc[1][1],
#                     "recipient_length": fc[1][2],
#                     "agent": a,
#                     "theme": t,
#                     "recipient": r,
#                     "do": do_dative,
#                     "pp": pp_dative,
#                 }
#             )
#     return dative_set


def generate_dative_set(lexicon, feature_combinations, sample_sizes):
    dative_set = []
    for i, fc in enumerate(feature_combinations):
        fc_id = utils.generate_acronym_tuple(fc)
        # if fc_id != "paspas":
        #     break
        feature_space = generate_feature_space(fc, lexicon)
        N = max(sample_sizes["do"][fc_id], sample_sizes["pp"][fc_id])
        if N == 0:
            pass
        else:
            sampled_items = sample_items(*feature_space, N)
            j = 0
            for dative in ["do", "pp"]:
                # items = sampled_items[: sample_sizes[dative][fc_id]]
                items = [
                    argument[: sample_sizes[dative][fc_id]]
                    for argument in sampled_items
                ]
                # print(dative, len(items))
                # for j, (a, t, r) in enumerate(zip(*items)):
                for agent, theme, recipient in zip(*items):
                    do_dative = Dative(
                        dative, "[verb]", agent, theme, recipient
                    ).generate()
                    # pp_dative = Dative("pp", "[verb]", a, t, r).generate()
                    dative_set.append(
                        {
                            "item": len(dative_set) + 1,
                            "hypothesis_id": i + 1,
                            "hypothesis_instance": j + 1,
                            "theme_pronominality": fc[0][0],
                            "theme_animacy": fc[0][1],
                            "theme_length": fc[0][2],
                            "recipient_pronominality": fc[1][0],
                            "recipient_animacy": fc[1][1],
                            "recipient_length": fc[1][2],
                            "agent": agent,
                            "theme": theme,
                            "recipient": recipient,
                            "dative": dative,
                            "sentence": do_dative,
                        }
                    )
                    j += 1
                j = 0
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

    gen_plausible_splits = plausibility_splits(feature_combinations, adaptation=False)
    adapt_plausible_splits = plausibility_splits(feature_combinations, adaptation=True)

    print(
        "Generalization:",
        len(gen_plausible_splits["do"]["plausible"]),
        len(gen_plausible_splits["pp"]["plausible"]),
    )
    print(
        "Adaptation:",
        len(adapt_plausible_splits["do"]["plausible"]),
        len(adapt_plausible_splits["pp"]["plausible"]),
    )

    gen_sample_sizes, gen_amt = specify_sample_size(
        gen_plausible_splits, args.generalization_size
    )
    adapt_sample_sizes, adapt_amt = specify_sample_size(
        adapt_plausible_splits, args.adaptation_size
    )

    # print(gen_sample_sizes, gen_amt)
    # print(adapt_sample_sizes, adapt_amt)

    # sample_sizes = {"do": defaultdict(int), "pp": defaultdict(int)}
    # for dative in ["do", "pp"]:
    #     for fc in feature_combinations:
    #         fc_id = utils.generate_acronym_tuple(fc)
    #         if fc_id in config.IMPLAUSIBLE[dative]:
    #             sample_sizes.

    # generate generalization set -- 10 items per feature combination? = 350 items
    generalization_set = generate_dative_set(
        generalization_lexicon, feature_combinations, gen_sample_sizes
    )

    # print(len(generalization_set))

    # # adaptation set time
    adaptation_set = generate_dative_set(
        adaptation_lexicon, feature_combinations, adapt_sample_sizes
    )

    # print(len(adaptation_set))

    # make exp dir
    exp_dir = f"data/experiments/{args.experiment_name}"
    pathlib.Path(exp_dir).mkdir(parents=True, exist_ok=True)

    # write to jsonl file
    with open(f"{exp_dir}/adaptation.jsonl", "w") as f:
        for item in adaptation_set:
            f.write(json.dumps(item) + "\n")

    adaptation_sentences = utils.reorganize_sentences(adaptation_set)

    pathlib.Path("data/experiments/single_stimuli_dative_simulation/sentences").mkdir(
        parents=True, exist_ok=True
    )
    with open(
        f"data/experiments/{args.experiment_name}/sentences/adaptation.json", "w"
    ) as f:
        json.dump(adaptation_sentences, f, indent=4)

    if not args.nogen:
        with open(f"{exp_dir}/generalization.jsonl", "w") as f:
            for item in generalization_set:
                f.write(json.dumps(item) + "\n")

        generalization_sentences = utils.reorganize_sentences(generalization_set)

        with open(
            f"data/experiments/{args.experiment_name}/sentences/generalization.json",
            "w",
        ) as f:
            json.dump(generalization_sentences, f, indent=4)

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
    parser.add_argument("--nogen", action="store_true")
    parser.add_argument("--adaptation_size", type=int, default=5)
    parser.add_argument("--generalization_size", type=int, default=20)
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument(
        "--experiment_name", type=str, default="single_stimuli_dative_simulation"
    )

    args = parser.parse_args()

    main(args)
