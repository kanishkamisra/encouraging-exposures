import math
import operator
import pathlib
import random
import utils

from collections import defaultdict, Counter
from functools import reduce
from itertools import product, cycle, islice
from ordered_set import OrderedSet
from string import Template


prenom = utils.read_json("data/new-lexicon/prenom-modifiers.json")
pp = utils.read_json("data/new-lexicon/pp-modifiers.json")
lexicon = utils.read_csv_dict("data/new-lexicon/datives-lexicon-new.csv")

do_template = Template("$agent [verb] $recipient $theme .")
po_template = Template("$agent [verb] $theme to $recipient .")

N = 8


def prenomify(key, form):
    modifiers = prenom[key]
    modified = []
    for m in modifiers:
        modified.append(f"{m} {form}")

    return modified


def ppmodify(key, form):
    modifiers = pp[key]
    modified = []
    for m in modifiers:
        modified.append(f"{form} {m}")

    return modified


def possible_definiteness_forms(entry):
    return {
        "singular": [
            (f"the {entry['singular']}", "definite"),
            (f"a {entry['singular']}", "indefinite"),
        ],
        "plural": [
            (f"the {entry['plural']}", "definite"),
            (f"some {entry['plural']}", "indefinite"),
        ],
    }


def definiteness_forms(word, number):
    if number == "singular":
        return [(f"a {word}", "indefinite"), (f"the {word}", "definite")]
    else:
        return [(f"some {word}", "indefinite"), (f"the {word}", "definite")]


definiteness = {"definite": OrderedSet(), "indefinite": OrderedSet()}
animacy = {"animate": OrderedSet(), "inanimate": OrderedSet()}
pronominality = {"pronoun": OrderedSet(), "noun": OrderedSet()}
unique_args = OrderedSet()
form2lemma = {}
lemma2forms = defaultdict(list)

for entry in lexicon:
    lemma, singular, plural, anim, defness, pronom = (
        entry["lemma"],
        entry["singular"],
        entry["plural"],
        entry["animacy"],
        entry["definiteness"],
        entry["pronominality"],
    )
    # add all the cases that are not supposed
    # to be modified and have fixed definiteness
    if entry["definiteness_flexibility"] == "fixed":
        definiteness[defness].add(singular)
        animacy[anim].add(singular)
        pronominality[pronom].add(singular)
        unique_args.add(singular)
        form2lemma[singular] = lemma
        lemma2forms[lemma].append(singular)
    else:
        defs = possible_definiteness_forms(entry)
        # add the unmodified forms first
        for k, v in defs.items():
            for form, definess in v:
                definiteness[definess].add(form)
                animacy[anim].add(form)
                pronominality[pronom].add(form)
                unique_args.add(form)
                form2lemma[form] = lemma
                lemma2forms[lemma].append(form)

        # modify and then add definiteness
        all_modified_singular = []
        all_modified_plural = []

        # basic prenom modification
        all_modified_singular.extend(prenomify(lemma, singular))
        all_modified_plural.extend(
            prenomify(lemma, plural)
        )  # comment out if we only want singular.

        try:
            # basic pp modification (I think singular only for these)
            all_modified_singular.extend(ppmodify(lemma, singular))
        except:
            continue

        # # combine the two
        # for entry in prenomify(lemma, singular):
        #     try:
        #         pped = ppmodify(lemma, entry)
        #         all_modified_singular.extend(pped)
        #     except:
        #         continue

        # add definiteness to all modified
        for entry in all_modified_singular:
            for form, definess in definiteness_forms(entry, "singular"):
                definiteness[definess].add(form)
                animacy[anim].add(form)
                pronominality[pronom].add(form)
                unique_args.add(form)
                form2lemma[form] = lemma
                lemma2forms[lemma].append(form)

        for entry in all_modified_plural:
            for form, definess in definiteness_forms(entry, "plural"):
                definiteness[definess].add(form)
                animacy[anim].add(form)
                pronominality[pronom].add(form)
                unique_args.add(form)
                form2lemma[form] = lemma
                lemma2forms[lemma].append(form)

lemma2forms = dict(lemma2forms)


pronominality_features = pronominality.keys()
animacy_features = animacy.keys()
definiteness_features = definiteness.keys()

arg_lengths = OrderedSet([len(item.split(" ")) for item in unique_args])
length_bins = sorted(
    OrderedSet([first - second for first, second in product(arg_lengths, arg_lengths)])
)

single_combo = list(
    product(pronominality_features, animacy_features, definiteness_features)
)

argument_combos = list(product(single_combo, single_combo))

# len(single_combo), len(argument_combos)

all_features = dict()
for feature_set in [pronominality, animacy, definiteness]:
    for k, v in feature_set.items():
        all_features[k] = v


def feature_intersection(features: tuple):
    items = [all_features[f] for f in features]
    intersected = OrderedSet.intersection(*items)

    return intersected


def feature_string(features):
    string = "".join([f[0] for f in features])
    return string


random.seed(1024)

unique_combos = 0
string2combo = defaultdict(list)

samples = defaultdict(list)
for theme_features, recipient_features in argument_combos:

    string = f"{feature_string(theme_features)}{feature_string(recipient_features)}"
    string2combo[string] = [theme_features, recipient_features]

    theme_features = feature_intersection(theme_features)
    recipient_features = feature_intersection(recipient_features)

    pairs = product(theme_features, recipient_features)
    initial_sampling_space = defaultdict(list)

    for item1, item2 in pairs:
        if item1 != item2:  # eliminate the obvious
            lemma1, lemma2 = form2lemma[item1], form2lemma[item2]

            if lemma1 != lemma2:
                if not (
                    (lemma1 == "me" and lemma2 == "us")
                    or (lemma1 == "us" and lemma2 == "me")
                ):
                    length_diff = len(item1.split(" ")) - len(item2.split(" "))
                    initial_sampling_space[length_diff].append((item1, item2))
    initial_sampling_space = dict(initial_sampling_space)

    for k, v in initial_sampling_space.items():
        key = str(k)
        sampled = random.sample(v, min(N, len(v)))
        if len(sampled) < N:
            sampled = list(islice(cycle(sampled), N))
        samples[f"{string}_{k}"] = sampled

    unique_combos += len(initial_sampling_space)

string2combo = dict(string2combo)


hims = ["Ross", "Joseph", "Ethan", "Peter", "Thomas"]  # sample some names
hers = ["Lily", "Nina", "Eve", "Catherine", "Sally"]  # sample some names
thems_animate = ["those people", "the children", "the birds", "the ducks", "the pigs"]
thems_inanimate = [
    "the funny pictures",
    "the crayons",
    "the photographs",
    "the candies",
    "the socks",
]
its = [
    "the picture",
    "the milk",
    "the paper",
    "the apple",
    "the coffee",
]  # sample some object names

given_items = {
    "him": hims,
    "her": hers,
    "them-animate": thems_animate,
    "them-inanimate": thems_inanimate,
    "it": its,
}

# prior mention instead of "given"
given_templates = {
    1: {
        "agent-only": Template("Do you see $agent ?"),
        "agent-1arg": Template("Do you see $agent and $arg1 ?"),
        "agent-2arg": Template("Do you see $agent and $arg1 and $arg2 ?"),
    },
    2: {
        "agent-only": Template("Look it's $agent !"),
        "agent-1arg": Template("Look it's $agent and $arg1 !"),
        "agent-2arg": Template("Look it's $agent and $arg1 and $arg2 !"),
    },
    3: {
        "agent-only": Template("Here's $agent !"),
        "agent-1arg": Template("Here's $agent with $arg1 !"),
        "agent-2arg": Template("Here's $agent with $arg1 and $arg2 !"),
    },
}


agents = ["Laura", "Mark", "Sarah", "William", "Alex"]

agents = list(islice(cycle(agents), N))

raw_stimuli_no_given = []
raw_stimuli = []
idx = 1

for h_id, (combo, items) in enumerate(samples.items()):
    random.shuffle(agents)

    # different givenness orders when both are given:
    theme_recipient = random.sample(range(1, N+1), int(N/2))
    recipient_theme = [x for x in range(1, N+1) if x not in theme_recipient]

    for h_item, (agent, item) in enumerate(zip(agents, items)):
        # raw_stimuli.append(item)
        theme, recipient = item

        do_sentence = do_template.substitute(
            agent=agent, theme=item[0], recipient=item[1]
        )
        po_sentence = po_template.substitute(
            agent=agent, theme=item[0], recipient=item[1]
        )

        do_sentence = do_sentence.replace("them-a", "them")
        po_sentence = po_sentence.replace("them-a", "them")
        do_sentence = do_sentence.replace("them-i", "them")
        po_sentence = po_sentence.replace("them-i", "them")

        string, length_diff = combo.split("_")
        length_diff = int(length_diff)
        theme_features, recipient_features = string2combo[string]

        raw_stimuli_no_given.append(
            {
                "item": idx,
                "hypothesis_id": h_id + 1,
                "hypothesis_item": h_item + 1,
                "combo": combo,
                "agent": agent,
                "theme": item[0],
                "recipient": item[1],
                "do_sentence": do_sentence,
                "po_sentence": po_sentence,
                "theme_pronominality": theme_features[0],
                "theme_animacy": theme_features[1],
                "theme_definiteness": theme_features[2],
                "recipient_pronominality": recipient_features[0],
                "recipient_animacy": recipient_features[1],
                "recipient_definiteness": recipient_features[2],
                "length_diff": length_diff,
            }
        )

        for template_num, type_templates in given_templates.items():
            for typ, template in type_templates.items():
                if typ == "agent-only":
                    prefix = template.substitute(agent=agent)
                    raw_stimuli.append(
                        {
                            "template_id": template_num,
                            "item": idx,
                            "hypothesis_id": h_id + 1,
                            "hypothesis_item": h_item + 1,
                            "template_type": "agent-only",
                            "template": typ,
                            "combo": combo,
                            "agent": agent,
                            "theme": item[0],
                            "recipient": item[1],
                            "prefix": prefix,
                            "do_sentence": do_sentence,
                            "po_sentence": po_sentence,
                            "theme_pronominality": theme_features[0],
                            "theme_animacy": theme_features[1],
                            "theme_definiteness": theme_features[2],
                            "recipient_pronominality": recipient_features[0],
                            "recipient_animacy": recipient_features[1],
                            "recipient_definiteness": recipient_features[2],
                            "length_diff": length_diff,
                        }
                    )
                elif typ == "agent-1arg":
                    # theme: theme def recipient indef
                    # theme: theme def recipient def
                    if theme_features[2] == "definite":
                        if theme_features[0] == "pronoun":
                            if theme in ["him", "her", "them-a", "them-i", "it"]:
                                if theme == "them-a":
                                    theme_arg = "them-animate"
                                elif theme == "them-i":
                                    theme_arg = "them-inanimate"
                                else:
                                    theme_arg = theme
                                given_theme = random.sample(given_items[theme_arg], 1)[
                                    0
                                ]
                            else:
                                given_theme = theme
                        else:
                            given_theme = theme
                        prefix = template.substitute(agent=agent, arg1=given_theme)
                        raw_stimuli.append(
                            {
                                "template_id": template_num,
                                "item": idx,
                                "hypothesis_id": h_id + 1,
                                "hypothesis_item": h_item + 1,
                                "template_type": "agent-1arg",
                                "template": "agent-theme",
                                "combo": combo,
                                "agent": agent,
                                "theme": item[0],
                                "recipient": item[1],
                                "prefix": prefix,
                                "do_sentence": do_sentence,
                                "po_sentence": po_sentence,
                                "theme_pronominality": theme_features[0],
                                "theme_animacy": theme_features[1],
                                "theme_definiteness": theme_features[2],
                                "recipient_pronominality": recipient_features[0],
                                "recipient_animacy": recipient_features[1],
                                "recipient_definiteness": recipient_features[2],
                                "length_diff": length_diff,
                            }
                        )

                    # recipient: theme def recipient def
                    # recipient: theme indef recipient def
                    if recipient_features[2] == "definite":
                        if recipient_features[0] == "pronoun":
                            if recipient in ["him", "her", "them-a", "them-i", "it"]:
                                if recipient == "them-a":
                                    recipient_arg = "them-animate"
                                elif recipient == "them-i":
                                    recipient_arg = "them-inanimate"
                                else:
                                    recipient_arg = recipient
                                given_recipient = random.sample(
                                    given_items[recipient_arg], 1
                                )[0]
                            else:
                                given_recipient = recipient
                        else:
                            given_recipient = recipient
                        prefix = template.substitute(agent=agent, arg1=given_recipient)
                        raw_stimuli.append(
                            {
                                "template_id": template_num,
                                "item": idx,
                                "hypothesis_id": h_id + 1,
                                "hypothesis_item": h_item + 1,
                                "template_type": "agent-1arg",
                                "template": "agent-recipient",
                                "combo": combo,
                                "agent": agent,
                                "theme": item[0],
                                "recipient": item[1],
                                "prefix": prefix,
                                "do_sentence": do_sentence,
                                "po_sentence": po_sentence,
                                "theme_pronominality": theme_features[0],
                                "theme_animacy": theme_features[1],
                                "theme_definiteness": theme_features[2],
                                "recipient_pronominality": recipient_features[0],
                                "recipient_animacy": recipient_features[1],
                                "recipient_definiteness": recipient_features[2],
                                "length_diff": length_diff,
                            }
                        )
                elif typ == "agent-2arg":
                    # half = theme and recipient
                    # half = recipient and theme
                    if (
                        theme_features[2] == "definite"
                        and recipient_features[2] == "definite"
                    ):
                        if theme_features[0] == "pronoun":
                            if theme in ["him", "her", "them-a", "them-i", "it"]:
                                if theme == "them-a":
                                    theme_arg = "them-animate"
                                elif theme == "them-i":
                                    theme_arg = "them-inanimate"
                                else:
                                    theme_arg = theme
                                given_theme = random.sample(given_items[theme_arg], 1)[
                                    0
                                ]
                            else:
                                given_theme = theme
                        else:
                            given_theme = theme

                        if recipient_features[0] == "pronoun":
                            if recipient in ["him", "her", "them-a", "them-i", "it"]:
                                if recipient == "them-a":
                                    recipient_arg = "them-animate"
                                elif recipient == "them-i":
                                    recipient_arg = "them-inanimate"
                                else:
                                    recipient_arg = recipient
                                given_recipient = random.sample(
                                    given_items[recipient_arg], 1
                                )[0]
                            else:
                                given_recipient = recipient
                        else:
                            given_recipient = recipient

                        # -- actual stimuli
                        if h_item + 1 in theme_recipient:
                            prefix = template.substitute(
                                agent=agent, arg1=given_theme, arg2=given_recipient
                            )
                            raw_stimuli.append(
                                {
                                    "template_id": template_num,
                                    "item": idx,
                                    "hypothesis_id": h_id + 1,
                                    "hypothesis_item": h_item + 1,
                                    "template_type": "agent-2arg",
                                    "template": "agent-theme-recipient",
                                    "combo": combo,
                                    "agent": agent,
                                    "theme": item[0],
                                    "recipient": item[1],
                                    "prefix": prefix,
                                    "do_sentence": do_sentence,
                                    "po_sentence": po_sentence,
                                    "theme_pronominality": theme_features[0],
                                    "theme_animacy": theme_features[1],
                                    "theme_definiteness": theme_features[2],
                                    "recipient_pronominality": recipient_features[0],
                                    "recipient_animacy": recipient_features[1],
                                    "recipient_definiteness": recipient_features[2],
                                    "length_diff": length_diff,
                                }
                            )
                        else:
                            prefix = template.substitute(
                                agent=agent, arg1=given_recipient, arg2=given_theme
                            )
                            raw_stimuli.append(
                                {
                                    "template_id": template_num,
                                    "item": idx,
                                    "hypothesis_id": h_id + 1,
                                    "hypothesis_item": h_item + 1,
                                    "template_type": "agent-2arg",
                                    "template": "agent-recipient-theme",
                                    "combo": combo,
                                    "agent": agent,
                                    "theme": item[0],
                                    "recipient": item[1],
                                    "prefix": prefix,
                                    "do_sentence": do_sentence,
                                    "po_sentence": po_sentence,
                                    "theme_pronominality": theme_features[0],
                                    "theme_animacy": theme_features[1],
                                    "theme_definiteness": theme_features[2],
                                    "recipient_pronominality": recipient_features[0],
                                    "recipient_animacy": recipient_features[1],
                                    "recipient_definiteness": recipient_features[2],
                                    "length_diff": length_diff,
                                }
                            )

        # raw_stimuli.append((idx, h_id+1, h_item+1, combo, agent, item[0], item[1], do_sentence, po_sentence))
        idx += 1


print(f"Total Unique Feature Combinations: {unique_combos}")
print(f"Total stimuli (no-givenness): {len(raw_stimuli_no_given)}")
print(f"Total stimuli (givenness): {len(raw_stimuli)}")

# writing to files.

def write_stimuli(lst, path):
    stimuli = []
    idx = 0
    for entry in lst:
        stimuli.append({
            "idx": idx+1,
            "template_id": entry['template_id'],
            "item": entry['item'],
            "dative": "do",
            "hypothesis_id": entry["hypothesis_id"],
            "hypothesis_item": entry["hypothesis_item"],
            "template_type": entry['template_type'],
            "template": entry["template"],
            "combo": entry["combo"],
            "stimulus": f"{entry['prefix']}\n<s> {entry['do_sentence']}",
            "agent": entry["agent"],
            "theme": entry["theme"],
            "recipient": entry["recipient"],
            "theme_pronominality": entry["theme_pronominality"],
            "theme_animacy": entry["theme_animacy"],
            "theme_definiteness": entry["theme_definiteness"],
            "recipient_pronominality": entry["recipient_pronominality"],
            "recipient_animacy": entry["recipient_animacy"],
            "recipient_definiteness": entry["recipient_definiteness"],
            "length_diff": entry["length_diff"],
        })

        stimuli.append({
            "idx": idx+2,
            "template_id": entry['template_id'],
            "item": entry['item'],
            "dative": "pp",
            "hypothesis_id": entry["hypothesis_id"],
            "hypothesis_item": entry["hypothesis_item"],
            "template_type": entry['template_type'],
            "template": entry["template"],
            "combo": entry["combo"],
            "stimulus": f"{entry['prefix']}\n<s> {entry['po_sentence']}",
            "agent": entry["agent"],
            "theme": entry["theme"],
            "recipient": entry["recipient"],
            "theme_pronominality": entry["theme_pronominality"],
            "theme_animacy": entry["theme_animacy"],
            "theme_definiteness": entry["theme_definiteness"],
            "recipient_pronominality": entry["recipient_pronominality"],
            "recipient_animacy": entry["recipient_animacy"],
            "recipient_definiteness": entry["recipient_definiteness"],
            "length_diff": entry["length_diff"],
        })

        idx+=2

    utils.write_jsonl(stimuli, file_path=path)


template_stimuli = defaultdict(list)
for entry in raw_stimuli:
    template_stimuli[entry['template_id']].append(entry)

pathlib.Path("data/experiments/final/").mkdir(exist_ok=True, parents=True)

for k,v in template_stimuli.items():
    write_stimuli(v, path = f"data/experiments/final/givenness_template_{k}.jsonl")

