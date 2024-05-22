import json
import copy
import pathlib

import utils
import config

from collections import defaultdict
from dataclasses import dataclass
from itertools import product
from string import Template
from ordered_set import OrderedSet

import random

random.seed(42)

TEMPLATES = {
    1: "look there's {} with {} and {} .",
    2: "{} was with {} and {}.",
    3: "do you see {} with {} and {} ?"
}

def write_jsonl(data, path):
    with open(path, "w") as f:
        for d in data:
            f.write(json.dumps(d) + "\n")

def generate_givenness(agent, entity1, entity2, sentence, template):
    return f"{template.format(agent, entity1, entity2)} {sentence}"


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


# read OG data
og_adaptation = utils.read_jsonl("data/experiments/single_stimuli_dative_simulation_valtest_vbd_discourse_control/adaptation.jsonl")
generalization = utils.read_jsonl("data/experiments/single_stimuli_dative_simulation_valtest_vbd_no_markedness_discourse_control/generalization.jsonl")

unique_pairs = OrderedSet()
forbidden_pairs = OrderedSet()
forbidden_pairs.add(("the boy", "the girl"))
forbidden_pairs.add(("the girl", "the boy"))
sudha_adaptation = defaultdict(list)

for item in og_adaptation:
    if item["theme_definiteness"] == "definite" and item['recipient_definiteness'] == "definite" \
        and item['theme_length'] == "short" and item['recipient_length'] == "short" \
        and item['recipient_animacy'] == "animate":
        theme, recipient = item['theme'], item['recipient']
        if theme == "a lego":
            theme = "the lego"
        elif recipient == "a lego":
            recipient = "the lego"

        # exact sudha config: both have "the X"
        if "the " in theme and "the " in recipient:
            sentence = item['sentence'].split(".")[-2].strip() + " ."
            item['sentence'] = sentence
            sudha_adaptation[(item['hypothesis_id'], item['dative'])].append(item)
            unique_pairs.add((theme, recipient))


items = {
    "animate": ["the dog", "the cat", "the frog", "the bear", "the duck", "the sheep"],
    "inanimate": ["the lego", "the book", "the block", "the ball", "the cup", "the milk"]
}

# generate all possible pairs, discarding pairs with same theme and recipient, and pairs that occur in unique_pairs.
# store as (animate, inanimate) = set() for all possible animacy combinations

all_pairs = defaultdict(OrderedSet)
for animacy1 in items:
    for animacy2 in items:
        for pair in product(items[animacy1], items[animacy2]):
            if pair[0] != pair[1] and pair not in unique_pairs and pair not in forbidden_pairs:
                all_pairs[(animacy1, animacy2)].add(pair)


all_pairs = {k: list(v) for k, v in all_pairs.items()}

# print length for each animacy pair in all pairs
# for k, v in all_pairs.items():
#     v = list(v)
#     print(k, len(v), v[:10])

item_id = 3000
for k, v in sudha_adaptation.items():
    hypothesis_instance = v[-1]['hypothesis_instance']
    theme_animacy = v[-1]['theme_animacy']
    recipient_animacy = v[-1]['recipient_animacy']
    dative = v[-1]['dative']
    pairs = random.sample(all_pairs[(theme_animacy, recipient_animacy)], 25) # so that we have 30 total pairs in the exp
    
    # print(hypothesis_instance, v[-1]['theme_animacy'], v[-1]['recipient_animacy'])
    agents = []
    for item in v:
        agent = item['agent']
        agents.extend([agent] * 5)

    for pair, agent in zip(pairs, agents):
        obj = copy.deepcopy(v[-1])
        theme, recipient = pair
        # print(agent, theme, recipient)
        dative_obj = Dative(dative, "[verb]", agent, theme, recipient)
        obj['item'] = item_id
        obj['hypothesis_instance'] = hypothesis_instance + 1
        obj['agent'] = agent
        obj['theme'] = theme
        obj['recipient'] = recipient
        obj['sentence'] = dative_obj.generate()
        print(obj['sentence'])
        sudha_adaptation[k].append(obj)
        item_id += 1
        hypothesis_instance += 1

sudha_adaptation_flattened = []
for k, v in sudha_adaptation.items():
    # print(v[5]['sentence'], v[-1]['sentence'])
    sudha_adaptation_flattened.extend(v)

# print([d['sentence'] for d in sudha_adaptation_flattened[:8]])

# print(len(sudha_adaptation_flattened))

for t_id, template in TEMPLATES.items():
    save_dir_tr = f"data/experiments/single_stimuli_dative_simulation_valtest_vbd_arunachalam_template_tr_{t_id}"
    pathlib.Path(save_dir_tr).mkdir(parents=True, exist_ok=True)
    
    save_dir_rt = f"data/experiments/single_stimuli_dative_simulation_valtest_vbd_arunachalam_template_rt_{t_id}"
    pathlib.Path(save_dir_rt).mkdir(parents=True, exist_ok=True)
    
    #write generalization as is:
    write_jsonl(generalization, f"{save_dir_tr}/generalization.jsonl")
    write_jsonl(generalization, f"{save_dir_rt}/generalization.jsonl")

    adaptation_copy_tr = copy.deepcopy(sudha_adaptation_flattened)
    adaptation_copy_rt = copy.deepcopy(sudha_adaptation_flattened)
    
    
    theme_recipient = []
    # for data in adaptation_copy_tr:
    for item in sudha_adaptation_flattened:
        data = copy.deepcopy(item)
        theme = data['theme'].replace("the ", "a ")
        recipient = data['recipient'].replace("the ", "a ")
        data['sentence'] = generate_givenness(data["sampled_agent"], theme, recipient, data["sentence"], template)
        data["template_id"] = t_id
        theme_recipient.append(data)
    
    # print([d['sentence'] for d in theme_recipient[:5]])

    write_jsonl(theme_recipient, f"{save_dir_tr}/adaptation.jsonl")

    recipient_theme = []
    # for data in adaptation_copy_rt:
    for item in sudha_adaptation_flattened:
        data = copy.deepcopy(item)
        theme = data['theme'].replace("the ", "a ")
        recipient = data['recipient'].replace("the ", "a ")
        # sentence = copy.deepcopy()
        data['sentence'] = generate_givenness(data["sampled_agent"], recipient, theme, data['sentence'], template)
        data["template_id"] = t_id
        recipient_theme.append(data)
    
    # print([d['sentence'] for d in recipient_theme[:5]])

    write_jsonl(recipient_theme, f"{save_dir_rt}/adaptation.jsonl")

    # recipient_given = []
    # for data in adaptation_copy_recipient:
    #     if data["recipient_definiteness"] == "definite":
    #         sentence = data["sentence"].split(".")[-2].strip() + " ."
    #         data["sentence"] = generate_givenness(data["sampled_agent"], data["recipient"], sentence, template)
    #         data["template_id"] = t_id
    #         data["given"] = "recipient"
    #         recipient_given.append(data)

    # write_jsonl(recipient_given, f"{save_dir_rg}/adaptation.jsonl")



# # write to jsonl
# utils.write_jsonl("data/experiments/single_stimuli_dative_simulation_valtest_vbd_arunachalam_exact/adaptation.jsonl", sudha_adaptation)

    
    # add more items (item_id, same hypothesis_instance(k), hypothesis_instance+=1)