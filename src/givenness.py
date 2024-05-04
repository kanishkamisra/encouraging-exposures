import pathlib
import utils
import json

from collections import defaultdict

'''
look there's {agent} with {given} .
{agent} was with {given} .
do you see {agent} and {given} ?
'''

TEMPLATES = {
    1: "look there's {} with {} .",
    2: "{} was with {} .",
    3: "do you see {} and {} ?"
}

def write_jsonl(data, path):
    with open(path, "w") as f:
        for d in data:
            f.write(json.dumps(d) + "\n")

def generate_givenness(agent, given, sentence, template):
    return f"{template.format(agent, given)} {sentence}"

adaptation = utils.read_jsonl("data/experiments/single_stimuli_dative_simulation_valtest_vbd_no_markedness_discourse_control/adaptation.jsonl")
generalization = utils.read_jsonl("data/experiments/single_stimuli_dative_simulation_valtest_vbd_no_markedness_discourse_control/generalization.jsonl")

for t_id, template in TEMPLATES.items():
    save_dir_tg = f"data/experiments/single_stimuli_dative_simulation_valtest_vbd_no_markedness_discourse_theme_given_template_{t_id}"
    pathlib.Path(save_dir_tg).mkdir(parents=True, exist_ok=True)
    
    save_dir_rg = f"data/experiments/single_stimuli_dative_simulation_valtest_vbd_no_markedness_discourse_recipient_given_template_{t_id}"
    pathlib.Path(save_dir_rg).mkdir(parents=True, exist_ok=True)
    
    #write generalization as is:
    write_jsonl(generalization, f"{save_dir_tg}/generalization.jsonl")
    write_jsonl(generalization, f"{save_dir_rg}/generalization.jsonl")

    adaptation_copy_theme = adaptation.copy()
    adaptation_copy_recipient = adaptation.copy()
    
    
    theme_given = []
    for data in adaptation_copy_theme:
        if data["theme_definiteness"] == "definite":
            sentence = data["sentence"].split(".")[-2].strip() + " ."
            data["sentence"] = generate_givenness(data["sampled_agent"], data["theme"], sentence, template)
            data["template_id"] = t_id
            data["given"] = "theme"
            theme_given.append(data)

    write_jsonl(theme_given, f"{save_dir_tg}/adaptation.jsonl")

    recipient_given = []
    for data in adaptation_copy_recipient:
        if data["recipient_definiteness"] == "definite":
            sentence = data["sentence"].split(".")[-2].strip() + " ."
            data["sentence"] = generate_givenness(data["sampled_agent"], data["recipient"], sentence, template)
            data["template_id"] = t_id
            data["given"] = "recipient"
            recipient_given.append(data)

    write_jsonl(recipient_given, f"{save_dir_rg}/adaptation.jsonl")