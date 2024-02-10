import csv
import json
from collections import defaultdict


def read_jsonl(file_path):
    data = []
    with open(file_path, "r") as f:
        for line in f:
            data.append(json.loads(line))
    return data

def read_json(file_path):
    with open(file_path, "r") as f:
        data = json.load(f)
    return data

def read_csv_dict(path):
    data = []
    with open(path, "r") as f:
        reader = csv.DictReader(f)
        for line in reader:
            data.append(line)
    return data

def generate_acronym(entry):
    acronym = ""
    args = ["theme", "recipient"]
    keys = ["pronominality", "animacy", "length"]
    for arg in args:
        for k in keys:
            acronym += entry[f"{arg}_{k}"][0]

    return acronym


def reorganize_sentences(experiment):
    sentences = defaultdict(lambda: defaultdict(list))
    for entry in experiment:
        acronym = generate_acronym(entry)
        sentences[acronym]["do"].append(entry["do"])
        sentences[acronym]["pp"].append(entry["pp"])

    sentences = dict(sentences)
    for acronym in sentences:
        sentences[acronym] = dict(sentences[acronym])

    return sentences
