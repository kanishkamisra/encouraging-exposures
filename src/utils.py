import csv
import json
import unicodedata
from collections import defaultdict


def read_file(path):
    """TODO: make read all"""
    return [
        unicodedata.normalize("NFKD", i.strip())
        for i in open(path, encoding="utf-8").readlines()
        if i.strip() != ""
    ]


def read_jsonl(file_path):
    data = []
    with open(file_path, "r") as f:
        for line in f:
            data.append(json.loads(line))
    return data

def write_jsonl(data, file_path):
    with open(file_path, "w") as f:
        for line in data:
            f.write(json.dumps(line) + "\n")

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

def generate_acronym_tuple(entry):
    acronym = ""
    for arg in entry:
        for k in arg:
            acronym+=k[0]

    return acronym


def generate_acronym(entry, full=False):
    acronym = ""
    args = ["theme", "recipient"]
    if full:
        keys = ["pronominality", "animacy", "length", "definiteness", "markedness", "givenness"]
    else:
        keys = ["pronominality", "animacy", "length"]
    for arg in args:
        for k in keys:
            try:
                acronym += entry[f"{arg}_{k}"][0]
            except:
                pass
    return acronym


# def reorganize_sentences(experiment):
#     sentences = defaultdict(lambda: defaultdict(list))
#     for entry in experiment:
#         acronym = generate_acronym(entry)
#         sentences[acronym]["do"].append(entry["do"])
#         sentences[acronym]["pp"].append(entry["pp"])

#     sentences = dict(sentences)
#     for acronym in sentences:
#         sentences[acronym] = dict(sentences[acronym])

#     return sentences

def reorganize_sentences(experiment, full=False):
    sentences = defaultdict(lambda: defaultdict(list))
    for entry in experiment:
        acronym = generate_acronym(entry, full)
        sentences[acronym][entry['dative']].append(entry['sentence'])

    sentences = dict(sentences)
    for acronym in sentences:
        sentences[acronym] = dict(sentences[acronym])

    return sentences


def write_csv_dict(data, path):
    with open(path, "w") as f:
        writer = csv.DictWriter(f, fieldnames=data[0].keys())
        writer.writeheader()
        for line in data:
            writer.writerow(line)