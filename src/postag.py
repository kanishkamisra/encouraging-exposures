"""POS-tag corpora"""

import argparse
import spacy
import unicodedata
import utils

from minicons.utils import get_batch
from tqdm import tqdm


def read_file(path):
    """TODO: make read all"""
    return [
        unicodedata.normalize("NFKD", i.strip())
        for i in open(path, encoding="utf-8").readlines()
    ]


def get_postags(texts, batch_size, processor):
    taglist = []
    for doc in tqdm(
        processor.pipe(
            texts,
            disable=["tok2vec", "parser", "attribute_ruler", "lemmatizer", "ner"],
            batch_size=batch_size,
        )
    ):
        sentlist = []
        for entity in doc:
            sentlist += [(entity.text, entity.tag_)]
        taglist += [sentlist]
    return taglist


def main(args):

    gpu = spacy.require_gpu(0)
    nlp = spacy.load("en_core_web_trf")

    source = args.source
    target = args.target

    corpus = utils.read_file(source)

    with open(target, "w") as f:
        for batch in get_batch(corpus, batch_size=args.batch_size):
            tags = get_postags(batch, args.batch_size, nlp)

            for j, item in enumerate(tags):
                # print(j, item)
                if item == []:
                    f.write("\n")
                else:
                    tokens, pos_tags = list(zip(*item))
                    pos_string = " ".join(pos_tags)
                    f.write(f"{pos_string}\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", "-s", type=str)
    parser.add_argument("--target", "-t", type=str)
    parser.add_argument("--batch_size", "-b", type=int, default=8192)
    args = parser.parse_args()

    main(args)
