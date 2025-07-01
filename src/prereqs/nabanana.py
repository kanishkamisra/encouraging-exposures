import argparse
import csv
import pathlib
import utils

from minicons import scorer
from torch.utils.data import DataLoader
from tqdm import tqdm


def main(args):
    model = args.model
    model_name = model.replace("models/final/", "").replace("/", "_")

    lm = scorer.IncrementalLMScorer(model, device=args.device)

    # assumes all stimuli files are formatted the same way.
    stimuli = utils.read_csv_dict(args.stimuli)

    # create a dataloader for the stimuli
    stimuli_dl = DataLoader(stimuli, batch_size=args.batch_size)

    results = []
    for batch in tqdm(stimuli_dl):
        item_ids = batch["item_id"]
        do, pp = batch["do"], batch["pp"]
        do_scores = lm.sequence_score(do)
        pp_scores = lm.sequence_score(pp)

        results.extend(list(zip(item_ids, do_scores, pp_scores)))

    results_path= f"{args.results_dir}/{model_name}.csv"
    pathlib.Path(args.results_dir).mkdir(parents=True, exist_ok=True)
    with open(results_path, "w") as f:
        writer = csv.writer(f)
        writer.writerow(["item_id", "do_score", "pp_score"])
        writer.writerows(results)



if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--stimuli", type=str, default="data/naba-nana-sentences-240428.csv", help="path to stimuli csv file")
    parser.add_argument("--model", type=str, help="path to model")
    parser.add_argument("--device", type=str, default="cpu", help="device to run model on")
    parser.add_argument("--batch_size", type=int, default=64, help="batch size for model")
    parser.add_argument("--results_dir", type=str, default="data/results/nabanana", help="directory to save results")

    args = parser.parse_args()
    main(args)
