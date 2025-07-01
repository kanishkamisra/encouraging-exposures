# conda activate spacy

# cp ../corpora/babylm_data/babylm_100M/aochildes.train data/corpora
python src/postag.py --source data/corpora/aochildes.train --target data/corpora/aochildes.postags
