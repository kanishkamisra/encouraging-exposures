import argparse
import csv
import json

from collections import defaultdict


def main(args):
    
    with open(args.input, 'r', encoding='utf-8') as f:
        with open(args.output, 'w', encoding='utf-8') as out:
            reader = csv.DictReader(f)
            lexicon = defaultdict(list)
            for row in reader:
                lexicon[row['key']].append(row['value'])
            json.dump(lexicon, out, ensure_ascii=False, indent=4)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Convert CSV to JSON lexicon')
    parser.add_argument('--input', '-i', help='Input CSV file')
    parser.add_argument('--output', '-o', help='Output JSON file')
    args = parser.parse_args()
    main(args)