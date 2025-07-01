import json
from typing import Iterator, List, Union

from tokenizers import (
    AddedToken,
    Regex,
    Tokenizer,
    decoders,
    normalizers,
    pre_tokenizers,
    trainers,
)
from tokenizers.implementations.base_tokenizer import BaseTokenizer
from tokenizers.models import Unigram, BPE
from tokenizers.processors import TemplateProcessing, RobertaProcessing


class SentencePieceUnigramTokenizer(BaseTokenizer):
    """
    This class is a copy of `DeDLOC's tokenizer implementation <https://github.com/yandex-research/DeDLOC/blob/main/sahajbert/tokenizer/tokenizer_model.py>`__ .

    Custom SentencePiece Unigram Tokenizer with NMT, NKFC, spaces and lower-casing characters normalization
    Represents the Unigram algorithm, with the pretokenization used by SentencePiece

    Modified slightly by Kanishka to add a bos_token to the vocabulary.
    """

    def __init__(
        self,
        mask: bool = False,
        replacement: str = "▁",
        add_prefix_space: bool = True,
        unk_token: Union[str, AddedToken] = "<unk>",
        bos_token: Union[str, AddedToken] = "<s>",
        eos_token: Union[str, AddedToken] = "</s>",
        pad_token: Union[str, AddedToken] = "<pad>",
    ):
        self.special_tokens = {
            "pad": {"id": 0, "token": pad_token},
            "bos": {"id": 1, "token": bos_token},
            "eos": {"id": 2, "token": eos_token},
            "unk": {"id": 3, "token": unk_token},
        }

        if mask:
            self.special_tokens.update({"mask": {"id": 4, "token": "<mask>"}})
            # print(self.special_tokens)

        self.special_tokens_list = [None] * len(self.special_tokens)
        for token_dict in self.special_tokens.values():
            self.special_tokens_list[token_dict["id"]] = token_dict["token"]

        tokenizer = Tokenizer(Unigram())

        tokenizer.normalizer = normalizers.Sequence(
            [
                normalizers.Nmt(),
                normalizers.NFKC(),
                normalizers.Replace(Regex(" {2,}"), " "),
                normalizers.Lowercase(),
            ]
        )
        tokenizer.pre_tokenizer = pre_tokenizers.Sequence(
            [
                pre_tokenizers.Metaspace(
                    replacement=replacement, add_prefix_space=add_prefix_space
                ),
                pre_tokenizers.Digits(individual_digits=True),
                pre_tokenizers.Punctuation(),
            ]
        )
        tokenizer.decoder = decoders.Metaspace(
            replacement=replacement, add_prefix_space=add_prefix_space
        )

        if mask:
            tokenizer.post_processor = TemplateProcessing(
                single=f"{self.special_tokens['bos']['token']} $0 {self.special_tokens['eos']['token']}",
                special_tokens=[
                    (
                        self.special_tokens["bos"]["token"],
                        self.special_tokens["bos"]["id"],
                    ),
                    (
                        self.special_tokens["eos"]["token"],
                        self.special_tokens["eos"]["id"],
                    ),
                ],
            )
        else:
            tokenizer.post_processor = TemplateProcessing(
                single=f"{self.special_tokens['bos']['token']} $0",
                special_tokens=[
                    (
                        self.special_tokens["bos"]["token"],
                        self.special_tokens["bos"]["id"],
                    )
                ],
            )

        parameters = {
            "model": "SentencePieceUnigram",
            "replacement": replacement,
            "add_prefix_space": add_prefix_space,
        }

        super().__init__(tokenizer, parameters)

    def train(
        self,
        files: Union[str, List[str]],
        vocab_size: int = 8192,
        show_progress: bool = True,
    ):
        """Train the model using the given files"""

        trainer = trainers.UnigramTrainer(
            vocab_size=vocab_size,
            special_tokens=self.special_tokens_list,
            show_progress=show_progress,
        )

        if isinstance(files, str):
            files = [files]
        self._tokenizer.train(files, trainer=trainer)

        self.add_unk_id()

    def train_from_iterator(
        self,
        iterator: Union[Iterator[str], Iterator[Iterator[str]]],
        vocab_size: int = 8192,
        show_progress: bool = True,
    ):
        """Train the model using the given iterator"""

        trainer = trainers.UnigramTrainer(
            vocab_size=vocab_size,
            special_tokens=self.special_tokens_list,
            show_progress=show_progress,
        )

        self._tokenizer.train_from_iterator(iterator, trainer=trainer)

        self.add_unk_id()

    def add_unk_id(self):
        tokenizer_json = json.loads(self._tokenizer.to_str())

        tokenizer_json["model"]["unk_id"] = self.special_tokens["unk"]["id"]

        self._tokenizer = Tokenizer.from_str(json.dumps(tokenizer_json))


class BPETokenizer(BaseTokenizer):
    def __init__(
        self,
        mask: bool = False,
        add_prefix_space: bool = True,
        unk_token: Union[str, AddedToken] = "<unk>",
        bos_token: Union[str, AddedToken] = "<s>",
        eos_token: Union[str, AddedToken] = "</s>",
        pad_token: Union[str, AddedToken] = "<pad>",
    ):
        self.special_tokens = {
            "pad": {"id": 0, "token": pad_token},
            "bos": {"id": 1, "token": bos_token},
            "eos": {"id": 2, "token": eos_token},
            "unk": {"id": 3, "token": unk_token},
        }

        if mask:
            self.special_tokens.update({"mask": {"id": 4, "token": "<mask>"}})
            # print(self.special_tokens)

        self.special_tokens_list = [None] * len(self.special_tokens)
        for token_dict in self.special_tokens.values():
            self.special_tokens_list[token_dict["id"]] = token_dict["token"]

        model = BPE(unk_token=self.special_tokens["unk"]["token"])
        tokenizer = Tokenizer(model)
        tokenizer.pre_tokenizer = pre_tokenizers.ByteLevel(
            add_prefix_space=add_prefix_space
        )
        tokenizer.normalizer = normalizers.Lowercase()

        if mask:
            tokenizer.post_processor = RobertaProcessing(
                sep=("</s>", 2),
                cls=("<s>", 1),
                add_prefix_space=add_prefix_space,
            )
        else:
            tokenizer.post_processor = TemplateProcessing(
                single=f"{self.special_tokens['bos']['token']} $0",
                special_tokens=[
                    (
                        self.special_tokens["bos"]["token"],
                        self.special_tokens["bos"]["id"],
                    )
                ],
            )

        tokenizer.decoder = decoders.ByteLevel(add_prefix_space=add_prefix_space)
        tokenizer.enable_padding(
            pad_id=0, pad_token=self.special_tokens["pad"]["token"]
        )
        # tokenizer.enable_truncation(max_length=128)

        parameters = {
            "model": "BPE",
            "add_prefix_space": add_prefix_space,
        }

        super().__init__(tokenizer, parameters)

    def train(
        self,
        files: Union[str, List[str]],
        vocab_size: int = 8192,
        show_progress: bool = True,
    ):
        """Train the model using the given files"""

        trainer = trainers.BpeTrainer(
            vocab_size=vocab_size,
            special_tokens=self.special_tokens_list,
            min_frequency=2,
            show_progress=show_progress,
        )

        if isinstance(files, str):
            files = [files]
        self._tokenizer.train(files, trainer=trainer)

    def train_from_iterator(
        self,
        iterator: Iterator[str],
        vocab_size: int = 8192,
        show_progress: bool = True,
    ):
        """Train the model using the given iterator"""

        trainer = trainers.BpeTrainer(
            vocab_size=vocab_size,
            special_tokens=self.special_tokens_list,
            min_frequency=2,
            show_progress=show_progress,
        )

        self._tokenizer.train_from_iterator(iterator, trainer=trainer)
