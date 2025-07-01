import csv
import json
import torch
import utils

import numpy as np

from collections import defaultdict
from copy import deepcopy
from minicons import scorer
from torch import optim
from torch.utils.data import DataLoader
from tqdm import tqdm, trange
from transformers import (
    AdamW,
    get_constant_schedule,
    set_seed,
)


class Learner:
    def __init__(
        self,
        model_name,
        device="cpu",
        gaussian=True,
        added_tokens=[" [verb]"],
        target_params=["model.decoder.embed_tokens.weight"],
        debug=False
    ):
        """Learner Class"""
        self.lm = scorer.IncrementalLMScorer(model_name, device)
        self.device = device
        self.gaussian = gaussian
        self.added_tokens = added_tokens
        self.target_params = target_params
        self.model_config = {
            "model_name": model_name,
            "device": device,
            "gaussian": gaussian,
            "added_tokens": added_tokens,
            "target_params": target_params,
        }
        self.length = self.lm.model.get_input_embeddings().weight.shape[0]
        self.new_length = self.length + len(self.added_tokens)
        self.new_index = self.new_length - len(self.added_tokens)
        self.debug = debug

    def _initialize_gaussian(self):
        embeddings_weight = self.lm.model.get_input_embeddings().weight
        embeddings_weight.requires_grad = False

        mu = embeddings_weight[: self.new_index].mean(0).detach()
        n = self.length
        sigma = (
            (embeddings_weight[: self.new_index] - mu).T
            @ (embeddings_weight[: self.new_index] - mu)
        ) / n
        dist = torch.distributions.multivariate_normal.MultivariateNormal(
            mu, covariance_matrix=1e-5 * sigma
        )

        embeddings_weight[self.new_index :] = torch.stack(
            tuple((dist.sample() for _ in range(len(self.added_tokens)))), dim=0
        )
        embeddings_weight.requires_grad = True

    def _freeze(self):
        for param in self.lm.model.named_parameters():
            if param[0] not in self.target_params:
                param[1].requires_grad = False
        assert [
            param[0]
            for param in self.lm.model.named_parameters()
            if param[1].requires_grad
        ] == self.target_params

    def unfreeze_full(self):
        for param in self.lm.model.parameters():
            param.requires_grad = True

    def freeze_full(self):
        for param in self.lm.model.parameters():
            param.requires_grad = False

    def add_tokens(self):
        self.lm.tokenizer.add_tokens(self.added_tokens)
        self.lm.model.resize_token_embeddings(self.new_length)
        if self.debug:
            print(
                f"New token added. New embedding size: {self.lm.model.get_output_embeddings().weight.shape}"
            )

        if self.gaussian:
            self._initialize_gaussian()

        self._freeze()
        self.initial_emb = deepcopy(
            self.lm.model.get_output_embeddings().weight[self.new_index].detach()
        )
        # self.lm.model = self.lm.model.to(self.device)

    def add_token_and_reinitialize(self, target_emb):
        self.add_tokens()
        self.freeze_full()
        self.lm.model.get_input_embeddings().weight[self.new_index] = target_emb
        # self.lm.model = self.lm.model.to(self.device)

    def reinitialize(self, target_emb):
        self.lm.model.get_input_embeddings().weight[self.new_index] = target_emb

    def prepare_text(self, text, **kwargs):
        encoded, offset = self.lm.prepare_text(text=text, **kwargs)
        encoded["input_ids"] = torch.tensor(
            [
                [t - 1 if t > self.length else t for t in token_ids]
                for token_ids in encoded.input_ids
            ]
        )
        return encoded, offset

    def token_score(
        self,
        batch,
        surprisal=False,
        prob=False,
        base_two=False,
        rank=False,
        decode=True,
        **kwargs,
    ):
        """
        For every input sentence, returns a list of tuples in the following format:
            `(token, score)`,

        where score represents the log-probability (by default) of the token given context. Can also return ranks along with scores.

        :param ``Union[str, List[str]]`` batch: a single sentence or a batch of sentences.
        :param ``bool`` surprisal: If `True`, returns per-word surprisals instead of log-probabilities.
        :param ``bool`` prob: If `True`, returns per-word probabilities instead of log-probabilities.
        :param ``bool`` base_two: If `True`, uses log base 2 instead of natural-log (returns bits of values in case of surprisals)
        :param ``bool`` rank: If `True`, also returns the rank of each word in context (based on the log-probability value)

        :return: A `List` containing a `Tuple` consisting of the word, its associated score, and optionally, its rank.
        :rtype: ``Union[List[Tuple[str, float]], List[Tuple[str, float, int]]]``
        """

        assert not (
            surprisal and prob
        ), "cannot both evaluate probability and surprisal at the same time!"
        assert not (
            base_two and prob
        ), "cannot both use base (which is for a log), and a probability measure at the same time!"

        tokenized = self.prepare_text(batch, **kwargs)
        if rank:
            scores, ranks = self.lm.compute_stats(
                tokenized, rank=rank, prob=prob, base_two=base_two, return_tensors=True
            )
        else:
            scores = self.lm.compute_stats(
                tokenized, prob=prob, base_two=base_two, return_tensors=True
            )

        if surprisal:
            scores = [-1.0 * s for s in scores]

        scores = [s.tolist() for s in scores]

        # indices = [
        #     [i for i in indexed if i != self.tokenizer.pad_token_id]
        #     for indexed in tokenized[0]["input_ids"].tolist()
        # ]

        indices = [
            [i for i, am in zip(instance, attention_mask) if am != 0]
            for instance, attention_mask in zip(
                tokenized[0]["input_ids"].tolist(),
                tokenized[0]["attention_mask"].tolist(),
            )
        ]
        indices = [[ii + 1 if ii >= self.length else ii for ii in i] for i in indices]
        # print(indices)
        if decode:
            tokens = [self.lm.decode(idx) for idx in indices]
        else:
            tokens = [self.lm.tokenizer.convert_ids_to_tokens(idx) for idx in indices]

        if rank:
            assert len(tokens) == len(scores) == len(ranks)
        else:
            assert len(tokens) == len(scores)

        res = []
        if rank:
            for t, s, r in zip(tokens, scores, ranks):
                if len(t) > len(s):
                    diff = len(t) - len(s)
                    sc = [0.0] * diff + s
                    ra = [0] * diff + r
                    res.append(list(zip(t, sc, ra)))
                else:
                    res.append(list(zip(t, sc, ra)))
            # return [list(zip(t, s, r)) for t, s, r in zip(tokens, scores, ranks)]
        else:
            for t, s in zip(tokens, scores):
                if len(t) > len(s):
                    diff = len(t) - len(s)
                    sc = [0.0] * diff + s
                    res.append(list(zip(t, sc)))
                else:
                    res.append(list(zip(t, sc)))

        return res

    def sequence_score(
        self,
        batch,
        reduction=lambda x: x.mean(0).item(),
        prob=False,
        base_two=False,
        **kw,
    ):
        """
        Pooled estimates of sequence log probabilities (or some modification of it).

        :param batch: a batch of sequences whose score you want to calculate.
        :type batch: ``Union[str, List[str]]``
        :param reduction: Reduction function, is selected to be
            ``lambda x: x.mean(0).item()`` by default, which stands for the avg. log-probability per token for each sequence in the batch.
        :type reduction: Callable
        :param kw: model-specific keyword arguments to pass to the `prepare_text` function
        :return: List of floats specifying the desired score for the stimuli part of the input, e.g., P(stimuli | preamble).
        :rtype: ``List[float]``

        TODO: reduction should be a string, if it's a function, specify what kind of function. --> how to ensure it is always that type?
        """
        tokenized = self.prepare_text(batch, **kw)
        # print(tokenized)
        scores = self.lm.compute_stats(
            tokenized, rank=False, base_two=base_two, prob=prob, return_tensors=True
        )
        reduced = list(map(reduction, scores))
        return reduced

    def logprob(self, corpus, batch_size=-1, by_instance=False):
        """gets the avg. log prob per token given a corpus."""
        if batch_size > 0:
            scores = []
            dl = DataLoader(corpus, batch_size=batch_size)
            for batch in dl:
                scores.extend(self.sequence_score(batch))
        else:
            scores = self.sequence_score(corpus)
        if by_instance:
            return scores
        return np.mean(scores)


class Trainer:
    def __init__(
        self,
        model,
        training_set,
        generalization_set,
        validation_set,
        val_performance_metric="diff",
        learning_rate=1e-3,
        weight_decay=0.0,
        debug=False
    ):
        """Trainer Class."""
        self.model = model
        self.model.add_tokens()
        self.val_performance_metric = val_performance_metric
        self.learning_rate = learning_rate
        self.weight_decay = weight_decay
        self.metrics = {"train_loss": [], "val_performance": []}
        self.training_set = training_set
        self.validation_set = validation_set
        self.generalization_set = generalization_set
        self.generalization_results = []
        self.agg_gen_results = defaultdict(float)
        self.best_epoch = 100
        # self.initial_emb = self.model.lm.model.get_output_embeddings().weight[self.model.new_index].detach()
        self.embs = []
        self.best_embs = []
        self.debug = debug

    # def add_token(self):
    #     self.model.add_tokens()

    def validate(self, batch_size=-1):
        # if self.validation set is a json with two cats, then we get
        # logprob for both and take diff if thats the metric, or else
        # just do pairwise comparison of 1 > 2.
        # if validation set is a list of sentences, then we return avg. logprob.
        if isinstance(self.validation_set, list):
            return self.model.logprob(self.validation_set)
        elif isinstance(self.validation_set, dict):
            if len(self.validation_set) == 2:
                if self.val_performance_metric == "diff":
                    return self.model.logprob(
                        self.validation_set["good"], batch_size=batch_size
                    ) - self.model.logprob(
                        self.validation_set["bad"], batch_size=batch_size
                    )
                else:
                    num_correct = 0
                    goods = self.model.logprob(
                        self.validation_set["good"],
                        batch_size=batch_size,
                        by_instance=True,
                    )
                    bads = self.model.logprob(
                        self.validation_set["bad"],
                        batch_size=batch_size,
                        by_instance=True,
                    )

                    for good, bad in zip(goods, bads):
                        if good > bad:
                            num_correct += 1

                    return num_correct / len(self.validation_set["good"])
            else:
                raise ValueError(
                    "Validation set must be a list of sentences or a dictionary with two keys (good and bad)."
                )

    def optimizer_setup(self):
        no_decay = ["bias", "LayerNorm.weight"]
        optimizer_grouped_parameters = [
            {
                "params": [
                    p
                    for n, p in self.model.lm.model.named_parameters()
                    if not any(nd in n for nd in no_decay)
                ],
                "weight_decay": self.weight_decay,
            },
            {
                "params": [
                    p
                    for n, p in self.model.lm.model.named_parameters()
                    if any(nd in n for nd in no_decay)
                ],
                "weight_decay": 0.0,
            },
        ]
        self.optimizer = AdamW(
            optimizer_grouped_parameters, lr=self.learning_rate, eps=1e-8
        )
        self.scheduler = get_constant_schedule(self.optimizer)

    def generalization_step(self, model_state, batch_size=64):
        dl = DataLoader(self.generalization_set, batch_size=batch_size, shuffle=False)
        results = []
        datives = []
        for batch in dl:
            dative = batch["sentence"]
            dative_type = batch["dative"]
            datives.extend(dative_type)

            scores = self.model.sequence_score(dative)
            results.extend(scores)

        for i, (res, dat) in enumerate(zip(results, datives)):
            # self.generalization_results.append([i + 1, model_state, dat, res])
            self.generalization_results.append(
                {
                    "item": i + 1,
                    "model_state": model_state,
                    "dative": dat,
                    "logprob": res,
                }
            )

    def aggregate_generalization_results(self):
        # check if generalization results has something in it:
        assert len(self.generalization_results) != 0

        results = defaultdict(lambda: defaultdict(list))
        for entry in self.generalization_results:
            results[entry["model_state"]][entry["dative"]].append(entry["logprob"])

        results = dict(results)
        for state, dative in results.items():
            dative = dict(dative)
            for d, scores in dative.items():
                avg = np.mean(scores)
                self.agg_gen_results[(state, d)] = avg

    def train(self, num_epochs, generalization_batch_size):
        # print(self.model.lm.model.get_output_embeddings().weight)
        # self.generalization_step("initial", generalization_batch_size)
        self.optimizer_setup()
        encoded, offset = self.model.prepare_text(self.training_set)
        encoded = encoded.to(self.model.device)

        labels = encoded.input_ids.clone()
        if self.model.lm.tokenizer.pad_token_id is not None:
            labels[labels == self.model.lm.tokenizer.pad_token_id] = -100

        if self.model.lm.tokenizer.bos_token_id is not None:
            labels[labels == 1] = -100

        # for i in trange(num_epochs, desc="Epoch"):
        for i in range(num_epochs):
            epoch = i + 1
            output = self.model.lm.model(**encoded, labels=labels)
            output.loss.backward()

            with torch.no_grad():
                for m, p in self.model.lm.model.named_parameters():
                    if m in self.model.target_params:
                        # embeddings = p
                        p.grad[: self.model.new_index] = 0
                        break

            self.optimizer.step()
            self.scheduler.step()
            self.model.lm.model.zero_grad()

            # store embeddings
            emb = (
                self.model.lm.model.resize_token_embeddings()
                .weight[self.model.new_index :]
                .detach()
                .clone()
            )
            # emb.requires_grad = False
            self.embs.append(emb)

            self.metrics["train_loss"].append(output.loss.item())
            self.metrics["val_performance"].append(self.validate())

        # self.generalization_step("final", generalization_batch_size)

        # print(self.model.lm.model.resize_token_embeddings().weight)

        self.best_epoch = np.argmax(self.metrics["val_performance"]) + 1
        if self.debug:
            print(
                f"Best Epoch: {self.best_epoch}. Validation: {self.metrics['val_performance'][self.best_epoch-1]}"
            )
        # re-train the model to the best epoch

        # reset model to initial state
        # self.model.zero_grad()
        # self.model.lm.model.eval()
        # self.model.requires_grad = False
        # self.model.zero_grad()
        # print(self.model.lm.model.resize_token_embeddings().weight[
        #     self.model.new_index :
        # ])
        # print(self.embs[self.best_epoch-1])
        for m, p in self.model.lm.model.named_parameters():
            if m in self.model.target_params:
                p.requires_grad = False
                # embeddings = p
                # p.grad[: self.model.new_index] = 0.0
                # break

        # print(self.embs[self.best_epoch-1])
        # print(self.model.lm.model.resize_token_embeddings().weight[
        #     self.model.new_index :
        # ] == self.embs[self.best_epoch - 1])

        # print(self.embs[self.best_epoch-1])
        self.model.lm.model.get_output_embeddings().weight[self.model.new_index :] = (
            self.embs[self.best_epoch - 1]
        )
        try:
            self.best_embs = torch.cat(self.embs[:self.best_epoch-1]).detach()
        except:
            self.best_embs = self.model.initial_emb
        # print(self.model.model_config)
        # set_seed(42)
        # self.model = Learner(**self.model.model_config)
        # self.model.add_tokens()

        # self.optimizer_setup()
        # self.optimizer.zero_grad()
        # for i in trange(self.best_epoch, desc="Epoch"):
        #     output = self.model.lm.model(**encoded, labels=labels)
        #     output.loss.backward()

        #     for m, p in self.model.lm.model.named_parameters():
        #         if m in self.model.target_params:
        #             # embeddings = p
        #             p.grad[: self.model.new_index] = 0.0
        #             break

        #     self.optimizer.step()
        #     self.scheduler.step()
        #     self.model.lm.model.zero_grad()
        if self.debug:
            print(f"Val performance recheck: {self.validate()}")
        # print(self.model.lm.model.get_output_embeddings().weight)

        self.generalization_step("best", generalization_batch_size)

        self.aggregate_generalization_results()

    # def reset(self):
    #     # self.__init__(
    #     #     self.model,
    #     #     self.training_set,
    #     #     self.generalization_set,
    #     #     self.validation_set,
    #     #     self.val_performance_metric,
    #     #     self.learning_rate,
    #     #     self.weight_decay,
    #     # )
    #     self.metrics = {"train_loss": [], "val_performance": []}
    #     self.generalization_results = []
    #     self.agg_gen_results = defaultdict(float)
    #     self.embs = []
    #     self.best_epoch = 100
    #     self.model.lm.model.get_output_embeddings().weight[
    #         self.model.new_index :
    #     ] = self.model.initial_emb
    #     self.model.unfreeze_full()
    #     self.model._freeze()
    #     # self.optimizer_setup()
    #     # # reset embedding value to initial
    #     # self.model.initial_emb.requires_grad = True
    #     # self.model.lm.model.get_output_embeddings().weight[self.model.new_index :] = (
    #     #     self.model.initial_emb
    #     # )
