# encouraging-exposures
Code for exploring the interplay between the nature of training exposures and LM generalization in the context of dative alternations


## reproducing results

```bash
bash scripts/run_exposure_exps.sh
```

## results

The results from our experiments can be found in `data/results/single_stimuli_dative_simulation`, where each directory is named as:

```bash
<ITEM-ID>_<HYPOTHESIS-ID>_<HYPOTHESIS-INSTANCE>_<LEARNING-RATE>_<DATIVE-TYPE>_RESULTS

ITEM-ID: Unique item, this variable tracks the pair of hypothesis (35) x hypothesis instances (5).

HYPOTHESIS-ID: Hypothesis, where each hypothesis is a combination of theme and recipient features.

HYPOTHESIS-INSTANCE: Instance of a particular hypothesis.

LEARNING-RATE: Learning rate used in the simulation

DATIVE-TYPE: Context used, dictated by the type of the dative (double object or prepositional)
```

Within each directory, we have:

1. `generalization_results.csv`
```bash
Column details:

item_id: unique generalization item id

model_state: either initial, best, or final

do: logprob of the do sentence

pp: logprob of the pp sentence
```

2. `metrics.csv`
```bash
Column details:

train_loss: train loss

val_performance: value of the performance metric chosen to quantify validation performance.
```

3. `training_summary.json`
```bash
Keys:

best_epoch: 

best_val_performance:

val_performance_metric:

train_loss:

initial_pref_do: % of time do sents are preferred over pp ones (>50 indicates preference for do over pp), initially

final_pref_do: % of time do sents are preferred over pp ones (>50 indicates preference for do over pp), at end of training

best_pref_do: % of time do sents are preferred over pp ones (>50 indicates preference for do over pp), at epoch that shows best val performance.
```