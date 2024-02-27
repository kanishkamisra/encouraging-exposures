library(tidyverse)
library(fs)
library(jsonlite)
library(lme4)
library(lmerTest)

adaptation <- stream_in(file("data/experiments/single_stimuli_dative_simulation/adaptation.jsonl")) %>%
  as_tibble()
generalization <- stream_in(file("data/experiments/single_stimuli_dative_simulation/generalization.jsonl")) %>%
  as_tibble()

results <- dir_ls("data/results/single_stimuli_dative_simulation/", recurse = TRUE, regexp = "*lr_results_hypwise.csv") %>%
  map_df(read_csv, .id = "model") %>%
  mutate(
    model = str_extract(model, "(?<=simulation/)(.*)(?=/best_lr_results_hypwise.csv)")
  )

glue::glue_collapse(str_sub(c("pronoun", "animate"), 1, 1))

feature_configs <- adaptation %>% 
  distinct(theme_pronominality, theme_animacy, theme_length, recipient_pronominality, recipient_animacy, recipient_length) %>%
  rowwise() %>%
  mutate(
    feature_config = glue::glue_collapse(str_sub(c(theme_pronominality, theme_animacy, theme_length, recipient_pronominality, recipient_animacy, recipient_length), 1, 1))
  )

aggregated_results <- results %>% 
  # filter(generalization_feature_config == adaptation_feature_config) %>%
  # filter(generalization_feature_config == "nispas") %>%
  # filter(str_detect(generalization_feature_config, "ni(s|l)pa(s|l)")) %>%
  group_by(model, item_id, hypothesis_id, hypothesis_instance, adaptation_dative, adaptation_feature_config, generalization_dative) %>%
  summarize(
    logprob = mean(logprob)
  )

aggregated_results %>%
  inner_join(feature_configs %>% rename(adaptation_feature_config = feature_config)) %>%
  mutate(
    # feature_config = glue::glue("{recipient_pronominality}-recipient\n{theme_pronominality}-theme"),
    feature_config = glue::glue("{recipient_animacy}-recipient\n{theme_animacy}-theme"),
  ) %>%
  group_by(feature_config, adaptation_dative, generalization_dative) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  mutate(
    exposure = glue::glue("Exposure = {str_to_upper(adaptation_dative)}")
  ) %>%
  filter(adaptation_dative == "pp", generalization_dative == "do") %>%
  ggplot(aes(feature_config, logprob)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = logprob-ste, ymax=logprob+ste)) +
  facet_wrap(~exposure) +
  theme_bw(base_size = 14, base_family = "Times") +
  labs(
    x = "Feature Configuration",
    y = "log P(DO)"
  )


aggregated_results %>%
  group_by(adaptation_dative, generalization_dative, adaptation_feature_config) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>% filter(str_detect(adaptation_feature_config, "[pn]a[ls][pn]a[ls]")) %>% View()
  mutate(
    exposure = glue::glue("Exposure = {str_to_upper(adaptation_dative)}")
  ) %>%
  filter(adaptation_dative == "pp", generalization_dative == "do") %>%
  mutate(
    adaptation_feature_config = fct_reorder(adaptation_feature_config, logprob)
  ) %>%
  ggplot(aes(adaptation_feature_config, logprob)) +
  geom_point(size = 2, shape = 21, color = "black", fill = "black") +
  geom_linerange(aes(ymin = logprob-ste, ymax = logprob+ste)) +
  theme_bw(base_size = 15, base_family = "Times") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
    legend.position = "top",
    axis.text.y = element_text(color = "black")
  )

