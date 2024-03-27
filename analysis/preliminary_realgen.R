library(tidyverse)
library(fs)
library(jsonlite)
library(lme4)
library(lmerTest)
library(glue)
library(ggtext)

theme_set(
  theme_bw(base_size = 17, base_family = "Times") +
    theme(
      legend.position = "top",
      axis.text = element_text(color="black")
    )
)

mode = "_valtest_vbd"
# mode = ""

adaptation <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation{mode}/adaptation.jsonl"))) %>%
  as_tibble()
generalization <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation{mode}/generalization.jsonl"))) %>%
  as_tibble()

results <- dir_ls(glue("data/results/single_stimuli_dative_simulation{mode}/"), recurse = TRUE, regexp = "*lr_results_hypwise.csv") %>%
  map_df(read_csv, .id = "model") %>%
  mutate(
    model = str_extract(model, glue("(?<=simulation{mode}/)(.*)(?=/best_lr_results_hypwise.csv)"))
  ) 

if(mode == ""){
  results <- results %>%
    group_by(model, item_id, hypothesis_id, hypothesis_instance, adaptation_dative, adaptation_feature_config, state, generalization_dative) %>%
    summarize(
      logprob = mean(logprob)
    ) %>%
    ungroup()
}

results <- results %>%
  inner_join(feature_configs %>% rename(adaptation_feature_config = feature_config))



altform_generalization <- results %>%
  filter(state == "best", adaptation_dative != generalization_dative)

altform_generalization %>%
# results %>%
#   filter(state == "best") %>%
  group_by(adaptation_dative, generalization_dative, adaptation_feature_config) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  filter(adaptation_dative == "pp") %>%
  mutate(
    adaptation_feature_config = fct_reorder(adaptation_feature_config, logprob)
  ) %>%
  ggplot(aes(adaptation_feature_config, logprob, color = adaptation_dative, shape = generalization_dative)) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = logprob-ste, ymax = logprob+ste)) +
  # facet_wrap(~adaptation_dative, nrow = 2, scales = "free_x") +
  theme_bw(base_size = 15, base_family = "Times") +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

t.test(
  altform_generalization %>% 
    filter(adaptation_dative == "pp", theme_animacy == "inanimate", recipient_animacy == "animate") %>% 
    pull(logprob), 
  altform_generalization %>% 
    filter(adaptation_dative == "pp", theme_animacy == "inanimate", recipient_animacy == "inanimate") %>% 
    pull(logprob)
) %>%
  tidy()

feature_configs <- adaptation %>% 
  distinct(theme_pronominality, theme_animacy, theme_length, recipient_pronominality, recipient_animacy, recipient_length) %>%
  rowwise() %>%
  mutate(
    feature_config = glue::glue_collapse(str_sub(c(theme_pronominality, theme_animacy, theme_length, recipient_pronominality, recipient_animacy, recipient_length), 1, 1))
  )

adaptation %>% inner_join(feature_configs) %>% View("adaptationcodes")

results %>%
  filter(state == "best") %>%
  group_by(adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  filter(adaptation_dative != generalization_dative) %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    experiment = glue::glue("{adaptation_dative} --> {generalization_dative}")
  ) %>%
  ggplot(aes(experiment, logprob)) +
  # geom_boxplot()
  # geom_jitter(size = 2, width = 0.1)
  geom_point(size = 3, color = "steelblue") +
  geom_linerange(aes(ymin = logprob-ste, ymax=logprob+ste), color = "steelblue") +
  labs(
    x = "Generalization Experiment",
    y = "Average Log P(construction)"
  ) +
  theme(
    axis.text.x = element_markdown(),
    panel.grid = element_blank()
  )

#413x356
  

results %>%
  mutate(
    # feature_config = glue::glue("{recipient_pronominality}-recipient\n{theme_pronominality}-theme"),
    feature_config = glue::glue("{recipient_animacy}-recip\n{theme_animacy}-theme"),
    # feature_config = glue::glue("{recipient_animacy}-recipient"),
    # feature_config = glue::glue("{theme_animacy}-theme"),
    # feature_config = glue::glue("{theme_pronominality}-theme")
    # feature_config = glue::glue("{recipient_pronominality}-recip")
    # feature_config = glue::glue("{recipient_animacy}\n{recipient_pronominality}\n{recipient_length}"),
    # feature_config = glue::glue("{theme_animacy}\n{theme_pronominality}\n{theme_length}"),
    # feature_config = glue::glue("{recipient_animacy}\n{recipient_pronominality}\n{theme_animacy}\n{theme_pronominality}"),
  ) %>%
  filter(state == "best") %>% 
  # filter(str_detect(model, "111|444|333|777")) %>%
  # filter(theme_length == "short", recipient_length == "short") %>%
  # inner_join(adaptation %>% rename(adaptation_dative = dative)) %>%
  # filter(str_detect(adaptation_feature_config, "p(.*)(.*)p(.*)(.*)")) %>%
  # filter(str_detect(adaptation_feature_config, "(.*)a(.*)(.*)a(.*)")) %>%
  # filter(theme_length == "short", recipient_length == "short") %>%
  group_by(feature_config, adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    exposure = glue::glue("Exposure = {str_to_upper(adaptation_dative)}")
  ) %>%
  # filter(adaptation_dative == "pp", generalization_dative == "do") %>%
  filter(adaptation_dative != generalization_dative) %>%
  filter(adaptation_dative == "DO") %>%
  ggplot(aes(feature_config, logprob, color = generalization_dative)) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = logprob-ste, ymax=logprob+ste)) +
  facet_wrap(~exposure, nrow=2) +
  scale_color_manual(values = c("#00BFC4")) +
  # facet_grid(exposure ~ recipient_animacy) +
  # theme_bw(base_size = 14, base_family = "Times") +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    x = "Feature Configuration",
    y = "log P(alt-form)",
    color = "Generalization Dative"
  )

# 1345 x 515


results %>%
  inner_join(feature_configs %>% rename(adaptation_feature_config = feature_config)) %>%
  # filter(state == "best", adaptation_dative != generalization_dative)
  mutate(
    # feature_config = glue::glue("{recipient_pronominality}-recipient\n{theme_pronominality}-theme"),
    # feature_config = glue::glue("{recipient_animacy}-recipient\n{theme_animacy}-theme"),
    # feature_config = glue::glue("{recipient_animacy}-recipient"),
    # feature_config = glue::glue("{theme_animacy}-theme"),
    # feature_config = glue::glue("{theme_pronominality}-theme")
    feature_config = glue::glue("{recipient_animacy}\n{recipient_pronominality}\n{recipient_length}"),
  ) %>%
  filter(state == "best") %>% 
  # filter(str_detect(adaptation_feature_config, "p(.*)(.*)p(.*)(.*)")) %>%
  # filter(str_detect(adaptation_feature_config, "(.*)a(.*)(.*)a(.*)")) %>%
  # filter(theme_length == "short", recipient_length == "short") %>%
  group_by(feature_config, adaptation_dative, generalization_dative) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  mutate(
    exposure = glue::glue("Exposure = {str_to_upper(adaptation_dative)}")
  ) %>%
  # filter(adaptation_dative == "pp", generalization_dative == "do") %>%
  filter(adaptation_dative != generalization_dative) %>%
  ggplot(aes(feature_config, logprob, color = generalization_dative)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = logprob-ste, ymax=logprob+ste)) +
  facet_wrap(~exposure) +
  # theme_bw(base_size = 14, base_family = "Times") +
  # theme(
  #   legend.position = "top"
  # ) +
  labs(
    x = "Feature Configuration",
    y = "log P(construction)"
  )

best_results <- results %>%
  inner_join(feature_configs %>% rename(adaptation_feature_config = feature_config)) %>%
  filter(state == "best")

fit1 <- lmer(logprob ~ theme_animacy * theme_pronominality + recipient_animacy * recipient_pronominality + (1|model) + (1|item_id), data = best_results %>% filter(adaptation_dative == "pp", generalization_dative == "do"))

summary(fit1)
