library(tidyverse)
library(fs)
library(jsonlite)
library(lme4)
library(lmerTest)
library(glue)
library(ggtext)
library(emmeans)

theme_set(
  theme_bw(base_size = 17, base_family = "Times") +
    theme(
      legend.position = "top",
      axis.text = element_text(color="black")
    )
)

pronouns = c("him", "her", "them", "me", "us", "it", "something", "someone")
propernouns = c("bear", "bear over there", "mommy", "daddy", "grandma", "cat", "dog", "dog", "bert", "elmo", "dog outside the house", "cat outside the house", "teddy", "dolly")

propro = c(pronouns, propernouns)

mode = "_valtest_vbd_discourse_control"

adaptation <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation{mode}/adaptation.jsonl"))) %>%
  as_tibble() %>%
  mutate(
    canonicality = case_when(
      theme_markedness == recipient_markedness ~ "unmarked",
      theme_pronominality == "pronoun" & recipient_pronominality == "pronoun" ~ "unmarked",
      theme_definiteness == "definite" & theme_pronominality != "pronoun" & recipient_pronominality == "pronoun" ~ "nc-marked",
      theme_markedness == "unmarked" & recipient_markedness == "marked" ~ "nc-marked",
      theme_markedness == "marked" & recipient_markedness == "unmarked" ~ "marked"
    )
  ) %>%
  mutate(
    distinctiveness = case_when(
      theme %in% propro & !recipient %in% propro ~ "distinct",
      theme %in% propro & recipient %in% propro ~ "not_distinct",
      !theme %in% propro & recipient %in% propro ~ "distinct",
      !theme %in% propro & !recipient %in% propro ~ "not_distinct"
    )
  )

generalization <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation{mode}/generalization.jsonl"))) %>%
  as_tibble()

results <- dir_ls(glue("data/results/single_stimuli_dative_simulation{mode}/"), recurse = TRUE, regexp = "*lr_results_hypwise.csv") %>%
  map_df(read_csv, .id = "model") %>%
  mutate(
    model = str_extract(model, glue("(?<=simulation{mode}/)(.*)(?=/best_lr_results_hypwise.csv)"))
  ) %>%
  mutate(
    seed = as.numeric(str_remove(model, "smolm-autoreg-bpe-seed_"))
  )

feature_configs <- adaptation %>% 
  distinct(theme_pronominality, theme_animacy, theme_length, theme_definiteness, theme_markedness, recipient_pronominality, recipient_animacy, recipient_length, recipient_definiteness, recipient_markedness, canonicality) %>%
  rowwise() %>%
  mutate(
    feature_config = glue::glue_collapse(str_sub(c(theme_pronominality, theme_animacy, theme_length, theme_definiteness, theme_markedness, recipient_pronominality, recipient_animacy, recipient_length, recipient_definiteness, recipient_markedness), 1, 1))
  )

results <- results %>%
  inner_join(feature_configs %>% rename(adaptation_feature_config = feature_config)) %>%
  inner_join(adaptation %>% rename(adaptation_dative = dative)) %>%
  # filter(seed %in% c(6, 28, 221, 1024, 1102, 1729))
  filter(seed %in% c(6, 28, 221, 394, 496, 1024, 1102, 1729, 2309, 8128))

balanced_genset_results <- dir_ls(glue("../data/results/single_stimuli_dative_simulation{mode}/"), recurse = TRUE, regexp = "*lr_results_hypwise_balanced_gen.csv") %>%
  map_df(read_csv, .id = "model") %>%
  mutate(
    model = str_extract(model, glue("(?<=simulation{mode}/)(.*)(?=/best_lr_results_hypwise_balanced_gen.csv)"))
  ) 



results %>%
  pivot_wider(names_from = state, values_from = logprob) %>%
  mutate(diff = best - initial) %>%
  inner_join(adaptation %>% rename(adaptation_dative = dative)) %>%
  # filter(canonicality == "marked") %>%
  group_by(adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    # ste = 1.96 * plotrix::std.error((best-initial)/initial),
    # logprob = mean((best-initial)/initial),
    ste = 1.96 * plotrix::std.error(best),
    logprob = mean(best)
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
  # facet_wrap(~model, scales="free") +
  labs(
    x = "Generalization Experiment",
    y = "Average Log P(construction)"
  ) +
  theme(
    axis.text.x = element_markdown(),
    panel.grid = element_blank()
  )


balanced_genset_results %>%
  group_by(adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    # ste = 1.96 * plotrix::std.error((best-initial)/initial),
    # logprob = mean((best-initial)/initial),
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
  # facet_wrap(~model, scales="free") +
  labs(
    x = "Generalization Experiment",
    y = "Average Log P(construction)"
  ) +
  theme(
    axis.text.x = element_markdown(),
    panel.grid = element_blank()
  )

best_results <- results %>%
  pivot_wider(names_from = state, values_from = logprob) %>%
  mutate(
    diff = best - initial
  ) 

t.test(
  best_results %>% 
    filter(adaptation_dative == "pp", generalization_dative == "do", theme_animacy == "animate") %>% 
    pull(best),
  best_results %>% 
    filter(adaptation_dative == "pp", generalization_dative == "do", theme_animacy == "inanimate") %>% 
    pull(best)
)

results %>%
  filter(generalization_dative != adaptation_dative) %>%
  # filter(theme_definiteness == "definite", recipient_definiteness=="definite") %>%
  # filter(theme_pronominality != "pronoun", recipient_pronominality != "pronoun") %>%
  # filter(str_detect(sentence, "the")) %>%
  filter(state == "best") %>%
  # filter(!str_detect(sentence, "(dolly|teddy)")) %>%
  mutate(
    # config2 = glue("{theme_animacy}\n{recipient_animacy}"),
    # config2 = distinctiveness,
    # config2 = glue("{recipient_animacy}-Recip\n{theme_animacy}-Thm"),
    # config2 = glue("{recipient_animacy}\nrecipient"),
    config2 = glue("{theme_animacy}\ntheme"),
    # config2 = glue("{recipient_pronominality}-Recip\n{theme_pronominality}-Thm"),
    # config2 = recipient_pronominality,
    # config2 = recipient_animacy,
    # config2 = theme_animacy,
    config1 = ""
    # config1 = theme_definiteness,
    # config1 = glue("{recipient_definiteness}-Recip\n{theme_definiteness}-Thm"),
    # config1 = glue("{theme_animacy}-{theme_definiteness}"),
    # config1 = glue("{theme_length}-{recipient_length}"),
  ) %>%
  group_by(config1, config2, adaptation_dative, generalization_dative) %>%
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
  filter(adaptation_dative != generalization_dative) %>%
  ggplot(aes(config2, logprob, color = generalization_dative)) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = logprob-ste, ymax=logprob+ste)) +
  # facet_grid(config1~exposure, scales = "free") +
  facet_wrap(~exposure, scales = "free")+
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    x = "Feature Configuration",
    y = "log P(alt-form)",
    color = "Generalization Dative"
  )

best_model <- best_results %>%
  # filter(!str_detect(sentence, "(dolly|teddy)")) %>%
  filter(adaptation_dative == "pp", generalization_dative == "do") %>%
  mutate(
    # recipient_pronominality = factor(recipient_pronominality),
    # theme_pronominality = factor(theme_pronominality),
    recipient_pronominality = case_when(
      recipient_pronominality == "pronoun" ~ 1,
      TRUE ~ -1
    ),
    theme_pronominality = case_when(
      theme_pronominality == "pronoun" ~ 1,
      TRUE ~ -1
    ),
    recipient_animacy = case_when(
      recipient_animacy == "animate" ~ 1,
      TRUE ~ -1
    ),
    theme_animacy = case_when(
      theme_animacy == "animate" ~ 1,
      TRUE ~ -1
    ),
    model = factor(model),
    item_id = factor(item_id),
    hypothesis_id = factor(hypothesis_id)
  )

fit1 <- lmer(best ~ theme_animacy + recipient_animacy:theme_animacy + theme_pronominality * recipient_pronominality +
               theme_definiteness + theme_length + 
               recipient_definiteness + recipient_length + distinctiveness + 
               (1|model) + (1|item_id),data = best_model, REML=FALSE)
fit2 <- lmer(best ~ theme_animacy * recipient_animacy + theme_pronominality * recipient_pronominality +
               theme_definiteness + theme_length + 
               recipient_definiteness + recipient_length + distinctiveness + 
               (1|model) + (1|item_id),data = best_model, REML=FALSE)

fit3 <- lmer(best ~ recipient_animacy + recipient_animacy:theme_animacy + theme_pronominality * recipient_pronominality +
               theme_definiteness + theme_length + 
               recipient_definiteness + recipient_length + distinctiveness + 
               (1|model) + (1|item_id),data = best_model, REML=FALSE)


fit_pronominality_interaction <- lmer(best ~ theme_animacy * recipient_animacy + theme_pronominality + recipient_pronominality +
                                        theme_definiteness + theme_length + 
                                        recipient_definiteness + recipient_length + distinctiveness + 
                                        (1|model) + (1|item_id),data = best_model, REML=FALSE)

fit_animacy_interaction <- lmer(best ~ theme_animacy + recipient_animacy + theme_pronominality * recipient_pronominality +
                                  theme_definiteness + theme_length + 
                                  recipient_definiteness + recipient_length + distinctiveness + 
                                  (1|model) + (1|item_id),data = best_model, REML=FALSE)

fully_additive_fit <- lmer(best ~ theme_animacy + recipient_animacy + theme_pronominality + recipient_pronominality +
                             theme_definiteness + theme_length + 
                             recipient_definiteness + recipient_length + distinctiveness + 
                             (1|model) + (1|item_id),data = best_model, REML=FALSE)

fit_theme_features_interaction <- lmer(best ~ theme_animacy * theme_pronominality  + recipient_animacy + recipient_pronominality +
                                         theme_definiteness + theme_length + 
                                         recipient_definiteness + recipient_length + distinctiveness + 
                                         (1|model) + (1|item_id),data = best_model, REML=FALSE)

fit_theme_features_interaction_theme_animacy <- lmer(best ~ theme_pronominality + theme_animacy:theme_pronominality +
                                                       recipient_animacy + recipient_pronominality +
                                                       theme_definiteness + theme_length + 
                                                       recipient_definiteness + recipient_length + distinctiveness + 
                                                       (1|model) + (1|item_id),data = best_model, REML=FALSE)

fit_theme_features_interaction_theme_pronominality <- lmer(best ~ theme_animacy + theme_animacy:theme_pronominality +
                                                       recipient_animacy + recipient_pronominality +
                                                       theme_definiteness + theme_length + 
                                                       recipient_definiteness + recipient_length + distinctiveness + 
                                                       (1|model) + (1|item_id),data = best_model, REML=FALSE)


fit_recipient_features_interaction <- lmer(best ~ theme_animacy + theme_pronominality + 
                                             recipient_animacy * recipient_pronominality +
                                             theme_definiteness + theme_length + 
                                             recipient_definiteness + recipient_length + distinctiveness + 
                                             (1|model) + (1|item_id),data = best_model, REML=FALSE)

# PP --> DO
# recipient animacy significance
anova(fit1,fit2)
summary(fit2)

# theme animacy --> fit 3 is better!, theme animacy not significant!
anova(fit2, fit3)

# interaction between pronominality-features is not significant.
anova(fit2, fit_pronominality_interaction)


# interaction between animacy-features is significant.
anova(fit2, fit_animacy_interaction)

anova(fully_additive_fit, fit_pronominality_interaction)

summary(fit_pronominality_interaction)
summary(fit_animacy_interaction)

summary(fully_additive_fit)
summary(fit_theme_features_interaction)
summary(fit_recipient_features_interaction)

anova(fully_additive_fit, fit_theme_features_interaction)

anova(fit_theme_features_interaction, fit_theme_features_interaction_theme_animacy)

# PP -> DO final model: fit_theme_features_interaction (interaction between theme animacy and pronominality)
# pp --> do analysis
emmip(fit_theme_features_interaction, theme_pronominality ~ theme_animacy)
emmeans(fit_theme_features_interaction, pairwise ~ theme_animacy | theme_pronominality)


# DO -> PP final model: fit2 (animacy interaction, pronominal interaction)
# do ---> pp analysis
emmip(fit3, recipient_animacy ~ theme_animacy)
emmeans(fit3, pairwise ~ theme_animacy)

best_results %>%
  filter(adaptation_dative!=generalization_dative) %>%
  group_by(adaptation_dative, generalization_dative, theme_animacy, theme_pronominality) %>%
  summarize(
    best = mean(best)
  ) %>%
  ggplot(aes(theme_animacy, best, color = theme_pronominality, group=theme_pronominality)) +
  geom_line() + 
  facet_wrap(~adaptation_dative)
