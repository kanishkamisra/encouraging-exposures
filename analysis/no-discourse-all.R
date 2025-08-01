library(tidyverse)
library(fs)
library(jsonlite)
library(lme4)
library(lmerTest)
library(glue)
library(ggtext)
library(emmeans)
library(broom.mixed)
library(DT)

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

mode = "_valtest_vbd_no_discourse"

adaptation1 <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation{mode}/adaptation.jsonl"))) %>%
  as_tibble() %>%
  mutate(
    canonicality = case_when(
      theme_markedness == recipient_markedness ~ "unmarked",
      theme_pronominality == "pronoun" & recipient_pronominality == "pronoun" ~ "unmarked",
      theme_definiteness == "definite" & theme_pronominality != "pronoun" & recipient_pronominality == "pronoun" ~ "nc-marked",
      theme_markedness == "unmarked" & recipient_markedness == "marked" ~ "nc-marked",
      theme_markedness == "marked" & recipient_markedness == "unmarked" ~ "marked"
    )
  )

adaptation2 <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation{mode}2/adaptation.jsonl"))) %>%
  as_tibble() %>%
  mutate(
    canonicality = case_when(
      theme_markedness == recipient_markedness ~ "unmarked",
      theme_pronominality == "pronoun" & recipient_pronominality == "pronoun" ~ "unmarked",
      theme_definiteness == "definite" & theme_pronominality != "pronoun" & recipient_pronominality == "pronoun" ~ "nc-marked",
      theme_markedness == "unmarked" & recipient_markedness == "marked" ~ "nc-marked",
      theme_markedness == "marked" & recipient_markedness == "unmarked" ~ "marked"
    )
  )

adaptation3 <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation{mode}3/adaptation.jsonl"))) %>%
  as_tibble() %>%
  mutate(
    canonicality = case_when(
      theme_markedness == recipient_markedness ~ "unmarked",
      theme_pronominality == "pronoun" & recipient_pronominality == "pronoun" ~ "unmarked",
      theme_definiteness == "definite" & theme_pronominality != "pronoun" & recipient_pronominality == "pronoun" ~ "nc-marked",
      theme_markedness == "unmarked" & recipient_markedness == "marked" ~ "nc-marked",
      theme_markedness == "marked" & recipient_markedness == "unmarked" ~ "marked"
    )
  )

td_ids = adaptation3 %>% pull(item)
# td_ids = c()

adaptation <- bind_rows(
  bind_rows(adaptation1, adaptation2) %>%
    filter(!item %in% td_ids),
  adaptation3
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


feature_configs <- adaptation %>% 
  distinct(theme_pronominality, theme_animacy, theme_length, theme_definiteness, theme_markedness, recipient_pronominality, recipient_animacy, recipient_length, recipient_definiteness, recipient_markedness, canonicality) %>%
  rowwise() %>%
  mutate(
    feature_config = glue::glue_collapse(str_sub(c(theme_pronominality, theme_animacy, theme_length, theme_definiteness, theme_markedness, recipient_pronominality, recipient_animacy, recipient_length, recipient_definiteness, recipient_markedness), 1, 1))
  )


results <- bind_rows(
  bind_rows(
    dir_ls(glue("data/results/single_stimuli_dative_simulation{mode}/"), recurse = TRUE, regexp = "*lr_results_hypwise.csv") %>%
      map_df(read_csv, .id = "model") %>%
      mutate(
        model = str_extract(model, glue("(?<=simulation{mode}/)(.*)(?=/best_lr_results_hypwise.csv)"))
      ),
    dir_ls(glue("data/results/single_stimuli_dative_simulation{mode}2/"), recurse = TRUE, regexp = "*lr_results_hypwise.csv") %>%
      map_df(read_csv, .id = "model") %>%
      mutate(
        model = str_extract(model, glue("(?<=simulation{mode}2/)(.*)(?=/best_lr_results_hypwise.csv)"))
      ) 
  ) %>%
    filter(!item_id %in% td_ids),
  dir_ls(glue("data/results/single_stimuli_dative_simulation{mode}3/"), recurse = TRUE, regexp = "*lr_results_hypwise.csv") %>%
    map_df(read_csv, .id = "model") %>%
    mutate(
      model = str_extract(model, glue("(?<=simulation{mode}3/)(.*)(?=/best_lr_results_hypwise.csv)"))
    ),
) %>%
  mutate(
    seed = as.numeric(str_remove(model, "smolm-autoreg-bpe-seed_"))
  ) %>%
  inner_join(feature_configs %>% rename(adaptation_feature_config = feature_config)) %>%
  inner_join(adaptation %>% rename(adaptation_dative = dative)) %>%
  # filter(seed %in% c(6, 28, 221, 1024, 1102, 1729))
  filter(seed %in% c(6, 28, 221, 394, 496, 1024, 1102, 1729, 2309, 8128))
  # filter(seed %in% c(394, 496,2309, 8128))

balgen_results <- bind_rows(
  bind_rows(
    dir_ls(glue("data/results/single_stimuli_dative_simulation{mode}/"), recurse = TRUE, regexp = "*lr_results_hypwise_balanced_gen.csv") %>%
      map_df(read_csv, .id = "model") %>%
      mutate(
        model = str_extract(model, glue("(?<=simulation{mode}/)(.*)(?=/best_lr_results_hypwise_balanced_gen.csv)"))
      ),
    dir_ls(glue("data/results/single_stimuli_dative_simulation{mode}2/"), recurse = TRUE, regexp = "*lr_results_hypwise_balanced_gen.csv") %>%
      map_df(read_csv, .id = "model") %>%
      mutate(
        model = str_extract(model, glue("(?<=simulation{mode}2/)(.*)(?=/best_lr_results_hypwise_balanced_gen.csv)"))
      ) 
  ) %>%
    filter(!item_id %in% td_ids),
  dir_ls(glue("data/results/single_stimuli_dative_simulation{mode}3/"), recurse = TRUE, regexp = "*lr_results_hypwise_balanced_gen.csv") %>%
    map_df(read_csv, .id = "model") %>%
    mutate(
      model = str_extract(model, glue("(?<=simulation{mode}3/)(.*)(?=/best_lr_results_hypwise_balanced_gen.csv)"))
    ),
) %>%
  mutate(
    seed = as.numeric(str_remove(model, "smolm-autoreg-bpe-seed_"))
  ) %>%
  # inner_join(feature_configs %>% rename(adaptation_feature_config = feature_config)) %>%
  inner_join(adaptation %>% rename(adaptation_dative = dative)) %>%
  filter(seed %in% c(6, 28, 221, 394, 496, 1024, 1102, 1729, 2309, 8128))

balgen_results %>%
  rename(balanced_logprob = logprob) %>%
  inner_join(results %>% filter(state == "best") %>% rename(real_logprob = logprob)) %>%
  mutate(
    context = "no-context"
  ) %>%
  # filter(adaptation_dative != generalization_dative) %>%
  select(context, model, seed, item_id, hypothesis_id, hypothesis_instance, lr, adaptation_dative, generalization_dative, val_performance, balanced_logprob, real_logprob) %>%
  write_csv("data/paper-results/cross-dative/no-context.csv")


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


results %>%
  inner_join(adaptation %>% rename(adaptation_dative = dative)) %>%
  mutate(
    model = str_extract(model, "\\d{1,}")
  ) %>% 
  mutate(
    # feature_config = canonicality
    # feature_config = distinctiveness,
    # feature_config = glue::glue("{recipient_pronominality}-recipient\n{theme_pronominality}-theme"),
    feature_config = glue::glue("{substr(recipient_animacy,1,1)}-r\n{substr(theme_animacy,1,1)}-t"),
    # feature_config = glue::glue("{recipient_animacy}-r\n{theme_animacy}-t"),
    # feature_config = glue::glue("{recipient_animacy}"),
    # feature_config = glue::glue("{theme_animacy}-theme"),
    # feature_config = glue::glue("{theme_pronominality}-theme")
    # feature_config = glue::glue("{recipient_pronominality}-recip")
    # feature_config = glue::glue("{recipient_animacy}\n{recipient_pronominality}\n{recipient_length}"),
    # feature_config = glue::glue("{theme_animacy}\n{theme_pronominality}\n{theme_length}"),
    # feature_config = glue::glue("{recipient_animacy}\n{recipient_pronominality}\n{theme_animacy}\n{theme_pronominality}"),
    # feature_config = glue::glue("{recipient_definiteness}-recip\n{theme_definiteness}-theme"),
    # feature_config = glue::glue("{recipient_definiteness}-recip"),
    # feature_config = glue::glue("{theme_definiteness}-theme"),
  ) %>%
  filter(state == "best") %>% 
  # filter(!str_detect(model, "333|444|555|666|777|888|999|1709")) %>%
  # filter(!str_detect(sentence, "(dolly|teddy)")) %>%
  # mutate(
  #   theme_animacy = case_when(
  #     str_detect(theme, "(dolly|teddy)") ~ "animate",
  #     TRUE ~ theme_animacy
  #   ),
  #   recipient_animacy = case_when(
  #     str_detect(recipient, "(dolly|teddy)") ~ "animate",
  #     TRUE ~ recipient_animacy
  #   )
  # ) %>%
  # filter(theme_length == "short", recipient_length == "short") %>%
  # inner_join(adaptation %>% rename(adaptation_dative = dative)) %>%
  # filter(str_detect(adaptation_feature_config, "p(.*)(.*)p(.*)(.*)")) %>%
  # filter(str_detect(adaptation_feature_config, "(.*)a(.*)(.*)a(.*)")) %>%
  # filter(theme_length == "short", recipient_length == "short") %>%
  # filter(theme_animacy == "animate", recipient_animacy == "animate") %>%
  # filter(canonicality == "nc-marked") %>%
  # filter(theme_markedness == "marked", recipient_markedness == "marked") %>%
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
  # filter(adaptation_dative == "DO") %>%
  ggplot(aes(feature_config, logprob, color = generalization_dative)) +
  geom_point(size = 3) +
  # geom_line(aes(group = model)) +
  geom_linerange(aes(ymin = logprob-ste, ymax=logprob+ste)) +
  facet_wrap(~exposure, nrow=1) +
  # scale_color_manual(values = c("#00BFC4")) +
  # facet_grid(exposure ~ model) +
  # theme_bw(base_size = 14, base_family = "Times") +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    x = "Feature Configuration",
    y = "log P(alt-form)",
    color = "Generalization Dative"
  )

best_results <- results %>%
  pivot_wider(names_from = state, values_from = logprob) %>%
  mutate(
    diff = best - initial
  ) 
# %>%
  # filter(!str_detect(sentence, "(dolly|teddy)"))

t.test(
  best_results %>% 
    filter(adaptation_dative == "pp", generalization_dative == "do", recipient_animacy == "animate") %>% 
    pull(best),
  best_results %>% 
    filter(adaptation_dative == "pp", generalization_dative == "do", recipient_animacy == "inanimate") %>% 
    pull(best)
)


best_results %>%
  filter(theme_definiteness == "definite", recipient_definiteness == "definite") %>%
  # filter(theme_pronominality == "nominal", recipient_pronominality == "nominal") %>%
  mutate(
    # config2 = glue("{recipient_animacy}-R\n{theme_animacy}-T"),
    config2 = theme_animacy,
    config1 = ""
  ) %>%
  group_by(config1, config2, adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(best),
    logprob = mean(best)
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

coded_results <- best_results %>%
  # filter(!str_detect(sentence, "(dolly|teddy)")) %>%
  # filter(adaptation_dative == "do", generalization_dative == "pp") %>%
  mutate(
    # recipient_pronominality = factor(recipient_pronominality),
    # theme_pronominality = factor(theme_pronominality),
    recipient_pronominality = case_when(
      recipient_pronominality == "pronoun" ~ 0.5,
      TRUE ~ -0.5
    ),
    theme_pronominality = case_when(
      theme_pronominality == "pronoun" ~ 0.5,
      TRUE ~ -0.5
    ),
    recipient_animacy = case_when(
      recipient_animacy == "animate" ~ 0.5,
      TRUE ~ -0.5
    ),
    theme_animacy = case_when(
      theme_animacy == "animate" ~ 0.5,
      TRUE ~ -0.5
    ),
    theme_definiteness = case_when(
      theme_definiteness == "definite" ~ 0.5,
      TRUE ~ -0.5
    ),
    recipient_definiteness = case_when(
      recipient_definiteness == "definite" ~ 0.5,
      TRUE ~ -0.5
    ),
    theme_length = case_when(
      theme_length == "short" ~ 0.5,
      TRUE ~ -0.5
    ),
    recipient_length = case_when(
      recipient_length == "short" ~ 0.5,
      TRUE ~ -0.5
    ),
    distinctiveness = case_when(
      distinctiveness == "distinct" ~ 0.5,
      TRUE ~ -0.5
    ),
    model = factor(model),
    item_id = factor(item_id),
    hypothesis_id = factor(hypothesis_id)
  )


do_pp <- coded_results %>%
  filter(adaptation_dative == "do", generalization_dative == "pp")

# interactions between theme-animacy and recipient animacy; theme-pronominality and recipient-pronominality

fit_tara_tprp_interaction_dopp <- lmer(best ~ 
                                         theme_animacy * theme_pronominality +
                                         recipient_animacy * recipient_pronominality +
                                         theme_animacy * recipient_animacy +
                                         theme_pronominality * recipient_pronominality +
                                         theme_definiteness + recipient_definiteness +
                                         theme_length  + recipient_length +
                                         (1|model) + (1|item_id), data = do_pp)

summary(fit_tara_tprp_interaction_dopp, correlation = FALSE)


pp_do <- coded_results %>%
  filter(adaptation_dative == "pp", generalization_dative == "do")

fit_tara_tprp_interaction_ppdo <- lmer(best ~ 
                                         theme_animacy * theme_pronominality +
                                         recipient_animacy * recipient_pronominality +
                                         theme_animacy * recipient_animacy +
                                         theme_pronominality * recipient_pronominality +
                                         theme_definiteness + recipient_definiteness +
                                         theme_length  + recipient_length +
                                         (1|model) + (1|item_id), data = pp_do)

summary(fit_tara_tprp_interaction_ppdo, correlation = FALSE)

tidy(fit_tara_tprp_interaction_dopp) %>%
  filter(effect == "fixed") %>%
  select(term, do_pp_estimate = estimate, do_pp_pval = p.value) %>%
  inner_join(
    tidy(fit_tara_tprp_interaction_ppdo) %>%
      filter(effect == "fixed") %>%
      select(term, pp_do_estimate = estimate, pp_do_pval = p.value)
  ) %>%
  mutate(
    # do_pp_estimate = format(do_pp_estimate , scientific=TRUE),
    # pp_do_estimate = format(pp_do_estimate, scientific=TRUE),
    do_pp_estimate = round(do_pp_estimate, 4),
    pp_do_estimate = round(pp_do_estimate, 4),
    do_pp_signif = case_when(
      do_pp_pval < 0.05 ~ "**",
      TRUE ~ as.character(round(do_pp_pval, 4))
    ),
    pp_do_signif = case_when(
      pp_do_pval < 0.05 ~ "**",
      TRUE ~ as.character(round(pp_do_pval, 4))
    )
  ) %>%
  select(term, do_pp_estimate, do_pp_signif, pp_do_estimate, pp_do_signif) %>%
  DT::datatable(options = list(pageLength = 14))


# ----------

# best_model$recip_pron = sapply(best_model$recipient_pronominality,function(i) contr.sum(2)[i,])

# best_model$recipient_pronominality

fit1 <- lmer(best ~ theme_animacy + recipient_animacy:theme_animacy + theme_pronominality * recipient_pronominality +
               theme_definiteness + theme_length + 
               recipient_definiteness + recipient_length + distinctiveness + 
               (1|model) + (1|item_id),data = best_model, REML=FALSE)
fit2 <- lmer(best ~ theme_animacy * recipient_animacy + theme_pronominality * recipient_pronominality +
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

summary(fully_additive_fit)

# PP -> DO
# recipient animacy significance
anova(fit1,fit2)

# interaction between pronominality-features is significant.
anova(fit2, fit_pronominality_interaction)


# interaction between animacy-features is significant.
anova(fit2, fit_animacy_interaction)

summary(fit2)

emmip(fit2, recipient_animacy ~ theme_animacy)

summary(fit_pronominality_interaction)

# fit1 <- lmer(
#   best ~ theme_animacy + recipient_animacy + 
#     theme_pronominality * recip_pron +
#     theme_definiteness + theme_length + 
#     recipient_definiteness + recipient_length + 
#     distinctiveness +
#     (1|model) + (1|item_id), 
#   data = best_model,
#   # data = best_results %>% 
#     # filter(adaptation_dative == "do", generalization_dative == "pp") %>%
#     # mutate(case_when(recipient_pronominality == "pronoun" ~ 1, TRUE ~ -1)),
#   REML = FALSE
# )
# 
# summary(fit1)
# 
# fit2 <- lmer(
#   best ~ theme_animacy + recipient_animacy + 
#     theme_pronominality + theme_pronominality:recip_pron + 
#     theme_definiteness + theme_length + 
#     recipient_definiteness + recipient_length + 
#     (1|model) + (1|item_id), 
#   data = best_model,
#   # data = best_results %>% 
#   #   filter(adaptation_dative == "do", generalization_dative == "pp") %>%
#   #   mutate(case_when(recipient_pronominality == "pronoun" ~ 1, TRUE ~ -1)),
#   REML = FALSE
# )
# 
# summary(fit2)
# 
# anova(fit1,fit2)


results %>%
  filter(generalization_dative != adaptation_dative) %>%
  filter(theme_definiteness == "definite", recipient_definiteness=="definite") %>%
  filter(theme_pronominality != "pronoun", recipient_pronominality != "pronoun") %>%
  filter(str_detect(sentence, "the")) %>%
  filter(state == "best") %>%
  # filter(!str_detect(sentence, "(dolly|teddy)")) %>%
  mutate(
    # config2 = glue("{theme_animacy}\n{recipient_animacy}"),
    # config1 = distinctiveness,
    config2 = glue("{recipient_animacy}-Recip\n{theme_animacy}-Thm"),
    # config2 = glue("{theme_pronominality}\n{recipient_pronominality}"),
    # config2 = recipient_animacy,
    config1 = ""
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


sudha <- best_results %>%
  filter(adaptation_dative == "pp", generalization_dative == "do") %>%
  # filter(theme_definiteness == "definite", recipient_definiteness == "definite") %>%
  # filter(theme_length == "short", recipient_length == "short") %>%
  # filter(theme_pronominality != "pronoun", recipient_pronominality != "pronoun") %>%
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
    item_id = factor(item_id)
  )

# question: does animate recipient and inanimate theme make pp -> do most likely?


recipient_fit <- lmer(best ~ theme_animacy + recipient_animacy:theme_animacy + (1|model) + (1|item_id),data = sudha)
theme_fit <- lmer(best ~ recipient_animacy + recipient_animacy:theme_animacy + (1|model) + (1|item_id),data = sudha)
full_model <- lmer(best ~ theme_animacy * recipient_animacy + (1|model) + (1|item_id),data = sudha)

anova(theme_fit, full_model)
anova(recipient_fit, full_model)

summary(full_model)

best_results %>%
  filter(adaptation_dative != generalization_dative) %>%
  group_by(adaptation_dative, generalization_dative, theme_animacy, 
           theme_pronominality, theme_length, theme_definiteness, theme_markedness,
           recipient_pronominality, recipient_animacy, recipient_length, recipient_definiteness,
           recipient_markedness, distinctiveness, sentence) %>%
  summarize(
    best = mean(best)
  ) %>%
  View()

