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

mode = "_valtest_vbd_discourse_theme_given_template_1"

read_results <- function(mode) {
  results <- dir_ls(glue("data/results/single_stimuli_dative_simulation{mode}/"), recurse = TRUE, regexp = "*lr_results_hypwise.csv") %>%
  map_df(read_csv, .id = "model") %>%
  mutate(
    model = str_extract(model, glue("(?<=simulation{mode}/)(.*)(?=/best_lr_results_hypwise.csv)"))
  ) %>%
  mutate(
    seed = as.numeric(str_remove(model, "smolm-autoreg-bpe-seed_"))
  )
  return(results)
}

read_balgen_results <- function(mode) {
  results <- dir_ls(glue("data/results/single_stimuli_dative_simulation{mode}/"), recurse = TRUE, regexp = "*lr_results_hypwise_balanced_gen.csv") %>%
    map_df(read_csv, .id = "model") %>%
    mutate(
      model = str_extract(model, glue("(?<=simulation{mode}/)(.*)(?=/best_lr_results_hypwise_balanced_gen.csv)"))
    ) %>%
    mutate(
      seed = as.numeric(str_remove(model, "smolm-autoreg-bpe-seed_"))
    )
  return(results)
}

read_givenness_balgen_results <- function(template) {
  theme = glue::glue("_valtest_vbd_discourse_theme_given_template_{template}")
  recipient = glue::glue("_valtest_vbd_discourse_recipient_given_template_{template}")
  
  theme_results <- read_balgen_results(theme) %>% mutate(given = "theme", template = template)
  recipient_results <- read_balgen_results(recipient) %>% mutate(given = "recipient", template = template)
  
  bind_rows(theme_results, recipient_results)
}

read_givenness_balgen_legofix_results <- function(template) {
  theme = glue::glue("_valtest_vbd_discourse_theme_given_legofix_template_{template}")
  recipient = glue::glue("_valtest_vbd_discourse_recipient_given_legofix_template_{template}")
  
  theme_results <- read_balgen_results(theme) %>% mutate(given = "theme", template = template)
  recipient_results <- read_balgen_results(recipient) %>% mutate(given = "recipient", template = template)
  
  bind_rows(theme_results, recipient_results)
}


read_givenness_results <- function(template) {
  theme = glue::glue("_valtest_vbd_discourse_theme_given_template_{template}")
  recipient = glue::glue("_valtest_vbd_discourse_recipient_given_template_{template}")
  
  theme_results <- read_results(theme) %>% mutate(given = "theme", template = template)
  recipient_results <- read_results(recipient) %>% mutate(given = "recipient", template = template)
  
  bind_rows(theme_results, recipient_results)
}

read_givenness_legofix_results <- function(template) {
  theme = glue::glue("_valtest_vbd_discourse_theme_given_legofix_template_{template}")
  recipient = glue::glue("_valtest_vbd_discourse_recipient_given_legofix_template_{template}")
  
  theme_results <- read_results(theme) %>% mutate(given = "theme", template = template)
  recipient_results <- read_results(recipient) %>% mutate(given = "recipient", template = template)
  
  bind_rows(theme_results, recipient_results)
}

read_adaptation <- function(path) {
  adapt <- stream_in(file(path)) %>%
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
  
  return(adapt)
}

read_givenness_adaptation <- function(template) {
  
  theme_given <- glue("data/experiments/single_stimuli_dative_simulation_valtest_vbd_discourse_theme_given_template_{template}/adaptation.jsonl")
  recipient_given <- glue("data/experiments/single_stimuli_dative_simulation_valtest_vbd_discourse_recipient_given_template_{template}/adaptation.jsonl")
  
  adaptation_theme <- read_adaptation(theme_given) %>% mutate(template = template)
  adaptation_recipient <- read_adaptation(recipient_given) %>% mutate(template = template)
  
  bind_rows(adaptation_theme, adaptation_recipient)
}

read_givenness_adaptation_legofix <- function(template) {
  
  theme_given <- glue("data/experiments/single_stimuli_dative_simulation_valtest_vbd_discourse_theme_given_legofix_template_{template}/adaptation.jsonl")
  recipient_given <- glue("data/experiments/single_stimuli_dative_simulation_valtest_vbd_discourse_recipient_given_legofix_template_{template}/adaptation.jsonl")
  
  adaptation_theme <- read_adaptation(theme_given) %>% mutate(template = template)
  adaptation_recipient <- read_adaptation(recipient_given) %>% mutate(template = template)
  
  bind_rows(adaptation_theme, adaptation_recipient)
}


adaptation <- bind_rows(
  read_givenness_adaptation("1"),
  read_givenness_adaptation("2"),
  read_givenness_adaptation("3")
)

adaptation_legofix <- bind_rows(
  read_givenness_adaptation_legofix("1"),
  read_givenness_adaptation_legofix("2"),
  read_givenness_adaptation_legofix("3")
)

fix_ids <- adaptation_legofix %>%
  distinct(item) %>%
  pull(item)

# adaptation1 <- read_givenness_adaptation("1")
# adaptation2 <- read_givenness_adaptation("2")
# adaptation3 <- read_givenness_adaptation("3")

results <- bind_rows(
  read_givenness_results("1"),
  read_givenness_results("2"),
  read_givenness_results("3")
)

results_legofix <- bind_rows(
  read_givenness_legofix_results("1"),
  read_givenness_legofix_results("2"),
  read_givenness_legofix_results("3")
)

balgen_results <- bind_rows(
  read_givenness_balgen_results("1"),
  read_givenness_balgen_results("2"),
  read_givenness_balgen_results("3"),
)

balgen_legofix_results <- bind_rows(
  read_givenness_balgen_legofix_results("1"),
  read_givenness_balgen_legofix_results("2"),
  read_givenness_balgen_legofix_results("3"),
)

results_fix <- bind_rows(
  results %>%
    filter(!item_id %in% fix_ids),
  results_legofix
)

balgen_results_fix <- bind_rows(
  balgen_results %>%
    filter(!item_id %in% fix_ids),
  balgen_legofix_results
)

generalization <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation{mode}/generalization.jsonl"))) %>%
  as_tibble()


results %>%
  filter(state == "best") %>%
  group_by(model, item_id, hypothesis_id, hypothesis_instance, adaptation_dative, generalization_dative, given) %>%
  nest() %>%
  ungroup() %>%
  slice(1) %>%
  pull(data) %>% .[[1]] 


best_results <- results_fix %>%
  filter(state == "best") %>%
  group_by(model, item_id, hypothesis_id, hypothesis_instance, adaptation_dative, generalization_dative, given) %>%
  nest() %>% 
  mutate(
    best = map(data, function(x) {
      x %>%
        filter(val_performance == max(val_performance)) %>%
        slice(1)
    })
  ) %>%
  select(-data) %>%
  unnest(best) %>%
  ungroup() %>%
  inner_join(adaptation %>% rename(adaptation_dative = dative))


balgen_results_best <- balgen_results_fix %>%
  inner_join(best_results %>% select(-logprob)) %>%
  filter(adaptation_dative != generalization_dative) %>%
  mutate(
    experiment = case_when(
      adaptation_dative == "pp" ~ 1,
      TRUE ~ 0
    )
  )

# write balgen and best results to csv
balgen_results_fix %>%
  rename(balanced_logprob = logprob) %>%
  inner_join(best_results %>% rename(real_logprob = logprob)) %>%
  mutate(
    context = case_when(
      given == "theme" ~ "theme-given",
      TRUE ~ "recipient-given"
    )
  ) %>%
  select(context, model, seed, item_id, hypothesis_id, hypothesis_instance, lr, adaptation_dative, generalization_dative, val_performance, balanced_logprob, real_logprob) %>%
  write_csv("data/paper-results/cross-dative/theme-recipient-given.csv")
  
  
# balgen_results_best %>%  
best_results %>%
  group_by(adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    experiment = glue::glue("{adaptation_dative} --> {generalization_dative}")
  ) %>%
  ggplot(aes(experiment, logprob)) +
  # geom_boxplot()
  # geom_jitter(size = 2, width = 0.1)
  geom_point(size = 3, width = 0.2) +
  geom_linerange(aes(ymin = logprob-ste, ymax=logprob+ste)) +
  # facet_wrap(~model, scales="free") +
  labs(
    x = "Generalization Experiment",
    y = "Average Log P(construction)"
  ) +
  theme(
    axis.text.x = element_markdown(),
    panel.grid = element_blank()
  )

balgen_fit <- lmer(logprob ~ experiment + (1 + experiment | model), data = balgen_results_best)

summary(balgen_fit)

best_results %>%
  inner_join(adaptation %>% rename(adaptation_dative = dative)) %>%
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
  # facet_wrap(~model, scales="free") +
  labs(
    x = "Generalization Experiment",
    y = "Average Log P(construction)"
  ) +
  theme(
    axis.text.x = element_markdown(),
    panel.grid = element_blank()
  )


best_results %>%
  mutate(
    best = logprob,
    # config2 = glue("{recipient_animacy}-R\n{theme_animacy}-T"),
    config2 = glue("{given}-given"),
    config1 = ""
  ) %>%
  group_by(config1, config2, seed, adaptation_dative, generalization_dative) %>%
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

sudha <- best_results %>%
  filter(theme_definiteness == "definite", recipient_definiteness == "definite") %>%
  # filter(recipient_animacy == "animate")
  filter(recipient_animacy == "animate", 
         recipient_pronominality != "pronoun", 
         theme_pronominality != "pronoun", 
         theme_length == "short", 
         recipient_length == "short", str_detect(theme, "the"), str_detect(recipient, "the"))



sudha %>%
  # best_results %>%
  # filter(theme_definiteness == "definite", recipient_definiteness == "definite") %>%
  # filter(recipient_animacy == "animate", recipient_pronominality != "pronoun", theme_pronominality != "pronoun", theme_length == "short", recipient_length == "short") %>%
  mutate(
    # config2 = glue("{recipient_animacy}-R\n{theme_animacy}-T"),
    config2 = theme_animacy,
    # config2 = recipient_animacy,
    config1 = ""
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

sudha_coded <- sudha %>%
  mutate(
    best = logprob,
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
      theme_definiteness == "definite" ~ 1,
      TRUE ~ 0
    ),
    recipient_definiteness = case_when(
      recipient_definiteness == "definite" ~ 1,
      TRUE ~ 0
    ),
    theme_length = case_when(
      theme_length == "short" ~ 1,
      TRUE ~ 0
    ),
    recipient_length = case_when(
      recipient_length == "short" ~ 1,
      TRUE ~ 0
    ),
    distinctiveness = case_when(
      distinctiveness == "distinct" ~ 1,
      TRUE ~ 0
    ),
    theme_given = case_when(
      given == "theme" ~ 1,
      TRUE ~ 0
    ),
    recipient_given = case_when(
      given == "recipient" ~ 1,
      TRUE ~ 0
    ),
    model = factor(model),
    item_id = factor(item_id),
    hypothesis_id = factor(hypothesis_id)
  )

t.test(sudha %>% 
         filter(adaptation_dative == "do", generalization_dative == "pp", theme_animacy == "animate") %>%
         pull(logprob),
       sudha %>% 
         filter(adaptation_dative == "do", generalization_dative == "pp", theme_animacy == "inanimate") %>%
         pull(logprob),
       )

do_pp_sudha <- sudha_coded %>%
  filter(adaptation_dative == "do", generalization_dative == "pp")

fit_dopp_sudha <- lmer(best ~ theme_animacy + (1 | model), data = do_pp_sudha)

summary(fit_dopp_sudha, correlation = FALSE)


pp_do_sudha <- sudha_coded %>%
  filter(adaptation_dative == "pp", generalization_dative == "do")

fit_ppdo_sudha <- lmer(best ~ theme_animacy + (1 | model), data = pp_do_sudha)

summary(fit_ppdo_sudha, correlation = FALSE)

coded_results <- best_results %>%
  # filter(!str_detect(sentence, "(dolly|teddy)")) %>%
  # filter(adaptation_dative == "do", generalization_dative == "pp") %>%
  mutate(
    # recipient_pronominality = factor(recipient_pronominality),
    # theme_pronominality = factor(theme_pronominality),
    best = logprob,
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
      theme_definiteness == "definite" ~ 1,
      TRUE ~ 0
    ),
    recipient_definiteness = case_when(
      recipient_definiteness == "definite" ~ 1,
      TRUE ~ 0
    ),
    theme_length = case_when(
      theme_length == "short" ~ 1,
      TRUE ~ 0
    ),
    recipient_length = case_when(
      recipient_length == "short" ~ 1,
      TRUE ~ 0
    ),
    distinctiveness = case_when(
      distinctiveness == "distinct" ~ 1,
      TRUE ~ 0
    ),
    theme_given = case_when(
      given == "theme" ~ 1,
      TRUE ~ 0
    ),
    recipient_given = case_when(
      given == "recipient" ~ 1,
      TRUE ~ 0
    ),
    model = factor(model),
    item_id = factor(item_id),
    hypothesis_id = factor(hypothesis_id)
  )

coded_results

do_pp <- coded_results %>%
  filter(adaptation_dative == "do", generalization_dative == "pp")

# interactions between theme-animacy and recipient animacy; theme-pronominality and recipient-pronominality

fit_tara_tprp_interaction_dopp <- lmer(best ~ 
                                         theme_animacy + theme_pronominality +
                                         recipient_animacy + recipient_pronominality +
                                         theme_animacy:recipient_animacy +
                                         theme_pronominality:recipient_pronominality +
                                         theme_animacy:theme_pronominality +
                                         recipient_animacy:recipient_pronominality +
                                         theme_definiteness + recipient_definiteness +
                                         theme_length  + recipient_length +
                                         theme_given +
                                         (1|model) + (1|hypothesis_id), data = do_pp)

summary(fit_tara_tprp_interaction_dopp, correlation = FALSE)

emmip(fit_tara_tprp_interaction_ppdo, theme_animacy ~ theme_pronominality)
emmeans(fit_tara_tprp_interaction_ppdo, pairwise ~ theme_animacy | theme_pronominality)


pp_do <- coded_results %>%
  filter(adaptation_dative == "pp", generalization_dative == "do")

# interactions between theme-animacy and recipient animacy; theme-pronominality and recipient-pronominality

fit_tara_tprp_interaction_ppdo <- lmer(best ~ 
                                         theme_animacy + theme_pronominality +
                                         recipient_animacy + recipient_pronominality +
                                         # theme_animacy:recipient_animacy + 
                                         # theme_pronominality:recipient_pronominality +
                                         theme_animacy:theme_pronominality +
                                         recipient_animacy:recipient_pronominality +
                                         theme_definiteness + recipient_definiteness +
                                         theme_length  + recipient_length +
                                         theme_given +
                                         (1|model), data = pp_do)

summary(fit_tara_tprp_interaction_ppdo, correlation = FALSE)

tidy(fit_tara_tprp_interaction_dopp, conf.int = TRUE)


# ---

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
