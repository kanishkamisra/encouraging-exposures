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
library(ggdist)
library(ggstance)

read_adaptation <- function(template_num) {
  
  stimuli <- glue("data/experiments/givenness_template_{template_num}.jsonl") %>% 
    file() %>%
    stream_in() %>% 
    as_tibble() %>% 
    mutate(givenness_template = template_num) %>%
    mutate(
      theme_givenness = case_when(
        template %in% c("agent-only", "agent-recipient") ~ "new",
        TRUE ~ "given"
      ),
      recipient_givenness = case_when(
        template %in% c("agent-only", "agent-theme") ~ "new",
        TRUE ~ "given"
      )
    )
  
  return(stimuli)
}

all_results <- dir_ls("data/results/simulation-results/", regexp = "*/results.csv", recurse = TRUE) %>%
  map_df(read_csv, .id = "file") %>%
  mutate(
    givenness_template = as.numeric(str_extract(file, "(?<=givenness_template_)(.*)(?=/smolm)")),
    seed = as.numeric(str_extract(file, "(?<=seed_)(.*)(?=/results)"))
  ) %>%
  inner_join(adaptation) %>%
  # filter(theme_pronominality != recipient_pronominality & theme_animacy != recipient_animacy & theme_definiteness != recipient_definiteness) %>%
  mutate(
    givenness_template = factor(givenness_template),
    seed = factor(seed)
  )

haaps <- all_results %>%
  mutate(
    theme_pronominality = case_when(
      dative == "pp" & theme_pronominality == "pronoun" ~ 1,
      dative == "pp" & theme_pronominality == "noun" ~ 0,
      dative == "do" & theme_pronominality == "pronoun" ~ 0,
      dative == "do" & theme_pronominality == "noun" ~ 1
    ),
    recipient_pronominality = case_when(
      dative == "do" & recipient_pronominality == "pronoun" ~ 1,
      dative == "do" & recipient_pronominality == "noun" ~ 0,
      dative == "pp" & recipient_pronominality == "pronoun" ~ 0,
      dative == "pp" & recipient_pronominality == "noun" ~ 1
    ),
    theme_animacy = case_when(
      dative == "pp" & theme_animacy == "animate" ~ 0,
      dative == "pp" & theme_animacy == "inanimate" ~ 1,
      dative == "do" & theme_animacy == "inanimate" ~ 1,
      dative == "do" & theme_animacy == "animate" ~ 0,
    ),
    recipient_animacy = case_when(
      dative == "do" & recipient_animacy == "animate" ~ 1,
      dative == "do" & recipient_animacy == "inanimate" ~ 0,
      dative == "pp" & recipient_animacy == "animate" ~ 1,
      dative == "pp" & recipient_animacy == "inanimate" ~ 0,
    ),
    theme_definiteness = case_when(
      dative == "pp" & theme_definiteness == "definite" ~ 1,
      dative == "pp" & theme_definiteness == "indefinite" ~ 0,
      dative == "do" & theme_definiteness == "definite" ~ 0,
      dative == "do" & theme_definiteness == "indefinite" ~ 1,
    ),
    recipient_definiteness = case_when(
      dative == "do" & recipient_definiteness == "definite" ~ 1,
      dative == "do" & recipient_definiteness == "indefinite" ~ 0,
      dative == "pp" & recipient_definiteness == "definite" ~ 0,
      dative == "pp" & recipient_definiteness == "indefinite" ~ 1,
    ),
    theme_givenness = case_when(
      dative == "do" & theme_givenness == "new" ~ 1,
      dative == "do" & theme_givenness == "given" ~ 0,
      dative == "pp" & theme_givenness == "given" ~ 1,
      dative == "pp" & theme_givenness == "new" ~ 0
    ),
    recipient_givenness = case_when(
      dative == "do" & recipient_givenness == "new" ~ 0,
      dative == "do" & recipient_givenness == "given" ~ 1,
      dative == "pp" & recipient_givenness == "given" ~ 0,
      dative == "pp" & recipient_givenness == "new" ~ 1
    ),
    length_diff = case_when(
      dative == "pp" ~ -1 * length_diff,
      TRUE ~ length_diff
    ),
    length_score = case_when(
      length_diff == 0 ~ 0,
      length_diff > 0 ~ log(length_diff)+1,
      length_diff < 0 ~ -(log(abs(length_diff))+1)
    ),
    alt_length_score = case_when(
      length_diff == 0 ~ 0,
      length_diff > 0 ~ 1,
      length_diff < 0 ~ -1
    ),
    haap_theme = theme_pronominality + theme_animacy + theme_definiteness + theme_givenness,
    haap_recipient = recipient_pronominality + recipient_animacy + recipient_definiteness + recipient_givenness,
    haap_nolen = haap_theme + haap_recipient,
    haap = haap_nolen + length_score,
    haap_alt = haap_nolen + alt_length_score,
    discrete_theme = theme_pronominality + theme_definiteness + theme_givenness,
    discrete_recipient = recipient_pronominality + recipient_definiteness + recipient_givenness,
    ha = discrete_theme + discrete_recipient,
    ap = theme_animacy + recipient_animacy
  )

haaps %>%
  count(dative, haap_nolen) %>%
  pivot_wider(names_from = dative, values_from = n)

haaps %>%
  filter(givenness_template == 2, seed == 211) %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative != dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}"),
  ) %>%
  # count(exp, haap_nolen)
  ggplot(aes(haap, fill = exp, group = exp)) +
  geom_histogram(color = NA, show.legend = FALSE) +
  facet_wrap(~exp) +
  theme_bw(base_size = 16)

bin2contrast <- function(x) {
  stopifnot(x %in% c(0,1))
  return (2*x - 1)
}


haaps_contrast <- haaps %>%
  mutate_at(vars(starts_with("theme_") | starts_with(("recipient_"))), bin2contrast) %>%
  mutate(
    haap_theme = theme_pronominality + theme_animacy + theme_definiteness + theme_givenness,
    haap_recipient = recipient_pronominality + recipient_animacy + recipient_definiteness + recipient_givenness,
    haap_nolen = haap_theme + haap_recipient,
    haap = haap_nolen + length_score,
    haap_alt = haap_nolen + alt_length_score,
    discrete_theme = theme_pronominality + theme_definiteness + theme_givenness,
    discrete_recipient = recipient_pronominality + recipient_definiteness + recipient_givenness,
    ha = discrete_theme + discrete_recipient,
    ap = theme_animacy + recipient_animacy
  )

# fits
fit_haap_overall_do_pp <- lmer(pp ~ haap + (1 | seed) + (1 | givenness_template),
                               data = haaps %>% filter(dative == "do"))

summary(fit_haap_overall_do_pp)

fit_haap_overall_pp_do <- lmer(do ~ haap + (1 | seed) + (1 | givenness_template),
                               data = haaps %>% filter(dative == "pp"))

summary(fit_haap_overall_pp_do)

fit_haap_do_pp <- lmer(pp ~ haap_theme + haap_recipient + length_score + (1 | seed) + (1 | givenness_template),
                       data = haaps %>% filter(dative == "do"))
summary(fit_haap_do_pp)

fit_haap_pp_do <- lmer(do ~ haap_theme + haap_recipient + length_score + (1 | seed) + (1 | givenness_template),
                       data = haaps %>% filter(dative == "pp"))
summary(fit_haap_pp_do)

bind_rows(
  fit_haap_do_pp %>%
    tidy() %>%
    filter(effect == "fixed", term != "(Intercept)") %>%
    mutate(exp = "DO -> PP"),
  fit_haap_pp_do %>%
    tidy() %>%
    filter(effect == "fixed", term != "(Intercept)") %>%
    mutate(exp = "PP -> DO")
) %>%
  mutate(
    term = str_remove(term, "haap_"),
    term = case_when(
      term == "length_score" ~ "length",
      TRUE ~ term
    ),
    term = str_to_title(term),
    term = factor(term, c("Theme", "Recipient", "Length"))
  ) %>%
  ggplot(aes(estimate, term)) +
  geom_point(size = 2) +
  geom_linerangeh(aes(xmin = estimate-std.error, xmax = estimate+std.error)) +
  geom_vline(xintercept = 0.0, linetype = "dashed") +
  facet_wrap(~exp, nrow = 2) +
  theme_bw(base_size = 16) +
  theme(
    panel.grid = element_blank()
  )

haaps %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative != dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}"),
  ) %>%
  group_by(exp, haap) %>%
  summarize(
    n = n(),
    sd = sd(score),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    score = mean(score)
  ) %>%
  ggplot(aes(haap, score)) +
  geom_point() +
  geom_linerange(aes(ymin = score-cb, ymax = score+cb)) +
  # geom_ribbon(aes(ymin = score-cb, ymax = score+cb), color = NA, alpha = 0.3) +
  facet_wrap(~exp)


haaps %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative != dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}"),
  )

haaps %>% 
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative != dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}")
  ) %>%
  select(seed, idx, template, givenness_template, exp, haap_theme, haap_recipient, score) %>%
  pivot_longer(haap_theme:haap_recipient, names_to = "metric", values_to = "haap") %>%
  group_by(exp, seed, metric, haap) %>%
  summarize(
    n = n(),
    sd = sd(score),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    score = mean(score)
  ) %>%
  ggplot(aes(haap, score, color = metric, fill = metric, group = interaction(metric, seed))) +
  geom_point() +
  geom_line() +
  # geom_linerange(aes(ymin = score-cb, ymax = score+cb)) +
  geom_ribbon(aes(ymin = score-cb, ymax = score+cb), color = NA, alpha = 0.3) +
  facet_wrap(~exp)

# complex

fit_haap_complex_do_pp <- lmer(pp ~ ha + ap + 
                                 length_score + (1 | seed) +
                                 (1 | givenness_template), 
                               data = haaps %>% filter(dative == "do"))

summary(fit_haap_complex_do_pp)

fit_haap_complex_pp_do <- lmer(do ~ ha + ap + 
                                 length_score + (1 | seed) +
                                 (1 | givenness_template), 
                               data = haaps %>% filter(dative == "pp"))

summary(fit_haap_complex_pp_do)

all_results %>%
  group_by(dative, theme_pronominality, theme_animacy, theme_definiteness, theme_givenness, recipient_pronominality, recipient_animacy, recipient_definiteness, recipient_givenness, length_diff) %>%
  summarize(
    do = mean(do),
    pp = mean(pp)
  ) %>% View()
