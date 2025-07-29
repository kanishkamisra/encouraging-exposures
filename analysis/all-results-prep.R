library(tidyverse)

final_results <- read_csv("data/results/final_results_250716.csv")

final_results %>%
  mutate(
    rg = substr(recipient_givenness, 1,1),
    tg = substr(theme_givenness, 1,1),
    full_code = glue::glue("{combo}_{tg}{rg}")
  ) %>%
  mutate(
    theme_pronominality = case_when(
      theme_pronominality == "pronoun" ~ 1,
      TRUE ~ 0
    ),
    recipient_pronominality = case_when(
      recipient_pronominality == "pronoun" ~ 1,
      TRUE ~ 0
    ),
    theme_animacy = case_when(
      theme_animacy == "animate" ~ 1,
      TRUE ~ 0
    ),
    recipient_animacy = case_when(
      recipient_animacy == "animate" ~ 1,
      TRUE ~ 0
    ),
    theme_definiteness = case_when(
      theme_definiteness == "definite" ~ 1,
      TRUE ~ 0
    ),
    recipient_definiteness = case_when(
      recipient_definiteness == "definite" ~ 1,
      TRUE ~ 0
    ),
    theme_givenness = case_when(
      # template %in% c("agent-only", "agent-recipient") ~ 0,
      theme_givenness == "given" ~ 1,
      TRUE ~ 0
    ),
    recipient_givenness = case_when(
      # template %in% c("agent-only", "agent-theme") ~ 0,
      recipient_givenness == "given" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(seed, givenness_template, exp, theme_pronominality, theme_animacy, 
         theme_definiteness, theme_givenness, recipient_pronominality, recipient_animacy, 
         recipient_definiteness, recipient_givenness, length_diff, score) %>%
  write_csv("data/results/final_results_250716_coded.csv")
