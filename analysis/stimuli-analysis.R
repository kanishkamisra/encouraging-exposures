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

adaptation <- bind_rows(
  read_adaptation(1),
  read_adaptation(2),
  read_adaptation(3),
) 

adaptation %>%
  filter(givenness_template == 2) %>%
  # count(dative, theme_pronominality, theme_animacy, theme_definiteness, theme_givenness,
  #       recipient_pronominality, recipient_animacy, recipient_definiteness, recipient_givenness) %>%
  # # pivot_wider(names_from = dative, values_from = n) %>%
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
    ha_theme = theme_pronominality + theme_animacy + theme_definiteness + theme_givenness,
    ha_recipient = recipient_pronominality + recipient_animacy + recipient_definiteness + recipient_givenness,
    ha_score = ha_theme + ha_recipient
  ) %>%
  count(dative, ha_score)
  count(dative, theme_pronominality, theme_animacy, theme_definiteness, theme_givenness,
        recipient_pronominality, recipient_animacy, recipient_definiteness, recipient_givenness) %>%
  pivot_wider(names_from = dative, values_from = n,values_fill = 0) %>%
  mutate(
    pairs = glue("{pp}_{do}")
  ) %>% count(pairs)
  # mutate(
  #   pairs = case_when(
  #     do > pp ~ glue("{pp}_{do}"),
  #     TRUE ~ glue("{do}_{pp}")
  #   )
  # ) %>% View()
  # count(pairs)

