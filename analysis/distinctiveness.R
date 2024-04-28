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

adaptation <- bind_rows(adaptation1, adaptation2)

pronouns = c("him", "her", "them", "me", "us", "it", "something", "someone")
propernouns = c("bear", "bear over there", "mommy", 
                "daddy", "grandma", "cat", "dog", "dog", 
                "bert", "elmo", "dog outside the house", 
                "cat outside the house", "teddy", "dolly",
                "cookies", "cheerios")

propro = c(pronouns, propernouns)

adaptation %>%
  mutate(
    distinctiveness = case_when(
      theme %in% propro & !recipient %in% propro ~ "distinct",
      theme %in% propro & recipient %in% propro ~ "not_distinct",
      !theme %in% propro & recipient %in% propro ~ "distinct",
      !theme %in% propro & !recipient %in% propro ~ "not_distinct"
    )
  ) %>%
  filter(canonicality != "nc-marked") %>%
  count(recipient_animacy)
