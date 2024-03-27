library(tidyverse)

mode = "_valtest_vbd_no_discourse"
adaptation <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation{mode}/adaptation.jsonl"))) %>%
  as_tibble()

adaptation %>%
  mutate(
    canonicality = case_when(
      theme_markedness == recipient_markedness ~ "unmarked",
      theme_pronominality == "pronoun" & recipient_pronominality == "pronoun" ~ "unmarked",
      theme_definiteness == "definite" & theme_pronominality != "pronoun" & recipient_pronominality == "pronoun" ~ "nc-marked",
      theme_markedness == "unmarked" & recipient_markedness == "marked" ~ "nc-marked",
      theme_markedness == "marked" & recipient_markedness == "unmarked" ~ "marked"
    )
  ) %>% 
  View("annotated")
  count(canonicality)

adaptation %>%
  filter(theme_markedness == "marked" & recipient_markedness == "unmarked") %>%
  View()
