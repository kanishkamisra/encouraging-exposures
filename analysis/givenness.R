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
  
  adaptation_theme <- read_adaptation(theme_given)
  adaptation_recipient <- read_adaptation(recipient_given)
  
  bind_rows(adaptation_theme, adaptation_recipient)
}

adaptation1 <- read_givenness_adaptation("1")
adaptation2 <- read_givenness_adaptation("2")
adaptation3 <- read_givenness_adaptation("3")

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


adaptation %>%
  count(theme_pronominality, theme_animacy, theme_length, theme_definiteness,
        recipient_pronominality, recipient_animacy, recipient_length, recipient_definiteness,
        dative) %>%
  View("counts")

adaptation %>%
  filter(
    theme_pronominality == "nominal",
    theme_animacy == "animate",
    theme_length == "long",
    theme_definiteness == "indefinite",
    recipient_pronominality == "nominal",
    recipient_animacy == "animate",
    recipient_length == "short",
    recipient_definiteness == "definite"
  ) %>%
  View()
