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

stimuli <- glue("data/experiments/arunachalam-final.jsonl") %>% 
  file() %>%
  stream_in() %>% 
  as_tibble()

read_stimuli <- function(x) {
  read_csv(x) %>%
    mutate(
      givenness_order = rep(c("theme_recipient", "theme_recipient", "recipient_theme", "recipient_theme"), 220)
    ) %>%
    inner_join(stimuli)
}

arunachalam <- dir_ls("data/results/simulation-results/arunachalam-final/arunachalam-final/", regexp = "*/results.csv", recurse = TRUE) %>%
  map_df(read_stimuli, .id = "file") %>%
  mutate(
    seed = as.numeric(str_extract(file, "(?<=seed_)(.*)(?=/results)"))
  ) %>%
  select(-file) %>%
  # filter(theme_pronominality != recipient_pronominality & theme_animacy != recipient_animacy & theme_definiteness != recipient_definiteness) %>%
  mutate(
    seed = factor(seed)
  ) %>%
  pivot_longer(do:pp, names_to = "generalization_dative", values_to = "logprob") %>%
  filter(generalization_dative == "do")

arunachalam  %>%
  group_by(dative, givenness_order) %>%
  summarize(
    n = n(),
    sd = sd(logprob),
    conf = qt(1 - (0.05/2), n - 1) * sd/sqrt(n),
    diff = mean(logprob)
  )

reg_data <- arunachalam %>%
  mutate(
    dative = case_when(
      dative == "pp" ~ 1,
      TRUE ~ 0
    ),
    theme_animacy = case_when(
      theme_animacy == "animate" ~ 1,
      TRUE ~ 0
    ),
    givenness_order = factor(givenness_order)
  )

fit_arunachalam <- lmer(logprob ~ dative * theme_animacy + (1 | seed) + (1 | givenness_order), data = reg_data)
summary(fit_arunachalam)

arunachalam  %>%
  group_by(dative, theme_animacy) %>%
  summarize(
    n = n(),
    sd = sd(logprob),
    conf = qt(1 - (0.05/2), n - 1) * sd/sqrt(n),
    logprob = mean(logprob)
  ) %>%
  ggplot(aes(theme_animacy, logprob)) +
  geom_point() +
  geom_linerange(aes(ymin = logprob-conf, ymax=logprob+conf))+
  facet_wrap(~dative) +
  labs(
    y = "Logprob of DO (generalization)"
  )
