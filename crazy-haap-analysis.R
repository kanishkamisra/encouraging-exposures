library(tidyverse)
library(fs)
library(lmerTest)
library(patchwork)

model_results <- read_csv("data/results/simulation-results/final-results-25-10-16.csv") %>%
  select(-file) %>%
  select(idx, seed, givenness_template, dative, do, pp)

multiverse <- fs::dir_ls("data/results/simulation-results/haap-25-10-16/") %>%
  map_df(read_csv, .id = "file") %>%
  inner_join(model_results) %>%
  mutate(
    length_diff = case_when(
      dative == "pp" ~ -1 * length_diff,
      TRUE ~ length_diff
    ),
    length_score = case_when(
      length_diff == 0 ~ 0,
      length_diff > 0 ~ log(length_diff)+1,
      length_diff < 0 ~ -(log(abs(length_diff))+1)
    ),
    seed = factor(seed),
    givenness_template = factor(givenness_template)
  )

code2haap <- multiverse %>% 
  distinct(code_id, haap_do, haap_po, haap_do_theme, haap_po_theme, haap_do_recipient, haap_po_recipient)


# fit1 <- lmer(pp ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template),
#              data = multiverse %>% filter(dative == "do", haap_do==TRUE))
# summary(fit1)
# 
# broom.mixed::glance(fit1)
# 
# fit2 <- lmer(pp ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template),
#              data = multiverse %>% filter(dative == "do", code_id == 201))
# broom.mixed::glance(fit2)

fits_do <- multiverse %>%
  filter(dative == "do") %>%
  group_by(code_id) %>%
  nest() %>%
  mutate(
    fit = map(data, function(x){
      fitted <- lmer(pp ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template), data = x)
      metrics <- broom.mixed::glance(fitted)
    })
  )

fits_pp <- multiverse %>%
  filter(dative == "pp") %>%
  group_by(code_id) %>%
  nest() %>%
  mutate(
    fit = map(data, function(x){
      fitted <- lmer(do ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template), data = x)
      metrics <- broom.mixed::glance(fitted)
    })
  )

p1 <- fits_pp %>% select(-data) %>% unnest(fit) %>% inner_join(code2haap) %>%
  ggplot(aes(code_id, logLik, color = haap_po_theme)) +
  geom_point() +
  theme(legend.position = "none")

p2 <- fits_do %>% select(-data) %>% unnest(fit) %>% inner_join(code2haap) %>%
  ggplot(aes(code_id, logLik, color = haap_do_recipient)) +
  geom_point()+
  theme(legend.position = "none")

