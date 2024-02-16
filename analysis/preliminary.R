library(tidyverse)
library(fs)
library(jsonlite)
library(lme4)
library(lmerTest)

adaptation <- stream_in(file("data/experiments/adaptation.jsonl"))

aggregated_results <- dir_ls("data/results/single_stimuli_dative_simulation/", recurse = TRUE, regexp = "*lr_results.csv") %>%
  map_df(read_csv) |>
  inner_join(adaptation)

aggregated_results |>
  count(best_lr)

aggregated_results  |>
  mutate(
    feature_config = glue::glue("{recipient_pronominality}-recipient\n{theme_pronominality}-theme"),
  ) %>%
  group_by(feature_config, adaptation_dative) %>%
  summarize(
    do_logprob = mean(best_do_mean),
    do_ste = 1.96 * plotrix::std.error(best_do_mean)
  ) %>%
  ungroup() %>%
  mutate(
    exposure = glue::glue("Exposure = {str_to_upper(adaptation_dative)}")
  ) %>%
  # filter(adaptation_dative == "pp") %>%
  ggplot(aes(feature_config, do_logprob)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = do_logprob-do_ste, ymax=do_logprob+do_ste)) +
  facet_wrap(~exposure) +
  theme_bw(base_size = 14, base_family = "Times") +
  labs(
    x = "Feature Configuration",
    y = "log P(DO)"
  )

pp_exposure <- aggregated_results |>
  filter(adaptation_dative == "pp")

fit1 <- lmer(best_do_mean ~ theme_pronominality * theme_animacy + 
                            recipient_animacy * recipient_pronominality + 
                            (1|lm) + (1|item), 
             data = pp_exposure, REML = FALSE)

summary(fit1)

anova(fit1)

aggregated_results %>%
  group_by(lm, adaptation_dative) %>%
  summarize(
    do = mean(best_do_mean),
    pp = mean(best_pp_mean)
  ) %>%
  pivot_longer(do:pp, names_to = "target", values_to = "logprob") %>%
  mutate(
    exp = glue::glue("{adaptation_dative} -> {target}")
  ) %>%
  ggplot(aes(exp, logprob, color = target, shape = adaptation_dative)) +
  geom_jitter(size = 2.5, alpha = 0.5) +
  theme_bw()
