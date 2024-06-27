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

theme_set(
  theme_bw(base_size = 17, base_family = "Times") +
    theme(
      legend.position = "top",
      axis.text = element_text(color="black"),
      panel.grid = element_blank()
    )
)

adaptation_spec <- stream_in(file("data/experiments/single_stimuli_dative_simulation_valtest_vbd_possession_specified/adaptation.jsonl")) %>% 
  as_tibble()

adaptation_unspec <- stream_in(file("data/experiments/single_stimuli_dative_simulation_valtest_vbd_possession_unspecified/adaptation.jsonl")) %>% 
  as_tibble()


adaptation <- bind_rows(
  adaptation_spec,
  adaptation_unspec
)

spec_results <- bind_rows(
  dir_ls("data/results/single_stimuli_dative_simulation_valtest_vbd_possession_specified/", regexp = "*.csv", recurse = TRUE) %>%
    map_df(read_csv, .id = "model") %>%
    mutate(
      model = str_extract(model, glue("(?<=possession_specified/)(.*)(?=/best_lr_results_hypwise.csv)"))
    ) %>%
    mutate(
      seed = as.numeric(str_remove(model, "smolm-autoreg-bpe-seed_")),
      specificity = "specified"
    ),
  dir_ls("data/results/single_stimuli_dative_simulation_valtest_vbd_possession_unspecified/", regexp = "*.csv", recurse = TRUE) %>%
    map_df(read_csv, .id = "model") %>%
    mutate(
      model = str_extract(model, glue("(?<=possession_unspecified/)(.*)(?=/best_lr_results_hypwise.csv)"))
    ) %>%
    mutate(
      seed = as.numeric(str_remove(model, "smolm-autoreg-bpe-seed_")),
      specificity = "unspecified"
    )
) %>%
  filter(state == "best") %>%
  inner_join(adaptation %>% rename(adaptation_dative = dative, item_id = item), by = c("item_id", "specificity", "adaptation_dative"))

spec_results %>%
  # filter(!theme %in% c("them")) %>%
  filter(generalization_dative == "do") %>%
  group_by(theme_animacy, specificity) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  mutate(
    theme_animacy = str_to_title(theme_animacy),
    specificity = str_to_title(specificity)
  ) %>%
  ggplot(aes(theme_animacy, logprob, color = specificity, shape = specificity)) +
  geom_point(position = position_dodge(0.9), size = 3) +
  geom_errorbar(aes(ymin = logprob-ste, ymax = logprob+ste), position = position_dodge(0.9), width = 0.1) +
  scale_y_continuous(limits = c(-5.3, -4.9)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Theme Animacy",
    y = "log <em>p</em>(<span style='font-size: 11pt;'>DO</span>-usage) per token",
    color = "Possession transfer",
    shape = "Possession transfer"
  ) +
  theme(
    axis.title.y = element_markdown(),
    axis.text.x = element_markdown()
  )

ggsave("paper/possession-transfer.pdf", width = 5.73, height = 5.03, dpi = 300, device = cairo_pdf)

spec_results_coded <- spec_results %>%
  filter(generalization_dative == "do") %>%
  mutate(
    seed = factor(seed),
    theme_animacy = case_when(
      theme_animacy == "animate" ~ 0.5,
      TRUE ~ -0.5
    ),
    specificity = case_when(
      specificity == "specified" ~ 0.5,
      TRUE ~ -0.5
    )
  )

fit_animacy_specificity <- lmer(logprob ~ theme_animacy * specificity + (1 | seed), data = spec_results_coded)

summary(fit_animacy_specificity)
anova(fit_animacy_specificity)

emmeans(fit_animacy_specificity, ~ theme_animacy | specificity) %>%
  contrast(simple = "each", combine = TRUE, adjust = "bonferroni")

emmip(fit_animacy_specificity, theme_animacy ~ specificity, CIs = TRUE)
