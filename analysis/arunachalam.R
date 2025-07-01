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

read_arunachalam_adaptation <- function(template) {
  
  theme_recipient <- glue("data/experiments/single_stimuli_dative_simulation_valtest_vbd_arunachalam_template_tr_{template}/adaptation.jsonl")
  recipient_theme <- glue("data/experiments/single_stimuli_dative_simulation_valtest_vbd_arunachalam_template_rt_{template}/adaptation.jsonl")
  
  adaptation_theme_recipient <- stream_in(file(theme_recipient)) %>% 
    as_tibble() %>% 
    mutate(template = template) %>%
    mutate(order = "theme-recipient")
  adaptation_recipient_theme <- stream_in(file(recipient_theme)) %>% 
    as_tibble() %>% 
    mutate(template = template) %>%
    mutate(order = "recipient-theme")
  
  bind_rows(adaptation_theme_recipient, adaptation_recipient_theme)
}

read_arunachalam_results <- function(template) {
  theme_recipient <- glue("data/results/single_stimuli_dative_simulation_valtest_vbd_arunachalam_template_tr_{template}/")
  recipient_theme <- glue("data/results/single_stimuli_dative_simulation_valtest_vbd_arunachalam_template_rt_{template}/")
  
  results_theme_recipient <- dir_ls(theme_recipient, regexp = "*.csv", recurse = TRUE) %>%
    map_df(read_csv, .id = "model") %>%
    mutate(
      model = str_extract(model, glue("(?<={theme_recipient})(.*)(?=/best_lr_results_hypwise.csv)"))
    ) %>%
    mutate(
      seed = as.numeric(str_remove(model, "smolm-autoreg-bpe-seed_"))
    ) %>%
    mutate(template = template) %>%
    mutate(order = "theme-recipient") %>%
    filter(state == "best")
  
  results_recipient_theme <- dir_ls(recipient_theme, regexp = "*.csv", recurse = TRUE) %>%
    map_df(read_csv, .id = "model") %>%
    mutate(
      model = str_extract(model, glue("(?<={recipient_theme})(.*)(?=/best_lr_results_hypwise.csv)"))
    ) %>%
    mutate(
      seed = as.numeric(str_remove(model, "smolm-autoreg-bpe-seed_"))
    ) %>%
    mutate(template = template) %>%
    mutate(order = "recipient-theme") %>%
    filter(state == "best")
  
  bind_rows(results_theme_recipient, results_recipient_theme)
}

arunachalam_adaptation <- bind_rows(
  read_arunachalam_adaptation("1"),
  read_arunachalam_adaptation("2"),
  read_arunachalam_adaptation("3")
)

arunachalam_results_raw <- bind_rows(
  read_arunachalam_results("1"),
  read_arunachalam_results("2"),
  read_arunachalam_results("3")
) %>% 
  filter(state == "best")

arunachalam_results <- arunachalam_results_raw %>%
  # filter(val_performance > 0) %>%
  # inner_join(arunachalam_adaptation %>% rename(adaptation_dative = dative)) %>%
  group_by(model, item_id, hypothesis_id, hypothesis_instance, adaptation_dative, generalization_dative, order) %>%
  # summarize(
  #   logprob = mean(logprob)
  # ) %>%
  nest() %>%
  mutate(
    best = map(data, function(x) {
      x %>%
        filter(val_performance == max(val_performance)) %>%
        slice(1)
    })
  ) %>%
  select(-data) %>%
  unnest(best) %>%
  ungroup() %>%
  inner_join(arunachalam_adaptation %>% rename(adaptation_dative = dative)) %>%
  filter(val_performance > 0)
  # inner_join(arunachalam_adaptation %>% rename(adaptation_dative = dative, item_id = item) %>% distinct(item_id, hypothesis_id, hypothesis_instance, adaptation_dative, theme_animacy, recipient_animacy))

arunachalam_results %>%
  filter(recipient_animacy == "animate", theme_animacy == "inanimate") %>%
  group_by(adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  filter(generalization_dative == "do") %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    # experiment = glue::glue("<span>{adaptation_dative} &#8594; {generalization_dative}</span>")
    experiment = glue::glue("{adaptation_dative} to {generalization_dative}")
  ) %>%
  # ggplot(aes(experiment, logprob, color = genset, shape = genset, group = interaction(seed, genset))) +
  ggplot(aes(adaptation_dative, logprob)) +
  geom_point(size = 3, color = "#4793AF") +
  # geom_line() +
  geom_errorbar(aes(ymin = logprob-ste, ymax = logprob+ste), color = "#4793AF", width = 0.1) +
  # facet_grid(genset ~ context)
  # facet_wrap(~ order, nrow = 1, scales = "free_y") +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  # ggh4x::facet_grid2(genset ~ context, scales = "free", independent = "y") +
  # scale_color_brewer(palette = "BrBg") +
  # scale_color_manual(values = c("#e9a3c9", "#a1d76a")) +
  # scale_color_manual(values = c("#4793AF", "#FFC470")) +
  scale_color_manual(values = c("#4793AF")) +
  labs(
    x = "Exposure Dative",
    y = "log <em>p</em>(<span style='font-size: 11pt;'>DO</span>-usage) per token",
    color = "Generalization Set",
    shape = "Generalization Set"
  ) +
  theme(
    axis.title.y = element_markdown(),
    axis.text.x = element_markdown()
  ) +
  guides(color = "none", shape = "none")

ggsave("paper/arunachalam-do.pdf", width = 4.72, height = 3.56, dpi = 300, device=cairo_pdf)

arunachalam_results %>%
  filter(recipient_animacy == "animate", theme_animacy == "inanimate") %>%
  group_by(adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  filter(generalization_dative == "do" | (adaptation_dative == "pp" & generalization_dative == "pp")) %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    # experiment = glue::glue("<span>{adaptation_dative} &#8594; {generalization_dative}</span>")
    experiment = glue::glue("{adaptation_dative} to {generalization_dative}"),
    experiment = factor(experiment, levels = c("PP to PP", "DO to DO", "PP to DO"))
  ) %>%
  # ggplot(aes(experiment, logprob, color = genset, shape = genset, group = interaction(seed, genset))) +
  ggplot(aes(experiment, logprob)) +
  geom_point(size = 3, color = "#4793AF") +
  # geom_line() +
  geom_errorbar(aes(ymin = logprob-ste, ymax = logprob+ste), color = "#4793AF", width = 0.1) +
  # facet_grid(genset ~ context)
  # facet_wrap(~ order, nrow = 1, scales = "free_y") +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  # ggh4x::facet_grid2(genset ~ context, scales = "free", independent = "y") +
  # scale_color_brewer(palette = "BrBg") +
  # scale_color_manual(values = c("#e9a3c9", "#a1d76a")) +
  # scale_color_manual(values = c("#4793AF", "#FFC470")) +
  scale_color_manual(values = c("#4793AF")) +
  labs(
    x = "Generalization Direction",
    y = "log <em>p</em>(target) per token",
    color = "Generalization Set",
    shape = "Generalization Set"
  ) +
  theme(
    axis.title.y = element_markdown(),
    axis.text.x = element_markdown()
  ) +
  guides(color = "none", shape = "none")

ggsave("paper/arunachalam-do.pdf", width = 4.72, height = 3.56, dpi = 300, device=cairo_pdf)


arunachalam_results %>%
  filter(recipient_animacy == "animate") %>%
  # filter(hypothesis_instance <=5) %>%
  mutate(
    recipient_animacy = str_to_title(recipient_animacy),
    theme_animacy = str_to_title(theme_animacy),
    config1 = glue("{theme_animacy}"),
    config2 = glue("{recipient_animacy}")
  ) %>%
  group_by(config1, config2, adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    exposure = glue::glue("Exposure = {str_to_upper(adaptation_dative)}"),
    experiment = glue::glue("{adaptation_dative} to {generalization_dative}")
  ) %>%
  # filter(adaptation_dative != "PP" | generalization_dative != "PP") %>%
  filter(generalization_dative == "DO") %>%
  ggplot(aes(config1, logprob, color = config1, shape = config1)) +
  geom_point(size = 3, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = logprob-ste, ymax=logprob+ste), position = position_dodge(0.9), width = 0.1) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  # facet_wrap(~ experiment, scales = "free") +
  facet_wrap(~ experiment) +
  # ggh4x::facet_grid2(experiment ~ order, scales = "free", independent = "y") +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_markdown()
  ) +
  labs(
    x = "Theme Animacy",
    # y = "log <em>p</em>(<span style='font-size: 12pt;'>Generalization Set</span>)",
    y = "log <em>p</em>(<span style='font-size: 11pt;'>DO</span>-usage) per token",
    # y = "log <em>p</em>(Generalization-Set)",
  ) +
  guides(color = "none", shape = "none")

  # ggsave("paper/arunachalam-animacy.pdf", width=10.23, height=3.24, dpi=300, device=cairo_pdf)
ggsave("paper/arunachalam-animacy.pdf", width=8.61, height=4.14, dpi=300, device=cairo_pdf)


# arunachalam_fit <- lmer(logprob ~ theme_animacy * exposure + (1 | model), 
#                         data = arunachalam_results %>% 
#                           filter(generalization_dative == "do", recipient_animacy == "animate") %>%
#                           mutate(
#                             theme_animacy = case_when(
#                               theme_animacy == "inanimate" ~ -0.5,
#                               TRUE ~ 0.5
#                             ),
#                             exposure = case_when(
#                               adaptation_dative == "do" ~ 0.5,
#                               TRUE ~ -0.5
#                             ),
#                             model = factor(model)
#                           ))
# 
# summary(arunachalam_fit)
# 
# emmeans(arunachalam_fit, ~ exposure | theme_animacy) %>%
#   contrast(simple = "each", combine = TRUE, adjust = "bonferroni")
# 
# emmeans(arunachalam_fit, ~ exposure | theme_animacy) %>%
#   contrast(interaction = "pairwise", combine = TRUE, adjust = "bonferroni") %>%
#   as_tibble()
# 
# emmeans(arunachalam_fit, ~ theme_animacy | exposure) %>%
#   contrast(simple = "each", combine = TRUE, adjust = "bonferroni") %>%
#   as_tibble() %>%
#   mutate(
#     estimate = estimate * 2,
#     exposure = case_when(
#       exposure == 0.5 ~ "DO",
#       exposure == -0.5 ~ "PP",
#       TRUE ~ exposure
#     ),
#     theme_animacy = case_when(
#       theme_animacy == 0.5 ~ "animate",
#       theme_animacy == -0.5 ~ "inanimate",
#       TRUE ~ theme_animacy
#     ),
#   ) %>%
#   filter(!str_detect(contrast, "-")) %>%
#   mutate(
#     contrast = str_remove(contrast, "0.5"),
#     t.ratio = format(round(t.ratio, 3), nsmall = 3),
#     estimate = format(round(estimate, 3), nsmall = 3), 
#     SE = format(round(SE, 3), nsmall = 3),
#     p = case_when(
#       p.value <= 0.001 ~ "$<.001$",
#       TRUE ~ glue("${round(p.value, digits=3)}$")
#     )
#   ) %>%
#   rename(t = t.ratio) %>%
#   select(-df, -p.value) 
#   
# 
# 
# emmip(arunachalam_fit, exposure ~ theme_animacy, CIs = TRUE, plotit = FALSE) %>%
#   as_tibble() %>%
#   mutate(
#     exposure = case_when(
#       exposure == 0.5 ~ "DO",
#       TRUE ~ "PP"
#     ),
#     theme_animacy = case_when(
#       theme_animacy == 0.5 ~ "animate",
#       TRUE ~ "inanimate"
#     )
#   ) %>%
#   ggplot(aes())
# 
# emmeans(arunachalam_fit, pairwise ~ exposure + theme_animacy)

arunachalam_do_gen <- arunachalam_results %>% 
  filter(generalization_dative == "do", recipient_animacy == "animate") %>%
  mutate(
    theme_animacy = case_when(
      theme_animacy == "inanimate" ~ 1,
      TRUE ~ 0
    ),
    model = factor(model)
  )

fit_do_pp <- lmer(logprob ~ exposure + (1 + exposure | model), 
                  data = arunachalam_do_gen %>% 
                    filter(theme_animacy == 1) %>%
                    mutate(
                      exposure = case_when(
                        adaptation_dative == "pp" ~ 1,
                        TRUE ~ 0
                      )
                    )
                  )


summary(fit_do_pp)


fit_do_do <- lmer(logprob ~ theme_animacy + (1 + theme_animacy | model),
                  data = arunachalam_do_gen %>% filter(adaptation_dative == "do"))

summary(fit_do_do, correlation=FALSE)

fit_pp_do <- lmer(logprob ~ theme_animacy + (1 + theme_animacy | model),
                  data = arunachalam_do_gen %>% filter(adaptation_dative == "pp"))

summary(fit_pp_do, correlation=FALSE)

cross_dative_cross_animacy <- bind_rows(
  arunachalam_results %>% 
    filter(generalization_dative == "do", adaptation_dative == "pp", recipient_animacy == "animate", theme_animacy == "animate"),
  arunachalam_results %>% 
    filter(generalization_dative == "do", adaptation_dative == "do", recipient_animacy == "animate", theme_animacy == "inanimate")
) %>%
  mutate(
    exposure = case_when(
      adaptation_dative == "pp" ~ 1,
      TRUE ~ 0
    ),
    model = factor(model)
  )

fit_cross_dative_cross_animacy <- lmer(logprob ~ exposure + (1 | model), data = cross_dative_cross_animacy)

summary(fit_cross_dative_cross_animacy)
