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

read_givenness_adaptation <- function(template) {
  
  theme_given <- glue("data/experiments/single_stimuli_dative_simulation_valtest_vbd_discourse_theme_given_template_{template}/adaptation.jsonl")
  recipient_given <- glue("data/experiments/single_stimuli_dative_simulation_valtest_vbd_discourse_recipient_given_template_{template}/adaptation.jsonl")
  
  adaptation_theme <- stream_in(file(theme_given)) %>% 
    as_tibble() %>% 
    mutate(template = template)
  adaptation_recipient <- stream_in(file(recipient_given)) %>% 
    as_tibble() %>% 
    mutate(template = template)
  
  bind_rows(adaptation_theme, adaptation_recipient)
}

mode = "_valtest_vbd_discourse_control"

adaptation_items <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation_valtest_vbd_neutral_discourse/adaptation.jsonl"))) %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(
    feature_config = glue::glue_collapse(str_sub(c(theme_pronominality, theme_animacy, theme_length, theme_definiteness, theme_markedness, recipient_pronominality, recipient_animacy, recipient_length, recipient_definiteness, recipient_markedness), 1, 1)),
    feature_config_small = glue::glue_collapse(str_sub(c(theme_pronominality, theme_animacy, theme_length, theme_definiteness, recipient_pronominality, recipient_animacy, recipient_length, recipient_definiteness), 1, 1)),
    unique_id = glue("{hypothesis_id}_{hypothesis_instance}")
  )

# adaptation_items %>%
#   filter(!theme %in% c("bear", "bear over there", "cat", "dog", "cat outside the house", "cup", "dog outside the house", "football", "book")) %>% 
#   filter(!recipient %in% c("bear", "bear over there", "cat", "dog", "cat outside the house", "cup", "dog outside the house", "football", "book", "cookies", "cheerios")) %>%
#   count(feature_config_small) %>% View()

set.seed(42)

sampled_items <- adaptation_items %>%
  filter(!theme %in% c("bear", "bear over there", "cat", "dog", "cat outside the house", "cup", "dog outside the house", "football", "book")) %>% 
  filter(!recipient %in% c("bear", "bear over there", "cat", "dog", "cat outside the house", "cup", "dog outside the house", "football", "book", "cookies", "cheerios")) %>%
  # filter(feature_config_small == "nisinisi") %>%
  # count(agent)
  filter(dative == "do") %>%
  group_by(feature_config_small, hypothesis_instance) %>%
  nest() %>%
  mutate(
    sampled = map(data, function(x){
      x %>%
        sample_n(1)
    })
  )

unique_ids <- sampled_items %>%
  select(-data) %>%
  unnest(sampled) %>%
  pull(unique_id)

# adaptation_items %>%
#   filter(unique_id %in% unique_ids) %>%
#   count(theme_pronominality, recipient_pronominality)

# filter(!theme %in% c("bear", "bear over there", "cat", "dog", "cat outside the house", "cup", "dog outside the house", "football", "book")) %>% 
# filter(!recipient %in% c("bear", "bear over there", "cat", "dog", "cat outside the house", "cup", "dog outside the house", "football", "book"))
# bad <- c("nalimnisdu", "nasdmnisdu", "")


adaptation_features <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation{mode}/adaptation.jsonl"))) %>%
  as_tibble() %>%
  select(item_id = item, hypothesis_id, hypothesis_instance, adaptation_dative = dative, theme_pronominality, theme_animacy, theme_length,
         theme_definiteness, theme_markedness, recipient_pronominality, recipient_animacy, recipient_length,
         recipient_definiteness, recipient_markedness) %>%
  mutate(given = NA)

adaptation_givenness_features <- read_givenness_adaptation("1") %>%
  select(item_id = item, hypothesis_id, hypothesis_instance, adaptation_dative = dative, theme_pronominality, theme_animacy, theme_length,
         theme_definiteness, theme_markedness, recipient_pronominality, recipient_animacy, recipient_length,
         recipient_definiteness, recipient_markedness, given)

results <- dir_ls("data/paper-results/cross-dative/", regexp = "*.csv") %>%
  map_df(read_csv) %>%
  mutate(
    context = str_replace_all(context, "-", " "),
    context = str_to_title(context),
    unique_id = glue("{hypothesis_id}_{hypothesis_instance}")
  ) 
# %>%
  # filter(unique_id %in% unique_ids)

# verbhood 

results %>%
  mutate(
    context = case_when(
      str_detect(context, "(Theme|Recipient)") ~ "Theme/Recipient Given",
      TRUE ~ context
    ),
    context = factor(
      context, 
      levels = c("No Context", "Neutral Context", "Only Agent Given", "Theme/Recipient Given"),
      labels = c("No Context", "Neutral\nContext", "Only Agent\nGiven", "Theme/Recipient\nGiven")
    )
  ) %>%
  filter(context %in% c("No Context", "Theme/Recipient\nGiven")) %>%
  filter(adaptation_dative != generalization_dative) %>%
  group_by(context, seed, adaptation_dative) %>%
  summarize(
    verbhood = mean(val_performance),
    accuracy = mean(val_performance > 0)
  ) %>%
  ungroup() %>%
  pivot_longer(verbhood:accuracy, names_to = "metric", values_to = "value") %>%
  group_by(context, adaptation_dative, metric) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(value),
    value = mean(value)
  ) %>%
  # group_by(context, adaptation_dative) %>%
  # summarize(
  #   n = n(),
  #   ste = 1.96 * plotrix::std.error(val_performance),
  #   verbhood = mean(val_performance),
  #   accuracy = mean(val_performance > 0),
  #   ste_accuracy = 1.96 * plotrix::std.error(val_performance > 0)
  # ) %>%
  mutate(
    # metric = str_to_title(metric),
    metric = case_when(
      metric == "accuracy" ~ "Verbhood Accuracy",
      metric == "verbhood" ~ "Verbhood Diff"
    ),
    metric_f = factor(metric, levels=c("Verbhood Accuracy", "Verbhood Diff"), labels = c("Verbhood Accuracy", "Verbhood &Delta;")),
    adaptation_dative = str_to_upper(adaptation_dative),
  ) %>%
  ggplot(aes(context, value, color = adaptation_dative, shape = adaptation_dative)) +
  # stat_gradientinterval(position = position_dodge(0.9)) +
  geom_point(size = 3, position = position_dodge(0.9)) +
  geom_linerange(aes(ymin = value - ste, ymax = value + ste), position = position_dodge(0.9)) +
  geom_hline(
    aes(yintercept = y), 
    data = tibble(metric = c("Verbhood Accuracy"), metric_f = c("Verbhood Accuracy"), y = c(0.5)),
    linetype = "dashed"
  ) +
  geom_hline(
    aes(yintercept = y), 
    data = tibble(metric = c("Verbhood Diff"), metric_f = c("Verbhood &Delta;"), y = c(0.0)),
    linetype = "dashed"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("#e08214", "#8073ac"), aesthetics = c("color", "fill")) +
  facet_wrap(~ metric_f) +
  labs(
    x = "Context Configuration",
    y = "Value (95% CI)",
    color = "Adaptation Dative",
    shape = "Adaptation Dative"
  ) +
  theme(
    strip.text = element_markdown()
  )


## Givenness

results %>%
  filter(unique_id %in% unique_ids) %>%
  mutate(
    context = case_when(
      str_detect(context, "(Theme|Recipient)") ~ "Theme/Recipient Given",
      TRUE ~ context
    ),
    context = factor(
      context, 
      levels = c("No Context", "Neutral Context", "Only Agent Given", "Theme/Recipient Given"),
      labels = c("No Context", "Neutral\nContext", "Only Agent\nGiven", "Theme/Recipient\nGiven")
    )
  ) %>%
  filter(context %in% c("Theme/Recipient\nGiven")) %>%
  filter(adaptation_dative != generalization_dative) %>%
  group_by(context, seed, adaptation_dative) %>%
  summarize(
    verbhood = mean(val_performance),
    accuracy = mean(val_performance > 0)
  ) %>%
  ungroup() %>%
  pivot_longer(verbhood:accuracy, names_to = "metric", values_to = "value") %>%
  group_by(context, adaptation_dative, metric) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(value),
    value = mean(value)
  ) %>%
  # group_by(context, adaptation_dative) %>%
  # summarize(
  #   n = n(),
  #   ste = 1.96 * plotrix::std.error(val_performance),
  #   verbhood = mean(val_performance),
  #   accuracy = mean(val_performance > 0),
  #   ste_accuracy = 1.96 * plotrix::std.error(val_performance > 0)
  # ) %>%
  mutate(
    # metric = str_to_title(metric),
    metric = case_when(
      metric == "accuracy" ~ "Verbhood Accuracy",
      metric == "verbhood" ~ "Verbhood Diff"
    ),
    metric_f = factor(metric, levels=c("Verbhood Accuracy", "Verbhood Diff"), labels = c("Verbhood Accuracy", "Verbhood &Delta;")),
    adaptation_dative = str_to_upper(adaptation_dative),
  ) %>%
  ggplot(aes(adaptation_dative, value, color = adaptation_dative, shape = adaptation_dative)) +
  # stat_gradientinterval(position = position_dodge(0.9)) +
  geom_point(size = 3, position = position_dodge(0.9), show.legend = FALSE) +
  geom_errorbar(aes(ymin = value - ste, ymax = value + ste), position = position_dodge(0.9), width = 0.1, show.legend = FALSE) +
  geom_hline(
    aes(yintercept = y), 
    data = tibble(metric = c("Verbhood Accuracy"), metric_f = c("Verbhood Accuracy"), y = c(0.5)),
    linetype = "dashed"
  ) +
  geom_hline(
    aes(yintercept = y), 
    data = tibble(metric = c("Verbhood Diff"), metric_f = c("Verbhood &Delta;"), y = c(0.0)),
    linetype = "dashed"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("#e08214", "#8073ac"), aesthetics = c("color", "fill")) +
  facet_wrap(~ metric_f) +
  labs(
    x = "Exposure Dative",
    y = "Value (95% CI)",
    color = "Adaptation Dative",
    shape = "Adaptation Dative"
  ) +
  theme(
    strip.text = element_markdown()
  )

ggsave("paper/verbhood.pdf", width = 8.61, height = 4.03, dpi = 300, device = cairo_pdf)
# ggsave("paper/verbhood.pdf", width = 9.24, height = 4.43, dpi = 300, device = cairo_pdf)
# ggsave("paper/verbhood.pdf", width = 11.77, height = 5.14, dpi = 300, device=cairo_pdf)

# Conwell and Demuth

results %>%
  filter(unique_id %in% unique_ids) %>%
  mutate(
    context = case_when(
      str_detect(context, "(Theme|Recipient)") ~ "Theme/Recipient Given",
      TRUE ~ context
    ),
    context = factor(context, levels = c("No Context", "Neutral Context", "Only Agent Given", "Theme/Recipient Given"))
  ) %>%
  filter(context %in% c("No Context", "Theme/Recipient Given")) %>%
  filter(adaptation_dative != generalization_dative) %>%
  pivot_longer(balanced_logprob:real_logprob, names_to = "genset", values_to = "logprob") %>%
  mutate(genset = str_remove(genset, "_logprob") %>% str_to_title()) %>%
  mutate(genset = factor(genset, levels = c("Real", "Balanced"), labels = c("Natural", "Synthetic"))) %>%
  # group_by(context, seed, adaptation_dative, generalization_dative, genset) %>%
  group_by(context, adaptation_dative, generalization_dative, genset) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    # experiment = glue::glue("<span>{adaptation_dative} &#8594; {generalization_dative}</span>")
    experiment = glue::glue("{adaptation_dative} to {generalization_dative}")
  ) %>%
  # ggplot(aes(experiment, logprob, color = genset, shape = genset, group = interaction(seed, genset))) +
  ggplot(aes(experiment, logprob, color = genset, shape = genset)) +
  geom_point(size = 3) +
  # geom_line() +
  geom_linerange(aes(ymin = logprob-ste, ymax = logprob+ste)) +
  # facet_grid(genset ~ context)
  facet_wrap(~ context, nrow = 1, scales = "free_y") +
  # scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_y_continuous(breaks = seq(-5.8, -4.2, by = 0.1)) +
  # scale_y_continuous(limits = c(-5.8, -4.4), breaks = scales::pretty_breaks()) +
  # ggh4x::facet_grid2(genset ~ context, scales = "free", independent = "y") +
  # scale_color_brewer(palette = "BrBg") +
  # scale_color_manual(values = c("#e9a3c9", "#a1d76a")) +
  scale_color_manual(values = c("#4793AF", "#FFC470")) +
  labs(
    x = "Generalization Direction",
    y = "log <em>p</em>(<span style='font-size: 11pt;'>ALT-FORM</span>) per token",
    color = "Generalization Set",
    shape = "Generalization Set"
  ) +
  theme(
    axis.title.y = element_markdown(),
    axis.text.x = element_markdown()
  )

# only givenness

results %>%
  filter(unique_id %in% unique_ids) %>%
  mutate(
    context = case_when(
      str_detect(context, "(Theme|Recipient)") ~ "Theme/Recipient Given",
      TRUE ~ context
    ),
    context = factor(context, levels = c("No Context", "Neutral Context", "Only Agent Given", "Theme/Recipient Given"))
  ) %>%
  filter(context %in% c("Theme/Recipient Given")) %>%
  filter(adaptation_dative != generalization_dative) %>%
  pivot_longer(balanced_logprob:real_logprob, names_to = "genset", values_to = "logprob") %>%
  mutate(genset = str_remove(genset, "_logprob") %>% str_to_title()) %>%
  mutate(genset = factor(genset, levels = c("Real", "Balanced"), labels = c("Natural", "Synthetic"))) %>%
  group_by(context, adaptation_dative, generalization_dative, genset) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    experiment = glue::glue("{adaptation_dative} to {generalization_dative}")
  ) %>%
  ggplot(aes(experiment, logprob, color = genset, shape = genset)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = logprob-ste, ymax = logprob+ste), width = 0.1) +
  scale_y_continuous(breaks = seq(-5.8, -4.2, by = 0.2), limits = c(-5.8, -4.8)) +
  scale_color_manual(values = c("#4793AF", "#FFC470")) +
  labs(
    x = "Generalization Direction",
    y = "log <em>p</em>(<span style='font-size: 11pt;'>ALT-FORM</span>) per token",
    color = "Generalization Set",
    shape = "Generalization Set"
  ) +
  theme(
    axis.title.y = element_markdown(),
    axis.text.x = element_markdown()
  )

ggsave("paper/conwell-demuth.pdf", width = 5.73, height = 5.03, dpi = 300, device = cairo_pdf)
# ggsave("paper/conwell-demuth.pdf", width = 8.84, height = 4.51, dpi = 300, device = cairo_pdf)
# ggsave("paper/conwell-demuth.pdf", width = 12.84, height = 4, dpi = 300, device=cairo_pdf)


cd <- results %>%
  filter(adaptation_dative != generalization_dative) %>%
  filter(unique_id %in% unique_ids) %>%
  filter(context %in% c("Theme Given", "Recipient Given")) %>%
  pivot_longer(balanced_logprob:real_logprob, names_to = "genset", values_to = "logprob") %>%
  mutate(genset = str_remove(genset, "_logprob") %>% str_to_title()) %>%
  mutate(
    experiment = case_when(
      adaptation_dative == "do" ~ 1,
      TRUE ~ 0
    ),
    genset = case_when(
      genset == "Balanced" ~ 1,
      TRUE ~ 0
    ),
    model = factor(model)
  )

fit_cd_synthetic <- lmer(logprob ~ experiment + (1 | model), data = cd %>% filter(genset == 1))
fit_cd_natural <- lmer(logprob ~ experiment + (1 | model), data = cd %>% filter(genset == 0))

summary(fit_cd_synthetic)
summary(fit_cd_natural)

fit_cd_givenness <- lmer(
  logprob ~ experiment * genset + (1 | model),
  data = cd
)

summary(fit_cd_givenness)
emmeans(fit_cd_givenness, pairwise ~ experiment | genset, adjust = "bonferroni")
tidy(fit_cd_givenness)

fit_cd_agent_given <- lmer(
  logprob ~ experiment * genset + (1 + experiment * genset | model),
  data = cd %>% 
    filter(context %in% c("Only Agent Given"))
)

summary(fit_cd_agent_given)
emmeans(fit_cd_agent_given, pairwise ~ experiment | genset, adjust = "bonferroni")


fit_cd_neutral <- lmer(
  logprob ~ experiment * genset + (1 + experiment * genset | model),
  data = cd %>% 
    filter(context %in% c("Neutral Context"))
)

summary(fit_cd_neutral)
emmeans(fit_cd_neutral, pairwise ~ experiment | genset, adjust = "bonferroni")

fit_cd_nocontext <- lmer(
  logprob ~ experiment * genset + (1 + experiment * genset | model),
  data = cd %>% 
    filter(context %in% c("No Context"))
)

summary(fit_cd_nocontext)
emmeans(fit_cd_nocontext, pairwise ~ experiment | genset, adjust = "bonferroni")




# diff way

cd_sep <- results %>%
  filter(adaptation_dative != generalization_dative) %>%
  mutate(
    experiment = case_when(
      adaptation_dative == "do" ~ 1,
      TRUE ~ 0
    ),
    model = factor(model)
  )

fit_cd_balanced <- lmer(
  balanced_logprob ~ experiment + (1 + experiment | model),
  data = cd_sep %>%
    # filter(context %in% c("Theme Given", "Recipient Given"))
    filter(context %in% c("Only Agent Given"))
)

summary(fit_cd_balanced)

# arunachalam plots # inanimate theme + animate recipients with definite arguments

feature_results <- bind_rows(
  results %>%
    filter(!context %in% c("Theme Given", "Recipient Given")) %>%
    inner_join(adaptation_features, by = c("item_id", "hypothesis_id", "hypothesis_instance", "adaptation_dative")) %>%
    mutate(given = "none"),
  results %>%
    filter(context %in% c("Theme Given", "Recipient Given")) %>%
    mutate(
      given = case_when(
        context == "Theme Given" ~ "theme",
        TRUE ~ "recipient"
      )
    ) %>%
    inner_join(adaptation_givenness_features, by = c("item_id", "hypothesis_id", "hypothesis_instance", "adaptation_dative", "given"))
)

sudha <- feature_results %>%
  filter(recipient_definiteness == "definite", theme_definiteness == "definite") %>%
  filter(recipient_pronominality != "pronoun", theme_pronominality != "pronoun") %>%
  filter(recipient_length == "short", theme_length == "short")

sudha_items <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation_valtest_vbd_discourse_control/adaptation.jsonl"))) %>%
  as_tibble() %>%
  filter(recipient_definiteness == "definite", theme_definiteness == "definite") %>%
  filter(recipient_pronominality != "pronoun", theme_pronominality != "pronoun") %>%
  filter(recipient_length == "short", theme_length == "short") %>%
  filter(recipient_animacy == "animate") %>% 
  filter(str_detect(theme, "(the|a )") & str_detect(recipient, "(the|a )")) %>%
  distinct(item) %>%
  pull(item)
  


sudha %>% 
  filter(generalization_dative == "do", recipient_animacy == "animate", theme_animacy == "inanimate") %>%
  # filter(str_detect(theme, "the") & str_detect(recipient, "the")) %>%
  filter(item_id %in% sudha_items) %>%
  mutate(
    context = case_when(
      str_detect(context, "(Theme|Recipient)") ~ "Theme/Recipient Given",
      TRUE ~ context
    ),
    context = factor(context, levels = c("No Context", "Neutral Context", "Only Agent Given", "Theme/Recipient Given"))
  ) %>%
  pivot_longer(balanced_logprob:real_logprob, names_to = "genset", values_to = "logprob") %>%
  mutate(genset = str_remove(genset, "_logprob") %>% str_to_title()) %>%
  # group_by(context, seed, adaptation_dative, generalization_dative, genset) %>%
  group_by(context, adaptation_dative, generalization_dative, genset) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    # experiment = glue::glue("<span>{adaptation_dative} &#8594; {generalization_dative}</span>")
    experiment = glue::glue("{adaptation_dative} to {generalization_dative}")
  ) %>%
  filter(genset == "Real") %>%
  # ggplot(aes(experiment, logprob, color = genset, shape = genset, group = interaction(seed, genset))) +
  ggplot(aes(adaptation_dative, logprob, color = genset, shape = genset)) +
  geom_point(size = 3) +
  # geom_line() +
  geom_linerange(aes(ymin = logprob-ste, ymax = logprob+ste)) +
  # facet_grid(genset ~ context)
  facet_wrap(~ context, nrow = 1, scales = "free_y") +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  # ggh4x::facet_grid2(genset ~ context, scales = "free", independent = "y") +
  # scale_color_brewer(palette = "BrBg") +
  # scale_color_manual(values = c("#e9a3c9", "#a1d76a")) +
  # scale_color_manual(values = c("#4793AF", "#FFC470")) +
  scale_color_manual(values = c("#4793AF")) +
  labs(
    x = "Exposure",
    y = "log <em>p</em>(<span style='font-size: 11pt;'>DO</span>-usage)",
    color = "Generalization Set",
    shape = "Generalization Set"
  ) +
  theme(
    axis.title.y = element_markdown(),
    axis.text.x = element_markdown()
  ) +
  guides(color = "none", shape = "none")

# ggsave("paper/arunachalam-do.pdf", width = 12.4, height = 3, dpi = 300, device=cairo_pdf)

# lmers!
sudha_do_pp <- sudha %>% 
  filter(generalization_dative == "do", recipient_animacy == "animate", theme_animacy == "inanimate") %>%
  mutate(
    adaptation_dative = case_when(
      adaptation_dative == "pp" ~ 1,
      TRUE ~ 0
    ),
    model = factor(model)
  )

fit_do_vs_pp_givenness <- lmer(
  real_logprob ~ adaptation_dative + (1 + adaptation_dative | model),
  data = sudha_do_pp %>%
    filter(context %in% c("Theme Given", "Recipient Given"))
)

summary(fit_do_vs_pp_givenness, correlation=FALSE)

fit_do_vs_pp_agent <- lmer(
  real_logprob ~ adaptation_dative + (1 + adaptation_dative | model),
  data = sudha_do_pp %>%
    filter(context %in% c("Only Agent Given"))
)

summary(fit_do_vs_pp_agent, correlation=FALSE)

fit_do_vs_pp_neutral <- lmer(
  real_logprob ~ adaptation_dative + (1 + adaptation_dative | model),
  data = sudha_do_pp %>%
    filter(context %in% c("Neutral Context"))
)

summary(fit_do_vs_pp_neutral, correlation=FALSE)

fit_do_vs_pp_nocontext <- lmer(
  real_logprob ~ adaptation_dative + (1 + adaptation_dative | model),
  data = sudha_do_pp %>%
    filter(context %in% c("No Context"))
)

summary(fit_do_vs_pp_nocontext, correlation=FALSE)


# OLD PLOT
sudha %>%
  # filter(adaptation_dative != generalization_dative) %>%
  mutate(
    context = case_when(
      str_detect(context, "(Theme|Recipient)") ~ "Theme/Recipient Given",
      TRUE ~ context
    ),
    context = factor(context, levels = c("No Context", "Neutral Context", "Only Agent Given", "Theme/Recipient Given"))
  ) %>%
  # filter(recipient_animacy == "animate") %>%
  mutate(
    # recipient_animacy = str_remove(recipient_animacy, "mate"),
    # theme_animacy = str_remove(theme_animacy, "mate"),
    recipient_animacy = str_to_title(recipient_animacy),
    theme_animacy = str_to_title(theme_animacy),
    # config2 = glue("{recipient_animacy}-R\n{theme_animacy}-T"),
    config1 = glue("{theme_animacy}"),
    config2 = glue("{recipient_animacy}")
  ) %>%
  group_by(context, config1, config2, adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(real_logprob),
    logprob = mean(real_logprob)
  ) %>%
  ungroup() %>%
  # filter(context %in% c("theme-given", "recipient-given")) %>%
  # filter(context == "only-agent-given") %>%
  # filter(context == "neutral-context") %>%
  # filter(context == "no-context") %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    exposure = glue::glue("Exposure = {str_to_upper(adaptation_dative)}"),
    experiment = glue::glue("{adaptation_dative} to {generalization_dative}")
  ) %>%
  filter(adaptation_dative != "PP" | generalization_dative != "PP") %>%
  ggplot(aes(config2, logprob, color = config1, shape = config1)) +
  geom_point(size = 3, position = position_dodge(0.9)) +
  geom_linerange(aes(ymin = logprob-ste, ymax=logprob+ste), position = position_dodge(0.9)) +
  # facet_grid(config1~exposure, scales = "free") +
  # facet_wrap(~exposure, scales = "free") +
  # scale_y_continuous(breaks = scales::pretty_breaks(4)) +
  scale_color_brewer(palette = "Dark2") +
  # ggh4x::facet_grid2(exposure ~ context, scales = "free", independent = "y") +
  ggh4x::facet_grid2(experiment ~ context, scales = "free", independent = "y") +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_markdown()
  ) +
  labs(
    x = "Recipient Animacy",
    y = "log <em>p</em>(<span style='font-size: 12pt;'>Generalization Set</span>)",
    # y = "log <em>p</em>(Generalization-Set)",
    color = "Theme Animacy",
    shape = "Theme Animacy"
  )


# NEW PLOT

sudha %>%
  # filter(adaptation_dative != generalization_dative) %>%
  mutate(
    context = case_when(
      str_detect(context, "(Theme|Recipient)") ~ "Theme/Recipient Given",
      TRUE ~ context
    ),
    context = factor(context, levels = c("No Context", "Neutral Context", "Only Agent Given", "Theme/Recipient Given"))
  ) %>%
  filter(recipient_animacy == "animate") %>%
  mutate(
    recipient_animacy = str_to_title(recipient_animacy),
    theme_animacy = str_to_title(theme_animacy),
    config1 = glue("{theme_animacy}"),
    config2 = glue("{recipient_animacy}")
  ) %>%
  group_by(context, config1, config2, adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(real_logprob),
    logprob = mean(real_logprob)
  ) %>%
  ungroup() %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    exposure = glue::glue("Exposure = {str_to_upper(adaptation_dative)}"),
    experiment = glue::glue("{adaptation_dative} to {generalization_dative}")
  ) %>%
  filter(adaptation_dative != "PP" | generalization_dative != "PP") %>%
  ggplot(aes(config1, logprob, color = config1, shape = config1)) +
  geom_point(size = 3, position = position_dodge(0.9)) +
  geom_linerange(aes(ymin = logprob-ste, ymax=logprob+ste), position = position_dodge(0.9)) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  ggh4x::facet_grid2(experiment ~ context, scales = "free", independent = "y") +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_markdown()
  ) +
  labs(
    x = "Theme Animacy",
    y = "log <em>p</em>(<span style='font-size: 12pt;'>Generalization Set</span>)",
    # y = "log <em>p</em>(Generalization-Set)",
  ) +
  guides(color = "none", shape = "none")

ggsave("paper/arunachalam-animacy.pdf", width = 11.87, height = 6.84, dpi = 300, device = cairo_pdf)

# 14.32, 6.07
# ggsave("paper/arunachalam-animacy.pdf", width = 14.32, height = 6.07, dpi = 300, device = cairo_pdf)

# all lmers...??

sudha_coded <- sudha %>%
  mutate(
    logprob = real_logprob,
    recipient_animacy = case_when(
      recipient_animacy == "animate" ~ 0.5,
      TRUE ~ -0.5
    ),
    theme_animacy = case_when(
      theme_animacy == "animate" ~ 0.5,
      TRUE ~ -0.5
    ),
    model = factor(model),
    item_id = factor(item_id),
    hypothesis_id = factor(hypothesis_id)
  )

sudha_coded <- sudha %>%
  mutate(
    logprob = real_logprob,
    recipient_animacy = case_when(
      recipient_animacy == "animate" ~ 1,
      TRUE ~ 0
    ),
    theme_animacy = case_when(
      theme_animacy == "animate" ~ 1,
      TRUE ~ 0
    ),
    model = factor(model),
    item_id = factor(item_id),
    hypothesis_id = factor(hypothesis_id)
  )

# lmers

tidy_all <- function(obj) {
  tidy(obj) %>%
    filter(term == "theme_animacy") %>%
    mutate(
      stars = case_when(
        p.value <= 0.001 ~ "***",
        p.value <= 0.01  ~ "**",
        p.value <= 0.05 ~ "*",
        TRUE ~ ""
      ),
      statistic = round(statistic, 3),
      estimate = round(estimate, 3), 
      std.error = round(std.error, 3),
      statistic = glue("{statistic}{stars}")
    ) %>%
    select(estimate, std.error, statistic)
}

fit_sudha_givenness_do_do <- lmer(
  real_logprob ~ theme_animacy + (1 + theme_animacy | model),
  data = sudha_coded %>% 
    filter(recipient_animacy == 1) %>%
    filter(context %in% c("Theme Given", "Recipient Given")) %>%
    filter(adaptation_dative == "do", generalization_dative == "do")
)

summary(fit_sudha_givenness_do_do, correlation = FALSE)
# broom.mixed::tidy(fit_sudha_givenness_do_do, conf.int=TRUE)
# emmeans(fit_sudha_givenness_do_do, pairwise ~ theme_animacy | recipient_animacy, adjust = "bonferroni")

fit_sudha_givenness_do <- lmer(
  real_logprob ~ theme_animacy + (1 + theme_animacy | model),
  data = sudha_coded %>% 
    filter(recipient_animacy == 1) %>%
    filter(context %in% c("Theme Given", "Recipient Given")) %>%
    filter(adaptation_dative == "do", generalization_dative == "pp")
)

summary(fit_sudha_givenness_do, correlation = FALSE)
# emmeans(fit_sudha_givenness_do, pairwise ~ theme_animacy | recipient_animacy, adjust = "bonferroni")

fit_sudha_givenness_pp <- lmer(
  real_logprob ~ theme_animacy + (1 + theme_animacy | model),
  data = sudha_coded %>% 
    filter(recipient_animacy == 1) %>%
    filter(context %in% c("Theme Given", "Recipient Given")) %>%
    filter(adaptation_dative == "pp", generalization_dative == "do")
)

summary(fit_sudha_givenness_pp, correlation = FALSE)
# emmeans(fit_sudha_givenness_pp, pairwise ~ theme_animacy | recipient_animacy, adjust = "bonferroni")

bind_rows(
  tidy_all(fit_sudha_givenness_do_do),
  tidy_all(fit_sudha_givenness_do),
  tidy_all(fit_sudha_givenness_pp)
)


# agent given

fit_sudha_agent_given_do_do <- lmer(
  real_logprob ~ theme_animacy + (1 + theme_animacy | model),
  data = sudha_coded %>% 
    filter(recipient_animacy == 1) %>%
    filter(context %in% c("Only Agent Given")) %>%
    filter(adaptation_dative == "do", generalization_dative == "do")
)

summary(fit_sudha_agent_given_do_do, correlation = FALSE)
# emmeans(fit_sudha_agent_given_do_do, pairwise ~ theme_animacy | recipient_animacy, adjust = "bonferroni")

fit_sudha_agent_given_do <- lmer(
  real_logprob ~ theme_animacy + (1 + theme_animacy | model),
  data = sudha_coded %>% 
    filter(recipient_animacy == 1) %>%
    filter(context %in% c("Only Agent Given")) %>%
    filter(adaptation_dative == "do", generalization_dative == "pp")
)

summary(fit_sudha_agent_given_do, correlation = FALSE)
# emmeans(fit_sudha_agent_given_do, pairwise ~ theme_animacy | recipient_animacy, adjust = "bonferroni")

fit_sudha_agent_given_pp <- lmer(
  real_logprob ~ theme_animacy + (1 + theme_animacy | model),
  data = sudha_coded %>% 
    filter(recipient_animacy == 1) %>%
    filter(context %in% c("Only Agent Given")) %>%
    filter(adaptation_dative == "pp", generalization_dative == "do")
)

summary(fit_sudha_agent_given_pp, correlation = FALSE)
# emmeans(fit_sudha_agent_given_pp, pairwise ~ theme_animacy | recipient_animacy, adjust = "bonferroni")

bind_rows(
  tidy_all(fit_sudha_agent_given_do_do),
  tidy_all(fit_sudha_agent_given_do),
  tidy_all(fit_sudha_agent_given_pp)
)

# NEUTRAL
fit_sudha_neutral_do_do <- lmer(
  real_logprob ~ theme_animacy + (1 + theme_animacy | model),
  data = sudha_coded %>% 
    filter(recipient_animacy == 1) %>%
    filter(context %in% c("Neutral Context")) %>%
    filter(adaptation_dative == "do", generalization_dative == "do")
)

summary(fit_sudha_neutral_do_do, correlation = FALSE)
# emmeans(fit_sudha_neutral_do_do, pairwise ~ theme_animacy | recipient_animacy, adjust = "bonferroni")

fit_sudha_neutral_do <- lmer(
  real_logprob ~ theme_animacy + (1 + theme_animacy | model),
  data = sudha_coded %>% 
    filter(recipient_animacy == 1) %>%
    filter(context %in% c("Neutral Context")) %>%
    filter(adaptation_dative == "do", generalization_dative == "pp")
)

summary(fit_sudha_neutral_do, correlation = FALSE)
# emmeans(fit_sudha_neutral_do, pairwise ~ theme_animacy | recipient_animacy, adjust = "bonferroni")

fit_sudha_neutral_pp <- lmer(
  real_logprob ~ theme_animacy + (1 + theme_animacy | model),
  data = sudha_coded %>% 
    filter(recipient_animacy == 1) %>%
    filter(context %in% c("Neutral Context")) %>%
    filter(adaptation_dative == "pp")
)

summary(fit_sudha_neutral_pp, correlation = FALSE)
# emmeans(fit_sudha_neutral_pp, pairwise ~ theme_animacy | recipient_animacy, adjust = "bonferroni")

bind_rows(
  tidy_all(fit_sudha_neutral_do_do),
  tidy_all(fit_sudha_neutral_do),
  tidy_all(fit_sudha_neutral_pp)
)

# NO CONTEXT

# fails to converge with random slopes
fit_sudha_nocontext_do_do <- lmer(
  real_logprob ~ theme_animacy + (1 | model),
  data = sudha_coded %>% 
    filter(recipient_animacy == 1) %>%
    filter(context %in% c("No Context")) %>%
    filter(adaptation_dative == "do", generalization_dative == "do")
)

summary(fit_sudha_nocontext_do_do, correlation = FALSE)
# emmeans(fit_sudha_nocontext_do_do, pairwise ~ theme_animacy | recipient_animacy, adjust = "bonferroni")

fit_sudha_nocontext_do <- lmer(
  real_logprob ~ theme_animacy + (1 + theme_animacy | model),
  data = sudha_coded %>% 
    filter(recipient_animacy == 1) %>%
    filter(context %in% c("No Context")) %>%
    filter(adaptation_dative == "do", generalization_dative == "pp")
)

summary(fit_sudha_nocontext_do, correlation = FALSE)
# emmeans(fit_sudha_nocontext_do, pairwise ~ theme_animacy | recipient_animacy, adjust = "bonferroni")

fit_sudha_nocontext_pp <- lmer(
  real_logprob ~ theme_animacy + (1 + theme_animacy | model),
  data = sudha_coded %>% 
    filter(recipient_animacy == 1) %>%
    filter(context %in% c("No Context")) %>%
    filter(adaptation_dative == "pp")
)

summary(fit_sudha_nocontext_pp, correlation = FALSE)
# emmeans(fit_sudha_nocontext_pp, pairwise ~ theme_animacy | recipient_animacy, adjust = "bonferroni")


bind_rows(
  tidy_all(fit_sudha_nocontext_do_do),
  tidy_all(fit_sudha_nocontext_do),
  tidy_all(fit_sudha_nocontext_pp)
)
# all fixed effects


coded_results <- feature_results %>%
  mutate(
    recipient_pronominality = case_when(
      recipient_pronominality == "pronoun" ~ 0.5,
      TRUE ~ -0.5
    ),
    theme_pronominality = case_when(
      theme_pronominality == "pronoun" ~ 0.5,
      TRUE ~ -0.5
    ),
    recipient_animacy = case_when(
      recipient_animacy == "animate" ~ 0.5,
      TRUE ~ -0.5
    ),
    theme_animacy = case_when(
      theme_animacy == "animate" ~ 0.5,
      TRUE ~ -0.5
    ),
    theme_definiteness = case_when(
      theme_definiteness == "definite" ~ 1,
      TRUE ~ 0
    ),
    recipient_definiteness = case_when(
      recipient_definiteness == "definite" ~ 1,
      TRUE ~ 0
    ),
    theme_length = case_when(
      theme_length == "short" ~ 1,
      TRUE ~ 0
    ),
    recipient_length = case_when(
      recipient_length == "short" ~ 1,
      TRUE ~ 0
    ),
    theme_given = case_when(
      given == "theme" ~ 1,
      given == "recipient" ~ 0,
      TRUE ~ -1
    ),
    recipient_given = case_when(
      given == "recipient" ~ 1,
      TRUE ~ 0
    ),
    model = factor(model),
    item_id = factor(item_id),
    hypothesis_id = factor(hypothesis_id)
  )

do_do <- coded_results %>%
  # filter(context %in% c("Theme Given", "Recipient Given")) %>%
  filter(context %in% c("No Context")) %>%
  filter(adaptation_dative == "do", generalization_dative == "do")

do_pp <- coded_results %>%
  # filter(context %in% c("Theme Given", "Recipient Given")) %>%
  filter(context %in% c("No Context")) %>%
  filter(adaptation_dative == "do", generalization_dative == "pp")

pp_do <- coded_results %>%
  # filter(context %in% c("Theme Given", "Recipient Given")) %>%
  filter(context %in% c("No Context")) %>%
  filter(adaptation_dative == "pp", generalization_dative == "do")

# interactions between theme-animacy and recipient animacy; theme-pronominality and recipient-pronominality

# fit_tara_tprp_interaction_dodo <- lmer(real_logprob ~ 
#                                          theme_animacy + theme_pronominality +
#                                          recipient_animacy + recipient_pronominality +
#                                          theme_animacy:recipient_animacy +
#                                          theme_pronominality:recipient_pronominality +
#                                          theme_animacy:theme_pronominality +
#                                          recipient_animacy:recipient_pronominality +
#                                          theme_definiteness + recipient_definiteness +
#                                          theme_length + recipient_length +
#                                          (1|model) + (1|hypothesis_id), data = do_do)
# 
# summary(fit_tara_tprp_interaction_dodo, correlation = FALSE)
# emmip(fit_tara_tprp_interaction_dopp, theme_animacy ~ recipient_animacy + theme_pronominality, CIs = TRUE)
# emmeans(fit_tara_tprp_interaction_dopp, pairwise ~ theme_animacy | recipient_animacy + theme_pronominality)

fit_tara_tprp_interaction_dopp <- lmer(real_logprob ~ 
                                         theme_animacy + theme_pronominality +
                                         recipient_animacy + recipient_pronominality +
                                         theme_definiteness + recipient_definiteness +
                                         # theme_given + 
                                         # theme_given:theme_pronominality +
                                         # theme_given:recipient_pronominality +
                                         # theme_animacy:recipient_animacy +
                                         # theme_pronominality:recipient_pronominality +
                                         theme_animacy:theme_pronominality +
                                         recipient_animacy:recipient_pronominality +
                                         theme_length + recipient_length + (1 |model), data = do_pp)

summary(fit_tara_tprp_interaction_dopp, correlation = FALSE)
emmip(fit_tara_tprp_interaction_dopp, theme_animacy ~ recipient_animacy + theme_pronominality, CIs = TRUE)
emmeans(fit_tara_tprp_interaction_dopp, pairwise ~ theme_animacy | recipient_animacy + theme_pronominality)


# interactions between theme-animacy and recipient animacy; theme-pronominality and recipient-pronominality

fit_tara_tprp_interaction_ppdo <- lmer(real_logprob ~ 
                                         theme_animacy + theme_pronominality +
                                         recipient_animacy + recipient_pronominality +
                                         theme_definiteness + recipient_definiteness +
                                         # theme_given + 
                                         # theme_given:theme_pronominality +
                                         # theme_given:recipient_pronominality +
                                         theme_animacy:recipient_animacy +
                                         theme_pronominality:recipient_pronominality +
                                         theme_animacy:theme_pronominality +
                                         recipient_animacy:recipient_pronominality +
                                         theme_length + recipient_length + (1|model), data = pp_do)



summary(fit_tara_tprp_interaction_ppdo, correlation = FALSE)
emmip(fit_tara_tprp_interaction_ppdo, theme_pronominality ~ theme_animacy + recipient_pronominality, CIs = TRUE)
emmeans(fit_tara_tprp_interaction_dopp, pairwise ~ theme_pronominality | theme_animacy + recipient_pronominality)


do_pp <- coded_results %>%
  filter(context %in% c("Theme Given", "Recipient Given")) %>%
  # filter(context %in% c("No Context")) %>%
  filter(adaptation_dative == "do", generalization_dative == "pp")

pp_do <- coded_results %>%
  filter(context %in% c("Theme Given", "Recipient Given")) %>%
  # filter(context %in% c("No Context")) %>%
  filter(adaptation_dative == "pp", generalization_dative == "do")

fit_tara_tprp_interaction_given_dopp <- lmer(real_logprob ~ 
                                               theme_animacy + theme_pronominality +
                                               recipient_animacy + recipient_pronominality +
                                               theme_definiteness + recipient_definiteness +
                                               theme_given +
                                               # theme_given:theme_pronominality +
                                               # theme_given:recipient_pronominality +
                                               theme_animacy:recipient_animacy +
                                               theme_pronominality:recipient_pronominality +
                                               theme_animacy:theme_pronominality +
                                               recipient_animacy:recipient_pronominality +
                                               theme_length + recipient_length + (1 |model),  data = do_pp)

summary(fit_tara_tprp_interaction_given_dopp, correlation = FALSE)
emmip(fit_tara_tprp_interaction_given_dopp, theme_given ~ recipient_animacy + theme_animacy)
emmeans(fit_tara_tprp_interaction_given_dopp, pairwise ~ theme_given | recipient_animacy + theme_animacy)
emmip(fit_tara_tprp_interaction_dopp, theme_pronominality ~ theme_animacy + recipient_pronominality, CIs = TRUE)
emmeans(fit_tara_tprp_interaction_dopp, pairwise ~ theme_pronominality | theme_animacy + recipient_pronominality)


fit_tara_tprp_interaction_given_ppdo <- lmer(real_logprob ~ 
                                               theme_animacy + theme_pronominality +
                                               recipient_animacy + recipient_pronominality +
                                               theme_definiteness + recipient_definiteness +
                                               theme_given + 
                                               # theme_given:theme_pronominality +
                                               # theme_given:recipient_pronominality +
                                               theme_animacy:recipient_animacy +
                                               theme_pronominality:recipient_pronominality +
                                               theme_animacy:theme_pronominality +
                                               recipient_animacy:recipient_pronominality +
                                               theme_length + recipient_length + (1 |model), data = pp_do %>% mutate(theme_given = case_when(theme_given == 1 ~ 0.5, TRUE ~ -0.5)))

summary(fit_tara_tprp_interaction_given_ppdo, correlation = FALSE)

emmip(fit_tara_tprp_interaction_given_ppdo, theme_given ~ theme_pronominality + recipient_pronominality, CIs = TRUE)
# emmeans(fit_tara_tprp_interaction_given_ppdo, pairwise ~ theme_given | theme_pro)
emmip(fit_tara_tprp_interaction_given_ppdo, theme_given ~ recipient_animacy + theme_animacy)
emmeans(fit_tara_tprp_interaction_given_ppdo, pairwise ~ theme_given | recipient_animacy + theme_animacy)
emmip(fit_tara_tprp_interaction_ppdo, theme_pronominality ~ theme_animacy + recipient_pronominality, CIs = TRUE)
emmeans(fit_tara_tprp_interaction_ppdo, pairwise ~ theme_pronominality | theme_animacy + recipient_pronominality)
emmip(fit_tara_tprp_interaction_ppdo, theme_animacy ~ recipient_animacy, CIs = TRUE)


# conwell (2019)

feature_results %>%
  filter(unique_id %in% unique_ids) %>%
  filter(str_detect(context, "(Theme|Recipient)")) %>%
  group_by(adaptation_dative, generalization_dative, recipient_pronominality) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(real_logprob),
    logprob = mean(real_logprob)
  ) %>%
  ungroup() %>%
  filter(adaptation_dative != generalization_dative) %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    condition = glue("{adaptation_dative} to {generalization_dative}")
  ) %>%
  ggplot(aes(recipient_pronominality, logprob, color = recipient_pronominality, shape = recipient_pronominality)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = logprob - ste, ymax = logprob + ste), width = 0.1) +
  facet_wrap(~ condition, scales = "free")


feature_results %>%
  filter(unique_id %in% unique_ids) %>%
  filter(str_detect(context, "(Theme|Recipient)")) %>%
  group_by(adaptation_dative, generalization_dative, recipient_animacy) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(real_logprob),
    logprob = mean(real_logprob)
  ) %>%
  ungroup() %>%
  filter(adaptation_dative != generalization_dative) %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    condition = glue("{adaptation_dative} to {generalization_dative}")
  ) %>%
  ggplot(aes(recipient_pronominality, logprob, color = recipient_pronominality, shape = recipient_pronominality)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = logprob - ste, ymax = logprob + ste), width = 0.1) +
  facet_wrap(~ condition, scales = "free")
