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
# mode = ""

adaptation <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation{mode}/adaptation.jsonl"))) %>%
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
generalization <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation{mode}/generalization.jsonl"))) %>%
  as_tibble()


feature_configs <- adaptation %>% 
  distinct(theme_pronominality, theme_animacy, theme_length, theme_definiteness, theme_markedness, recipient_pronominality, recipient_animacy, recipient_length, recipient_definiteness, recipient_markedness, canonicality) %>%
  rowwise() %>%
  mutate(
    feature_config = glue::glue_collapse(str_sub(c(theme_pronominality, theme_animacy, theme_length, theme_definiteness, theme_markedness, recipient_pronominality, recipient_animacy, recipient_length, recipient_definiteness, recipient_markedness), 1, 1))
  )

adaptation %>%
  inner_join(feature_configs) %>% View()

results <- dir_ls(glue("data/results/single_stimuli_dative_simulation{mode}/"), recurse = TRUE, regexp = "*lr_results_hypwise.csv") %>%
  map_df(read_csv, .id = "model") %>%
  mutate(
    model = str_extract(model, glue("(?<=simulation{mode}/)(.*)(?=/best_lr_results_hypwise.csv)"))
  ) 

results <- results %>%
  inner_join(feature_configs %>% rename(adaptation_feature_config = feature_config))

results %>%
  inner_join(adaptation) %>% 
  group_by(model, agent) %>% summarize(n = n(), logprob = mean(logprob)) %>% View()

altform_generalization <- results %>%
  filter(state == "best", adaptation_dative != generalization_dative)

altform_generalization %>%
  # results %>%
  #   filter(state == "best") %>%
  group_by(adaptation_dative, generalization_dative, adaptation_feature_config) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  filter(adaptation_dative == "pp") %>%
  mutate(
    adaptation_feature_config = fct_reorder(adaptation_feature_config, logprob)
  ) %>%
  ggplot(aes(adaptation_feature_config, logprob, color = adaptation_dative, shape = generalization_dative)) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = logprob-ste, ymax = logprob+ste)) +
  # facet_wrap(~adaptation_dative, nrow = 2, scales = "free_x") +
  theme_bw(base_size = 15, base_family = "Times") +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

# adaptation %>% inner_join(feature_configs) %>% View("adaptationcodes")
  

results %>%
  pivot_wider(names_from = state, values_from = logprob) %>%
  mutate(diff = best - initial) %>%
  inner_join(adaptation %>% rename(adaptation_dative = dative)) %>%
  # filter(canonicality == "marked") %>%
  filter(!str_detect(model, "444|555|666|777|888|999|1709")) %>%
  group_by(adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    # ste = 1.96 * plotrix::std.error((best-initial)/initial),
    # logprob = mean((best-initial)/initial),
    ste = 1.96 * plotrix::std.error(best),
    logprob = mean(best)
  ) %>%
  ungroup() %>%
  filter(adaptation_dative != generalization_dative) %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    experiment = glue::glue("{adaptation_dative} --> {generalization_dative}")
  ) %>%
  ggplot(aes(experiment, logprob)) +
  # geom_boxplot()
  # geom_jitter(size = 2, width = 0.1)
  geom_point(size = 3, color = "steelblue") +
  geom_linerange(aes(ymin = logprob-ste, ymax=logprob+ste), color = "steelblue") +
  # facet_wrap(~model, scales="free") +
  labs(
    x = "Generalization Experiment",
    y = "Average Log P(construction)"
  ) +
  theme(
    axis.text.x = element_markdown(),
    panel.grid = element_blank()
  )

results %>%
  inner_join(adaptation %>% rename(adaptation_dative = dative)) %>%
  mutate(
    model = str_extract(model, "\\d{1,}")
  ) %>% 
  mutate(
    # feature_config = canonicality
    feature_config = glue::glue("{recipient_pronominality}-recipient\n{theme_pronominality}-theme"),
    # feature_config = glue::glue("{substr(recipient_animacy,1,1)}-r\n{substr(theme_animacy,1,1)}-t"),
    # feature_config = glue::glue("{recipient_animacy}-r\n{theme_animacy}-t"),
    # feature_config = glue::glue("{recipient_animacy}"),
    # feature_config = glue::glue("{theme_animacy}-theme"),
    # feature_config = glue::glue("{theme_pronominality}-theme")
    # feature_config = glue::glue("{recipient_pronominality}-recip")
    # feature_config = glue::glue("{recipient_animacy}\n{recipient_pronominality}\n{recipient_length}"),
    # feature_config = glue::glue("{theme_animacy}\n{theme_pronominality}\n{theme_length}"),
    # feature_config = glue::glue("{recipient_animacy}\n{recipient_pronominality}\n{theme_animacy}\n{theme_pronominality}"),
    # feature_config = glue::glue("{recipient_definiteness}-recip\n{theme_definiteness}-theme"),
    # feature_config = glue::glue("{recipient_definiteness}-recip"),
    # feature_config = glue::glue("{theme_definiteness}-theme"),
  ) %>%
  filter(state == "best") %>% 
  filter(!str_detect(model, "333|444|555|666|777|888|999|1709")) %>%
  # filter(!str_detect(sentence, "(dolly|teddy)")) %>%
  # filter(theme_length == "short", recipient_length == "short") %>%
  # inner_join(adaptation %>% rename(adaptation_dative = dative)) %>%
  # filter(str_detect(adaptation_feature_config, "p(.*)(.*)p(.*)(.*)")) %>%
  # filter(str_detect(adaptation_feature_config, "(.*)a(.*)(.*)a(.*)")) %>%
  # filter(theme_length == "short", recipient_length == "short") %>%
  # filter(theme_animacy == "animate", recipient_animacy == "animate") %>%
  # filter(canonicality == "nc-marked") %>%
  # filter(theme_markedness == "marked", recipient_markedness == "marked") %>%
  group_by(feature_config, adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(logprob),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    exposure = glue::glue("Exposure = {str_to_upper(adaptation_dative)}")
  ) %>%
  # filter(adaptation_dative == "pp", generalization_dative == "do") %>%
  filter(adaptation_dative != generalization_dative) %>%
  # filter(adaptation_dative == "DO") %>%
  ggplot(aes(feature_config, logprob, color = generalization_dative)) +
  geom_point(size = 3) +
  # geom_line(aes(group = model)) +
  geom_linerange(aes(ymin = logprob-ste, ymax=logprob+ste)) +
  facet_wrap(~exposure, nrow=1) +
  # scale_color_manual(values = c("#00BFC4")) +
  # facet_grid(exposure ~ model) +
  # theme_bw(base_size = 14, base_family = "Times") +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    x = "Feature Configuration",
    y = "log P(alt-form)",
    color = "Generalization Dative"
  )

results

best_results <- results %>%
  pivot_wider(names_from = state, values_from = logprob) %>%
  mutate(
    diff = best - initial
  ) %>%
  inner_join(adaptation %>% rename(adaptation_dative = dative))

fit1 <- lmer(diff ~ theme_animacy + recipient_animacy + theme_pronominality * recipient_pronominality + theme_definiteness + theme_length + recipient_definiteness + recipient_length + (1|model) + (1|item_id), data = best_results %>% filter(adaptation_dative == "pp", generalization_dative == "do"))

summary(fit1)

best_results %>%
  filter(recipient_animacy == "animate")

best_results %>%
  filter(
    theme_markedness == "marked", recipient_markedness == "marked",
    # recipient_animacy == "inanimate",
    # theme_length == "short", recipient_length == "short",
    # canonicality == "marked"
  ) %>%
  # group_by(model, adaptation_dative, generalization_dative) %>%
  # mutate(
  #   best = (best - min(best))/(max(best) - min(best))
  # ) %>%
  filter(!str_detect(model, "333|444|555|666|777|888|999|1709")) %>%
  # filter(!str_detect(sentence, "(dolly|teddy)")) %>%
  mutate(
    # feature_config = canonicality
    feature_config = glue::glue("{recipient_animacy}-recip\n{theme_animacy}-theme"),
  ) %>%
  group_by(feature_config, adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(best),
    logprob = mean(best)
  ) %>%
  ungroup() %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    exposure = glue::glue("Exposure = {str_to_upper(adaptation_dative)}")
  ) %>%
  # filter(adaptation_dative == "pp", generalization_dative == "do") %>%
  filter(adaptation_dative != generalization_dative) %>%
  # filter(adaptation_dative == "DO") %>%
  ggplot(aes(feature_config, logprob, color = generalization_dative)) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = logprob-ste, ymax=logprob+ste)) +
  facet_wrap(~exposure, nrow=1) +
  # scale_color_manual(values = c("#00BFC4")) +
  # facet_grid(exposure ~ recipient_animacy) +
  # theme_bw(base_size = 14, base_family = "Times") +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    x = "Feature Configuration",
    y = "log P(alt-form)",
    color = "Generalization Dative"
  )


# regressing out

seed = "1024"

fit <- lm(best ~ scale(initial, scale=FALSE), data = best_results %>% filter(canonicality=="marked", theme_definiteness=="definite"))
broom::tidy(fit)

best_results %>%
  filter(canonicality=="marked", theme_definiteness=="definite") %>%
  mutate(
    regressed_out = residuals(fit) + coef(fit)["(Intercept)"]
    # regressed_out = best
  ) %>%
  # filter(canonicality=="marked") %>%
  group_by(adaptation_dative, generalization_dative) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(regressed_out),
    logprob = mean(regressed_out)
  ) %>%
  ungroup() %>%
  filter(adaptation_dative != generalization_dative) %>%
  mutate(
    adaptation_dative = str_to_upper(adaptation_dative),
    generalization_dative = str_to_upper(generalization_dative),
    experiment = glue::glue("{adaptation_dative} --> {generalization_dative}")
  ) %>%
  ggplot(aes(experiment, logprob)) +
  # geom_boxplot()
  # geom_jitter(size = 2, width = 0.1)
  geom_point(size = 3, color = "steelblue") +
  geom_linerange(aes(ymin = logprob-ste, ymax=logprob+ste), color = "steelblue") +
  labs(
    x = "Generalization Experiment",
    y = "Average Log P(construction)"
  ) +
  theme(
    axis.text.x = element_markdown(),
    panel.grid = element_blank()
  )
