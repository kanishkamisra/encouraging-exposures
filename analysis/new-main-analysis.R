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
library(gridtext)

# read_adaptation <- function(template_num) {
#   
#   stimuli <- glue("data/experiments/final/givenness_template_{template_num}.jsonl") %>% 
#     file() %>%
#     stream_in() %>% 
#     as_tibble() %>% 
#     mutate(givenness_template = template_num) %>%
#     mutate(
#       theme_givenness = case_when(
#         template %in% c("agent-only", "agent-recipient") ~ "new",
#         TRUE ~ "given"
#       ),
#       recipient_givenness = case_when(
#         template %in% c("agent-only", "agent-theme") ~ "new",
#         TRUE ~ "given"
#       )
#     )
#   
#   return(stimuli)
# }
# 
# adaptation <- bind_rows(
#   read_adaptation(1),
#   read_adaptation(2),
#   read_adaptation(3),
# )
# 
# adaptation %>% count(givenness_template)
# 
# all_results <- dir_ls("data/results/simulation-results/final/", regexp = "*/results.csv", recurse = TRUE) %>%
#   map_df(read_csv, .id = "file") %>%
#   mutate(
#     givenness_template = as.numeric(str_extract(file, "(?<=givenness_template_)(.*)(?=/smolm)")),
#     seed = as.numeric(str_extract(file, "(?<=seed_)(.*)(?=/results)"))
#   ) %>%
#   inner_join(adaptation) %>%
#   # filter(theme_pronominality != recipient_pronominality & theme_animacy != recipient_animacy & theme_definiteness != recipient_definiteness) %>%
#   mutate(
#     givenness_template = factor(givenness_template),
#     seed = factor(seed)
#   )

read_adaptation <- function(template_num, folder="final") {
  
  stimuli <- glue("data/experiments/{folder}/givenness_template_{template_num}.jsonl") %>% 
    file() %>%
    stream_in() %>% 
    as_tibble() %>% 
    mutate(givenness_template = template_num) %>%
    mutate(
      theme_givenness = case_when(
        template %in% c("agent-only", "agent-recipient") ~ "new",
        TRUE ~ "given"
      ),
      recipient_givenness = case_when(
        template %in% c("agent-only", "agent-theme") ~ "new",
        TRUE ~ "given"
      )
    )
  
  return(stimuli)
}

adaptation <- bind_rows(
  bind_rows(
    read_adaptation(1),
    read_adaptation(2),
    read_adaptation(3),
  ),
  bind_rows(
    read_adaptation(1, "final-2arg"),
    read_adaptation(2, "final-2arg"),
    read_adaptation(3, "final-2arg"),
  )
)

# adaptation %>% 
#   filter(givenness_template==1) %>%
#   write_csv("data/experiments/final-adaptation-25-10-16.csv")

adaptation %>% count(template)

all_results <- bind_rows(
  dir_ls("data/results/simulation-results/final/", regexp = "*/results.csv", recurse = TRUE) %>%
    map_df(read_csv, .id = "file") %>%
    mutate(
      givenness_template = as.numeric(str_extract(file, "(?<=givenness_template_)(.*)(?=/smolm)")),
      seed = as.numeric(str_extract(file, "(?<=seed_)(.*)(?=/results)"))
    ),
  dir_ls("data/results/simulation-results/final-2arg/", regexp = "*/results.csv", recurse = TRUE) %>%
    map_df(read_csv, .id = "file") %>%
    mutate(
      givenness_template = as.numeric(str_extract(file, "(?<=givenness_template_)(.*)(?=/smolm)")),
      seed = as.numeric(str_extract(file, "(?<=seed_)(.*)(?=/results)"))
    ) 
) %>%
  inner_join(adaptation) %>%
  # filter(theme_pronominality != recipient_pronominality & theme_animacy != recipient_animacy & theme_definiteness != recipient_definiteness) %>%
  mutate(
    givenness_template = factor(givenness_template),
    seed = factor(seed)
  )

all_results %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  group_by(givenness_template, seed, dative, gen_dative) %>%
  summarize(
    n = n(),
    sd = sd(score),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    score = mean(score)
  ) %>%
  ungroup() %>%
  filter(dative != gen_dative) %>%
  mutate(
    dative = case_when(
      dative == "pp" ~ "PO",
      dative == "do" ~ "DO"
    ),
    gen_dative = case_when(
      gen_dative == "pp" ~ "PO",
      gen_dative == "do" ~ "DO"
    )
  ) %>%
  mutate(
    exp = glue("{dative} to {gen_dative}"),
    seed = factor(seed, levels = c(42, 211, 2409, 1709, 1024))
  ) %>%
  ggplot(aes(exp, score, shape = seed, color = seed, fill = seed)) +
  geom_point(size = 2) +
  geom_line(aes(group =  interaction(seed, givenness_template))) +
  geom_linerange(aes(ymin = score-cb, ymax = score+cb)) +
  # geom_ribbon(aes(ymin = score-cb, ymax = score+cb, group =  interaction(seed, givenness_template)), color = NA, alpha = 0.2) +
  facet_wrap(~givenness_template) +
  # scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
  scale_color_manual(values = c("#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000"), aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_y_continuous(limits = c(-6.4, -5.4)) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(
    legend.position = "None",
    panel.grid = element_blank(),
    # axis.text = element_markdown(color = "black")
    axis.text = element_text(color = "black")
  ) +
  labs(
    x = "Experiment",
    y = "Avg. Log Prob on\nGeneralization Set"
  )

all_results %>%
  filter(recipient_animacy == "animate", 
         recipient_definiteness == "indefinite") %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(dative != gen_dative) %>%
  group_by(recipient_pronominality, recipient, dative, gen_dative) %>%
  summarize(
    n = n(),
    score = mean(score)
  ) %>% 
  ggplot(aes(recipient_pronominality, score)) +
  geom_jitter() +
  facet_wrap(~gen_dative)

all_results %>%
  # filter(template == "agent-only") %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  group_by(dative, length_diff, gen_dative) %>%
  summarize(
    n = n(),
    sd = sd(score),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    score = mean(score)
  ) %>%
  ungroup() %>%
  filter(dative != gen_dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}"),
  ) %>%
  ggplot(aes(length_diff, score, color = exp)) +
  geom_point()

arunachalam <- all_results %>%
  filter(template_type == "agent-2arg") %>%
  filter(
    recipient_animacy == "animate",
    recipient_definiteness == "definite",
    theme_definiteness == "definite",
    theme_pronominality == "noun",
    recipient_pronominality == "noun"
  ) %>%
  mutate(
    theme_len = map_int(theme, function(x) {length(str_split(x, " ") %>% .[[1]])}),
    recipient_len = map_int(recipient, function(x) {length(str_split(x, " ") %>% .[[1]])})
  ) %>%
  filter(theme_len <= 3, recipient_len <= 3) %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative == "do") %>%
  mutate(exp = glue("{dative} -> {gen_dative}"))

arunachalam %>%
  group_by(exp, theme_animacy) %>%
  summarize(
    n = n(),
    sd = sd(score),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    score = mean(score)
  ) %>%
  ggplot(aes(theme_animacy, score)) +
  geom_point() +
  geom_linerange(aes(ymin = score-cb, ymax = score+cb)) +
  facet_wrap(~exp)

fit_sudha_do_do <- lmer(score ~ theme_animacy + (1 | seed), data = arunachalam %>% filter(dative == "do"))
summary(fit_sudha_do_do)

reg_data <- all_results %>%
  # filter(theme != "them-a") %>%
  # filter(recipient != "them-a") %>%
  mutate(
    theme_pronominality = case_when(
      theme_pronominality == "pronoun" ~ 0.5,
      TRUE ~ -0.5
    ),
    recipient_pronominality = case_when(
      recipient_pronominality == "pronoun" ~ 0.5,
      TRUE ~ -0.5
    ),
    theme_animacy = case_when(
      theme_animacy == "animate" ~ 0.5,
      TRUE ~ -0.5
    ),
    recipient_animacy = case_when(
      recipient_animacy == "animate" ~ 0.5,
      TRUE ~ -0.5
    ),
    theme_definiteness = case_when(
      theme_definiteness == "definite" ~ 0.5,
      TRUE ~ -0.5
    ),
    recipient_definiteness = case_when(
      recipient_definiteness == "definite" ~ 0.5,
      TRUE ~ -0.5
    ),
    theme_givenness = case_when(
      # template %in% c("agent-only", "agent-recipient") ~ 0,
      theme_givenness == "given" ~ 0.5,
      TRUE ~ -0.5
    ),
    recipient_givenness = case_when(
      # template %in% c("agent-only", "agent-theme") ~ 0,
      recipient_givenness == "given" ~ 0.5,
      TRUE ~ -0.5
    )
  ) 
# %>%
  # filter(length_diff != 0) %>%
  # filter(!(seed == 42 & givenness_template == 1))

reg_data %>%
  count(seed, givenness_template)

# options(contrasts=c("contr.sum", "contr.poly"))

fit_do_pp <- lmer(pp ~ theme_pronominality * theme_animacy * theme_definiteness * theme_givenness +
                    recipient_pronominality * recipient_animacy * recipient_definiteness * recipient_givenness + 
                    length_diff + (1|seed) + (1|givenness_template),
                  data = reg_data %>%
                    filter(dative == "do"))

summary(fit_do_pp)

fit_pp_do <- lmer(pp ~ theme_pronominality * theme_animacy * theme_definiteness * theme_givenness +
                    recipient_pronominality * recipient_animacy * recipient_definiteness * recipient_givenness + 
                    length_diff + (1|seed) + (1|givenness_template),
                  data = reg_data %>%
                    filter(dative == "do"))

summary(fit_pp_do)

contrast_coded_results <- all_results %>%
  mutate(
    theme_animacy = factor(theme_animacy, levels = c("animate", "inanimate")),
    recipient_animacy = factor(recipient_animacy, levels = c("animate", "inanimate")),
    theme_pronominality = factor(theme_pronominality, levels = c("pronoun", "noun")),
    recipient_pronominality = factor(recipient_pronominality, levels = c("pronoun", "noun")),
    theme_definiteness = factor(theme_definiteness, levels = c("definite", "indefinite")),
    recipient_definiteness = factor(recipient_definiteness, levels = c("definite", "indefinite")),
    theme_givenness = factor(theme_givenness, levels = c("given", "new")),
    recipient_givenness = factor(recipient_givenness, levels = c("given", "new"))
  )
contrasts(contrast_coded_results$theme_animacy) <- "contr.sum"
contrasts(contrast_coded_results$recipient_animacy) <- "contr.sum"
contrasts(contrast_coded_results$theme_pronominality) <- "contr.sum"
contrasts(contrast_coded_results$recipient_pronominality) <- "contr.sum"
contrasts(contrast_coded_results$theme_definiteness) <- "contr.sum"
contrasts(contrast_coded_results$recipient_definiteness) <- "contr.sum"
contrasts(contrast_coded_results$theme_givenness) <- "contr.sum"
contrasts(contrast_coded_results$recipient_givenness) <- "contr.sum"

fit_do_pp_special <- lmer(pp ~ theme_pronominality * theme_animacy * theme_definiteness +
                    recipient_pronominality * recipient_animacy * recipient_definiteness +
                    theme_givenness + recipient_givenness + length_diff +
                    (1|seed) + (1|givenness_template),
                  data = contrast_coded_results %>%
                    filter(dative == "do"))

summary(fit_do_pp_special)

fit_do_pp_relative <- lmer(pp ~ theme_pronominality * theme_animacy * theme_definiteness * theme_givenness +
                             recipient_pronominality * recipient_animacy * recipient_definiteness * recipient_givenness + 
                             length_diff + (1|seed) + (1|givenness_template),
                           data = contrast_coded_results %>%
                             filter(dative == "do"))

summary(fit_do_pp_relative)

fit_pp_do_relative <- lmer(do ~ theme_pronominality * theme_animacy * theme_definiteness * theme_givenness +
                             recipient_pronominality * recipient_animacy * recipient_definiteness * recipient_givenness + 
                             length_diff + (1|seed) + (1|givenness_template),
                          data = contrast_coded_results %>%
                            filter(dative == "pp") %>% mutate(length_diff = -1 * length_diff))

summary(fit_pp_do_relative)
# car::Anova(fit_do_pp_special, type = 'III')
car::Anova(fit_do_pp_special, type = 3, test.statistic = "F")

emm <- emmeans(fit_do_pp_special, ~ recipient_pronominality + recipient_animacy | recipient_definiteness)
simple_effects <- pairs(emm, by = "recipient_definiteness", adjust = "Tukey")
summary(simple_effects)

emmip(fit_do_pp, recipient_pronominality ~ recipient_definiteness | recipient_animacy, CIs = TRUE, plotit = FALSE) %>%
  as_tibble() %>%
  mutate(
    recipient_pronominality = case_when(
      recipient_pronominality == 0.5 ~ "Pronoun",
      TRUE ~ "Nominal"
    ),
    recipient_definiteness = case_when(
      recipient_definiteness == 0.5 ~ "Definite",
      TRUE ~ "Indefinite"
    ),
    recipient_animacy = case_when(
      recipient_animacy == 0.5 ~ "Animate Recipient",
      TRUE ~ "Inanimate Recipient"
    )
  ) %>%
  ggplot(aes(recipient_definiteness, yvar, color = recipient_pronominality, fill = recipient_pronominality, shape = recipient_pronominality, group = recipient_pronominality)) +
  geom_point(position = position_dodge(0.1), size = 3) +
  geom_line(aes(linetype = recipient_pronominality), position = position_dodge(0.1)) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, position = position_dodge(0.1)) +
  facet_wrap(~ recipient_animacy) +
  scale_color_manual(values = c("#7570b3","#e6ab02"), aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(15, 23)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_bw(base_size = 16, base_family = "Times") +
  theme(
    legend.position = "top",
    panel.grid = element_blank()
  ) +
  labs(
    x = "Recipient Definiteness",
    y = "PO-generalization (EMM)",
    color = "Recipient Pronominality",
    fill = "Recipient Pronominality",
    shape = "Recipient Pronominality",
    linetype = "Recipient Pronominality"
  )


emmip(fit_do_pp, theme_pronominality ~ theme_definiteness | theme_animacy, CIs = TRUE,  plotit = FALSE) %>%
  as_tibble() %>%
  mutate(
    theme_pronominality = case_when(
      theme_pronominality == 0.5 ~ "Pronoun",
      TRUE ~ "Nominal"
    ),
    theme_definiteness = case_when(
      theme_definiteness == 0.5 ~ "Definite",
      TRUE ~ "Indefinite"
    ),
    theme_animacy = case_when(
      theme_animacy == 0.5 ~ "Animate Theme",
      TRUE ~ "Inanimate Theme"
    )
  ) %>%
  ggplot(aes(theme_definiteness, yvar, color = theme_pronominality, shape = theme_pronominality, group = theme_pronominality)) +
  geom_point(position = position_dodge(0.1), size = 3) +
  geom_line(aes(linetype = theme_pronominality), position = position_dodge(0.1), linewidth = 0.7) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, position = position_dodge(0.1)) +
  facet_wrap(~ theme_animacy) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    x = "Theme Definiteness",
    y = "PO-generalization (EMM)",
    color = "Theme Pronominality",
    shape = "Theme Pronominality",
    linetype = "Theme Pronominality"
  )

emmip(fit_do_pp, ~ recipient_animacy, CIs = TRUE)

fit_pp_do <- lmer(do ~ theme_pronominality * theme_animacy * theme_definiteness +
                    recipient_pronominality * recipient_animacy * recipient_definiteness +
                    theme_givenness + recipient_givenness + length_diff +
                    (1|seed) + (1|givenness_template),
                  data = reg_data %>%
                    filter(dative == "pp") %>%
                    mutate(length_diff = -1 * length_diff))

summary(fit_pp_do)

emmip(fit_pp_do, recipient_pronominality ~ recipient_definiteness | recipient_animacy, CIs = TRUE, plotit = FALSE) %>%
  as_tibble() %>%
  mutate(
    recipient_pronominality = case_when(
      recipient_pronominality == 0.5 ~ "Pronoun",
      TRUE ~ "Nominal"
    ),
    recipient_definiteness = case_when(
      recipient_definiteness == 0.5 ~ "Definite",
      TRUE ~ "Indefinite"
    ),
    recipient_animacy = case_when(
      recipient_animacy == 0.5 ~ "Animate Recipient",
      TRUE ~ "Inanimate Recipient"
    )
  ) %>%
  ggplot(aes(recipient_definiteness, yvar, color = recipient_pronominality, fill = recipient_pronominality, shape = recipient_pronominality, group = recipient_pronominality)) +
  geom_point(position = position_dodge(0.1), size = 3) +
  geom_line(aes(linetype = recipient_pronominality), position = position_dodge(0.1), linewidth = 0.7) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, position = position_dodge(0.1)) +
  facet_wrap(~ recipient_animacy) +
  # scale_color_brewer(palette="Dark2") +
  scale_color_manual(values = c("#7570b3","#e6ab02"), aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(15, 23)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    x = "Recipient Definiteness",
    y = "DO-generalization (EMM)",
    color = "Recipient Pronominality",
    fill = "Recipient Pronominality",
    shape = "Recipient Pronominality",
    linetype = "Recipient Pronominality"
  )

emmip(fit_pp_do, recipient_pronominality ~ recipient_definiteness, CIs = TRUE, plotit = FALSE) %>%
  as_tibble() %>%
  mutate(
    recipient_pronominality = case_when(
      recipient_pronominality == 0.5 ~ "Pronoun",
      TRUE ~ "Nominal"
    ),
    recipient_definiteness = case_when(
      recipient_definiteness == 0.5 ~ "Definite",
      TRUE ~ "Indefinite"
    )
  ) %>%
  ggplot(aes(recipient_definiteness, yvar, color = recipient_pronominality, fill = recipient_pronominality, shape = recipient_pronominality, group = recipient_pronominality)) +
  geom_point(position = position_dodge(0.1), size = 3) +
  geom_line(aes(linetype = recipient_pronominality), position = position_dodge(0.1), linewidth = 0.7) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, position = position_dodge(0.1)) +
  # facet_wrap(~ recipient_animacy) +
  # scale_color_brewer(palette="Dark2") +
  scale_color_manual(values = c("#7570b3","#e6ab02"), aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(15, 23)) +
  # scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(-5.6, -5.1)) +
  labs(
    x = "Recipient Definiteness",
    y = "DO-generalization (EMM)",
    color = "Recipient Pronominality",
    fill = "Recipient Pronominality",
    shape = "Recipient Pronominality",
    linetype = "Recipient Pronominality"
  )


emmip(fit_pp_do, theme_pronominality ~ theme_definiteness | theme_animacy, CIs = TRUE, plotit = FALSE) %>%
  as_tibble() %>%
  mutate(
    theme_pronominality = case_when(
      theme_pronominality == 0.5 ~ "Pronoun",
      TRUE ~ "Nominal"
    ),
    theme_definiteness = case_when(
      theme_definiteness == 0.5 ~ "Definite",
      TRUE ~ "Indefinite"
    ),
    theme_animacy = case_when(
      theme_animacy == 0.5 ~ "Animate Theme",
      TRUE ~ "Inanimate Theme"
    )
  ) %>%
  ggplot(aes(theme_definiteness, yvar, color = theme_pronominality, shape = theme_pronominality, group = theme_pronominality)) +
  geom_point(position = position_dodge(0.1), size = 3) +
  geom_line(aes(linetype = theme_pronominality), position = position_dodge(0.1), linewidth = 0.7) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, position = position_dodge(0.1)) +
  facet_wrap(~ theme_animacy) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    x = "Theme Definiteness",
    y = "DO-generalization (EMM)",
    color = "Theme Pronominality",
    shape = "Theme Pronominality",
    linetype = "Theme Pronominality"
  )
emmip(fit_do_pp, ~ theme_givenness, CIs = TRUE)


all_results %>%
  filter(!(seed == 42 & givenness_template == 1)) %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(dative != gen_dative) %>%
  group_by(combo, dative, gen_dative) %>%
  summarize(
    n = n(),
    sd = sd(score),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    score = mean(score)
  ) %>%
  ungroup() %>%
  filter(gen_dative == "pp") %>% 
  mutate(
    exp = glue("{dative} -> {gen_dative}"),
    combo = factor(combo),
    combo = fct_reorder(combo, score)
  ) %>%
  ggplot(aes(combo, score)) +
  geom_point() +
  facet_wrap(~exp, nrow = 2) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

  
## ALT models

fit_do_pp_alt <- lmer(pp ~ theme_pronominality + recipient_pronominality +
                      theme_animacy + recipient_animacy +
                        theme_definiteness + recipient_definiteness +
                        theme_givenness + recipient_givenness + length_diff + 
                        (1|seed) + (1|givenness_template),
                      data = reg_data %>%
                        filter(dative == "do"))

summary(fit_do_pp_alt)

emmip(fit_do_pp_alt, recipient_animacy ~ theme_animacy, CIs=TRUE)


fit_pp_do_alt <- lmer(do ~ theme_pronominality + recipient_pronominality +
                        theme_animacy + recipient_animacy +
                        theme_definiteness + recipient_definiteness +
                        theme_givenness + recipient_givenness + length_diff + 
                        (1|seed) + (1|givenness_template),
                      data = reg_data %>%
                        filter(dative == "pp"))
summary(fit_pp_do_alt)


adaptation %>%
  filter(theme_pronominality != recipient_pronominality & theme_animacy != recipient_animacy & theme_definiteness != recipient_definiteness)


all_results %>%
  # filter(theme_pronominality != recipient_pronominality) %>%
  mutate(
    combo_nolen = str_replace(combo, "_(-\\d|\\d)", ""),
    length_diff_code = case_when(
      length_diff > 0 ~ "t>r",
      length_diff < 0 ~ "r>t",
      length_diff == 0 ~ "t=r"
    ),
    combo_reduced = glue("{combo_nolen}_{length_diff_code}")
  ) %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  group_by(dative, combo_reduced, gen_dative) %>%
  summarize(
    n = n(),
    sd = sd(score),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    score = mean(score)
  ) %>% View()
  ggplot(aes(theme_pronominality))
  
  
ha_scores <- all_results %>%
  mutate(
    theme_pronominality = case_when(
      dative == "pp" & theme_pronominality == "pronoun" ~ 1,
      dative == "pp" & theme_pronominality == "noun" ~ 0,
      dative == "do" & theme_pronominality == "pronoun" ~ 0,
      dative == "do" & theme_pronominality == "noun" ~ 1
    ),
    recipient_pronominality = case_when(
      dative == "do" & recipient_pronominality == "pronoun" ~ 1,
      dative == "do" & recipient_pronominality == "noun" ~ 0,
      dative == "pp" & recipient_pronominality == "pronoun" ~ 0,
      dative == "pp" & recipient_pronominality == "noun" ~ 1
    ),
    theme_animacy = case_when(
      dative == "pp" & theme_animacy == "animate" ~ 0,
      dative == "pp" & theme_animacy == "inanimate" ~ 1,
      dative == "do" & theme_animacy == "inanimate" ~ 1,
      dative == "do" & theme_animacy == "animate" ~ 0,
    ),
    recipient_animacy = case_when(
      dative == "do" & recipient_animacy == "animate" ~ 1,
      dative == "do" & recipient_animacy == "inanimate" ~ 0,
      dative == "pp" & recipient_animacy == "animate" ~ 1,
      dative == "pp" & recipient_animacy == "inanimate" ~ 0,
    ),
    theme_definiteness = case_when(
      dative == "pp" & theme_definiteness == "definite" ~ 1,
      dative == "pp" & theme_definiteness == "indefinite" ~ 0,
      dative == "do" & theme_definiteness == "definite" ~ 0,
      dative == "do" & theme_definiteness == "indefinite" ~ 1,
    ),
    recipient_definiteness = case_when(
      dative == "do" & recipient_definiteness == "definite" ~ 1,
      dative == "do" & recipient_definiteness == "indefinite" ~ 0,
      dative == "pp" & recipient_definiteness == "definite" ~ 0,
      dative == "pp" & recipient_definiteness == "indefinite" ~ 1,
    ),
    theme_givenness = case_when(
      dative == "do" & template %in% c("agent-only", "agent-recipient") ~ 1,
      dative == "do" & !(template %in% c("agent-only", "agent-recipient")) ~ 0,
      dative == "pp" & !(template %in% c("agent-only", "agent-recipient")) ~ 1,
      dative == "pp" & template %in% c("agent-only", "agent-recipient") ~ 0
    ),
    recipient_givenness = case_when(
      dative == "do" & template %in% c("agent-only", "agent-theme") ~ 0,
      dative == "do" & !(template %in% c("agent-only", "agent-theme")) ~ 1,
      dative == "pp" & !(template %in% c("agent-only", "agent-theme")) ~ 0,
      dative == "pp" & template %in% c("agent-only", "agent-theme") ~ 1
    ),
    length_diff = case_when(
      dative == "pp" ~ -1 * length_diff,
      TRUE ~ length_diff
    ),
    length_score = case_when(
      length_diff == 0 ~ 0,
      length_diff > 0 ~ log(length_diff)+1,
      length_diff < 0 ~ -(log(abs(length_diff))+1)
    ),
    alt_length_score = case_when(
      length_diff == 0 ~ 0,
      length_diff > 0 ~ 1,
      length_diff < 0 ~ -1
    ),
    ha_theme = theme_pronominality + theme_animacy + theme_definiteness + theme_givenness,
    ha_recipient = recipient_pronominality + recipient_animacy + recipient_definiteness + recipient_givenness,
    ha_alt = ha_theme + ha_recipient + alt_length_score,
    ha_score = ha_theme + ha_recipient + length_score
  ) %>%
  filter(!(seed == 42 & givenness_template == 1))

# random digression

adaptation %>% 
  filter(givenness_template == 2) %>% 
  distinct(dative, theme_pronominality, recipient_pronominality, theme_animacy, 
        recipient_animacy, theme_definiteness, recipient_definiteness, template) %>%
  mutate(
    theme_pronominality = case_when(
      dative == "pp" & theme_pronominality == "pronoun" ~ 1,
      dative == "pp" & theme_pronominality == "noun" ~ 0,
      dative == "do" & theme_pronominality == "pronoun" ~ 0,
      dative == "do" & theme_pronominality == "noun" ~ 1
    ),
    recipient_pronominality = case_when(
      dative == "do" & recipient_pronominality == "pronoun" ~ 1,
      dative == "do" & recipient_pronominality == "noun" ~ 0,
      dative == "pp" & recipient_pronominality == "pronoun" ~ 0,
      dative == "pp" & recipient_pronominality == "noun" ~ 1
    ),
    theme_animacy = case_when(
      dative == "pp" & theme_animacy == "animate" ~ 0,
      dative == "pp" & theme_animacy == "inanimate" ~ 1,
      dative == "do" & theme_animacy == "inanimate" ~ 1,
      dative == "do" & theme_animacy == "animate" ~ 0,
    ),
    recipient_animacy = case_when(
      dative == "do" & recipient_animacy == "animate" ~ 1,
      dative == "do" & recipient_animacy == "inanimate" ~ 0,
      dative == "pp" & recipient_animacy == "animate" ~ 1,
      dative == "pp" & recipient_animacy == "inanimate" ~ 0,
    ),
    theme_definiteness = case_when(
      dative == "pp" & theme_definiteness == "definite" ~ 1,
      dative == "pp" & theme_definiteness == "indefinite" ~ 0,
      dative == "do" & theme_definiteness == "definite" ~ 0,
      dative == "do" & theme_definiteness == "indefinite" ~ 1,
    ),
    recipient_definiteness = case_when(
      dative == "do" & recipient_definiteness == "definite" ~ 1,
      dative == "do" & recipient_definiteness == "indefinite" ~ 0,
      dative == "pp" & recipient_definiteness == "definite" ~ 0,
      dative == "pp" & recipient_definiteness == "indefinite" ~ 1,
    ),
    theme_givenness = case_when(
      dative == "do" & template %in% c("agent-only", "agent-recipient") ~ 1,
      dative == "do" & !(template %in% c("agent-only", "agent-recipient")) ~ 0,
      dative == "pp" & !(template %in% c("agent-only", "agent-recipient")) ~ 1,
      dative == "pp" & template %in% c("agent-only", "agent-recipient") ~ 0
    ),
    recipient_givenness = case_when(
      dative == "do" & template %in% c("agent-only", "agent-theme") ~ 0,
      dative == "do" & !(template %in% c("agent-only", "agent-theme")) ~ 1,
      dative == "pp" & !(template %in% c("agent-only", "agent-theme")) ~ 0,
      dative == "pp" & template %in% c("agent-only", "agent-theme") ~ 1
    )
  ) %>% 
  select(-template) %>%
  mutate(
    ha_theme = theme_pronominality + theme_animacy + theme_definiteness + theme_givenness,
    ha_recipient = recipient_pronominality + recipient_animacy + recipient_definiteness + recipient_givenness
  ) %>% View()
  count(dative, ha_recipient, ha_theme) %>%
  pivot_wider(names_from = dative, values_from = n) %>% View()
  summarize(
    do = sum(do, na.rm=TRUE),
    pp = sum(pp, na.rm=TRUE)
  )

# end of digression

ha_scores %>%
  filter(givenness_template == 2, seed == 211) %>%
  count(dative, theme_pronominality, recipient_pronominality)
  
ha_scores %>%
  filter(givenness_template == 2, seed == 211) %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative != dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}"),
  ) %>%
  ggplot(aes(ha_score, fill = exp, group = exp)) +
  geom_histogram(color = NA)


bin2contrast <- function(x) {
  stopifnot(x %in% c(0,1))
  return (2*x - 1)
}


ha_scores_contrast <- ha_scores %>%
  mutate_at(vars(starts_with("theme_") | starts_with(("recipient_"))), bin2contrast) %>%
  mutate(
    ha_theme = theme_pronominality + theme_animacy + theme_definiteness + theme_givenness,
    ha_recipient = recipient_pronominality + recipient_animacy + recipient_definiteness + recipient_givenness,
    ha_alt = ha_theme + ha_recipient + alt_length_score,
    ha_score = ha_theme + ha_recipient + length_score
  )

fit_ha_do_pp <- lmer(pp ~ ha_score + (ha_score || seed) + (ha_score || givenness_template),
                     data = ha_scores %>% filter(dative == "do"), REML = F)

summary(fit_ha_do_pp)

fit_ha_pp_do <- lmer(do ~ ha_score + (1 | seed) + (1 | givenness_template),
                     data = ha_scores %>% filter(dative == "pp"), REML = F)

summary(fit_ha_pp_do)

# -- 

fit_ha_do_pp <- lmer(pp ~ ha_theme + ha_recipient + length_score + (1 | seed) + (1 | givenness_template),
                     data = ha_scores %>% filter(dative == "do"), REML = F)

summary(fit_ha_do_pp)

fit_ha_pp_do <- lmer(do ~ ha_theme + ha_recipient + length_score + (1 | seed) + (1 | givenness_template),
                     data = ha_scores %>% filter(dative == "pp"), REML = F)

summary(fit_ha_pp_do)

ha_scores %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative != dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}"),
  ) %>%
  ggplot(aes(factor(ha_recipient), score)) +
  geom_boxplot() +
  facet_wrap(~exp)

ha_scores_contrast %>% 
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative != dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}"),
    non_length = ha_theme + ha_recipient,
    metric = non_length
  ) %>%
  group_by(exp, seed, metric) %>%
  summarize(
    n = n(),
    sd = sd(score),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    score = mean(score)
  ) %>%
  ggplot(aes(metric, score, group = seed)) +
  geom_point() +
  geom_line() +
  # geom_linerange(aes(ymin = score-cb, ymax = score+cb)) +
  geom_ribbon(aes(ymin = score-cb, ymax = score+cb), color = NA, alpha = 0.3) +
  facet_wrap(~exp)

ha_scores_contrast %>% 
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative != dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}"),
    non_length = ha_theme + ha_recipient,
    metric = non_length
  ) %>%
  ggplot(aes(factor(non_length), score)) +
  geom_boxplot() +
  facet_wrap(~exp)

ha_scores_contrast %>% 
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative != dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}")
  ) %>%
  select(seed, idx, template, givenness_template, exp, ha_theme, ha_recipient, score) %>%
  pivot_longer(ha_theme:ha_recipient, names_to = "metric", values_to = "haap") %>%
  group_by(exp, seed, metric, haap) %>%
  summarize(
    n = n(),
    sd = sd(score),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    score = mean(score)
  ) %>%
  ggplot(aes(haap, score, color = metric, fill = metric, group = interaction(metric, seed))) +
  geom_point() +
  geom_line() +
  # geom_linerange(aes(ymin = score-cb, ymax = score+cb)) +
  geom_ribbon(aes(ymin = score-cb, ymax = score+cb), color = NA, alpha = 0.3) +
  facet_wrap(~exp)

ha_scores %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative != dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}"),
    non_length = ha_theme + ha_recipient,
    metric = non_length
  ) %>%
  group_by(exp, metric) %>%
  summarize(
    n = n(),
    sd = sd(score),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    score = mean(score)
  ) %>%
  ggplot(aes(metric, score)) +
  geom_point() +
  geom_line() +
  # geom_linerange(aes(ymin = score-cb, ymax = score+cb)) +
  geom_ribbon(aes(ymin = score-cb, ymax = score+cb), color = NA, alpha = 0.3) +
  facet_wrap(~exp)

ha_scores %>% group_by(dative, ha_score) %>% summarize(do = mean(do), pp = mean(pp)) %>% filter(dative == "pp") %>% ggplot(aes(ha_score,do)) + geom_point()


ha_scores %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative != dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}"),
  ) %>%
  group_by(exp, seed) %>%
  nest() %>%
  mutate(
    cor = map_df(data, function(x) {cor.test(x$score, x$ha_score, method = "spearman") %>% tidy()})
  )

# quick_fit_do_pp <- lmer(do ~ theme_pronominality + theme_animacy + theme_definiteness + theme_givenness +
#                           recipient_pronominality + recipient_animacy + recipient_definiteness + recipient_givenness +
#                           length_score + (1 | seed) + (1 | givenness_template), data = ha_scores_contrast %>% filter(dative == "pp"))
# summary(quick_fit_do_pp)

ha_scores %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative != dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}"),
  ) %>%
  count(exp, ha_score)
  ggplot(aes(ha_score, fill = exp, group = exp)) +
  geom_histogram(alpha = 0.4, color = NA) +
  facet_wrap(~givenness_template)

fit_haap_do_pp <- lmer(pp ~ ha_score + (1 + ha_score || seed) + (1 + ha_score || givenness_template),
                       data = ha_scores %>% filter(dative == "do"), REML = FALSE)
fit_haap_do_pp_null <- lmer(pp ~ 1 + (1 + ha_score || seed) + (1 + ha_score || givenness_template),
                            data = ha_scores_contrast %>% filter(dative == "do"), REML = FALSE)
summary(fit_haap_do_pp)

anova(fit_haap_do_pp, fit_haap_do_pp_null)

fit_haap_pp_do <- lmer(do ~ ha_alt + (1 | seed) + (1 | givenness_template),
                       data = ha_scores_contrast %>% filter(dative == "pp"), REML = FALSE)
fit_haap_pp_do_null <- lmer(do ~ 1 + (1 | seed) + (1 | givenness_template),
                            data = ha_scores_contrast %>% filter(dative == "pp"), REML = FALSE)
summary(fit_haap_pp_do)

anova(fit_haap_pp_do, fit_haap_pp_do_null)

all_results %>% 
  filter(!(seed == 42 & givenness_template == 1)) %>%
  pivot_longer(do:pp, names_to = "gen_dative", values_to = "score") %>%
  filter(gen_dative != dative) %>%
  mutate(
    exp = glue("{dative} -> {gen_dative}"),
  ) %>%
  group_by(exp, theme_animacy) %>%
  summarize(
    n = n(),
    sd = sd(score),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    score = mean(score)
  )
