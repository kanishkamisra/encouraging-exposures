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

seeds = c(42, 211, 2409, 1709, 1024)

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

arunachalam <- dir_ls("data/results/simulation-results/arunachalam-final/", regexp = "*/results.csv", recurse = TRUE) %>%
  map_df(read_stimuli, .id = "file") %>%
  mutate(
    seed = as.numeric(str_extract(file, "(?<=seed_)(.*)(?=/results)"))
  ) %>%
  select(-file) %>%
  filter(seed %in% seeds) %>%
  # filter(theme_pronominality != recipient_pronominality & theme_animacy != recipient_animacy & theme_definiteness != recipient_definiteness) %>%
  mutate(
    seed = factor(seed, levels = c(42, 211, 2409, 1709, 1024)),
  ) %>%
  pivot_longer(do:pp, names_to = "generalization_dative", values_to = "logprob") %>%
  filter(generalization_dative == "do") %>%
  mutate(
    dative = case_when(
      dative == "pp" ~ "PO",
      dative == "do" ~ "DO"
    )
  )

arunachalam  %>%
  group_by(dative, seed, givenness_order) %>%
  summarize(
    n = n(),
    sd = sd(logprob),
    conf = qt(1 - (0.05/2), n - 1) * sd/sqrt(n),
    diff = mean(logprob)
  ) %>%
  ungroup() %>%
  mutate(
    givenness_order = case_when(
      givenness_order == "recipient_theme" ~ "Recipient before Theme",
      TRUE ~ "Theme before Recipient"
    )
  ) %>%
  ggplot(aes(dative, diff, group = seed, color = seed)) +
  geom_point(size = 2.5) +
  geom_line() +
  geom_linerange(aes(ymin = diff-conf, ymax = diff+conf)) +
  facet_wrap(~givenness_order) +
  scale_color_manual(values = c("#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000"), aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(21,22,23,24,25)) +
  # scale_y_continuous(limits = c(-7, -5)) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(
    legend.position = "None",
    panel.grid = element_blank(),
  ) +
  labs(
    x = "Exposure Dative",
    y = "Avg. DO Log Prob\non Generalization Set"
  )

# width 637, height 339

arunachalam  %>%
  group_by(dative, givenness_order) %>%
  summarize(
    n = n(),
    sd = sd(logprob),
    conf = qt(1 - (0.05/2), n - 1) * sd/sqrt(n),
    diff = mean(logprob)
  ) %>%
  ungroup() %>%
  mutate(
    givenness_order = case_when(
      givenness_order == "recipient_theme" ~ "Recipient before Theme",
      TRUE ~ "Theme before Recipient"
    )
  ) %>%
  ggplot(aes(dative, diff)) +
  geom_point(size = 2.5) +
  geom_line() +
  geom_linerange(aes(ymin = diff-conf, ymax = diff+conf)) +
  facet_wrap(~givenness_order) +
  scale_y_continuous(limits = c(-7, -5.5)) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(
    legend.position = "None",
    panel.grid = element_blank(),
  ) +
  labs(
    x = "Exposure Dative",
    y = "Avg. DO Log Prob\non Generalization Set"
  )

##

arunachalam  %>%
  mutate(
    givenness_order = case_when(
      givenness_order == "recipient_theme" ~ "Recipient before Theme",
      TRUE ~ "Theme before Recipient"
    )
  ) %>%
  ggplot(aes(dative, logprob)) +
  geom_boxplot() +
  # geom_point(size = 2.5) +
  # geom_line() +
  # geom_linerange(aes(ymin = diff-conf, ymax = diff+conf)) +
  facet_wrap(~givenness_order) +
  # scale_y_continuous(limits = c(-7, -5.5)) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(
    legend.position = "None",
    panel.grid = element_blank(),
  ) +
  labs(
    x = "Exposure Dative",
    y = "Avg. DO Log Prob\non Generalization Set"
  )

arunachalam  %>%
  mutate(
    givenness_order = case_when(
      givenness_order == "recipient_theme" ~ "Recipient before Theme",
      TRUE ~ "Theme before Recipient"
    )
  ) %>%
  ggplot(aes(logprob, color = dative, fill = dative)) +
  geom_histogram(alpha = 0.2) +
  facet_wrap(~givenness_order, nrow=2)

reg_data <- arunachalam %>%
  mutate(
    dative = case_when(
      dative == "PO" ~ 1,
      TRUE ~ 0
    ),
    theme_animacy = case_when(
      theme_animacy == "animate" ~ 1,
      TRUE ~ 0
    ),
    givenness_order = factor(givenness_order)
  )

fit_arunachalam <- lmer(logprob ~ dative + (dative | seed) + (dative | givenness_order), data = reg_data)
summary(fit_arunachalam)

fit_theme <- lmer(logprob ~ theme_animacy + (theme_animacy|seed) + (theme_animacy | givenness_order), data = reg_data %>% filter(dative == 0))
summary(fit_theme)

arunachalam  %>%
  group_by(dative, seed, theme_animacy) %>%
  summarize(
    n = n(),
    sd = sd(logprob),
    conf = qt(1 - (0.05/2), n - 1) * sd/sqrt(n),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  filter(dative == "DO") %>%
  ggplot(aes(theme_animacy, logprob)) +
  geom_point(size = 2) +
  geom_line(aes(group = seed)) +
  geom_linerange(aes(ymin = logprob-conf, ymax=logprob+conf))+
  # facet_grid(seed~dative) +
  # facet_wrap(~dative) +
  # facet_grid(givenness_order ~ dative) +
  # scale_y_continuous(limits = c(-7, -6)) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(
    legend.position = "None",
    panel.grid = element_blank(),
    axis.text=element_text(color = "black")
  ) +
  labs(
    # y = "Logprob of DO (generalization)"
    x = "Theme Animacy",
    y = "Avg. DO Log Prob\non Generalization Set"
  )


arunachalam  %>%
  filter(dative == "DO") %>%
  ggplot(aes(logprob, color = theme_animacy, fill = theme_animacy)) +
  geom_histogram(alpha = 0.2) +
  # facet_grid(seed~dative) +
  # facet_wrap(~dative) +
  # facet_grid(givenness_order ~ dative) +
  # scale_y_continuous(limits = c(-7, -6)) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(
    legend.position = "None",
    panel.grid = element_blank(),
    axis.text=element_text(color = "black")
  ) +
  labs(
    # y = "Logprob of DO (generalization)"
    # x = "Theme Animacy",
    x = "Avg. DO Log Prob\non Generalization Set"
  )


# ---Main Text Plots

arunachalam  %>%
  # group_by(dative) %>%
  # summarize(
  #   n = n(),
  #   sd = sd(logprob),
  #   conf = qt(1 - (0.05/2), n - 1) * sd/sqrt(n),
  #   diff = mean(logprob)
  # ) %>%
  # ungroup() %>%
  ggplot(aes(dative, logprob)) +
  # geom_point(size = 2.5, ) +
  geom_point(position = position_jitter(width = 0.2, seed=1024), alpha = 0.05) +
  geom_boxplot(alpha = 0.2, outliers = FALSE, width = 0.2) +
  # geom_linerange(aes(ymin = diff-conf, ymax = diff+conf)) +
  # scale_y_continuous(limits = c(-7, -5.5)) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(
    legend.position = "None",
    panel.grid = element_blank(),
  ) +
  labs(
    x = "Exposure Dative",
    y = "Avg. DO Log Prob\non Generalization Set"
  )

ggstatsplot::ggbetweenstats(arunachalam, x = dative, y = logprob)


arunachalam  %>%
  filter(theme_animacy == "inanimate") %>%
  group_by(dative) %>%
  summarize(
    n = n(),
    sd = sd(logprob),
    conf = qt(1 - (0.05/2), n - 1) * sd/sqrt(n),
    diff = mean(logprob)
  ) %>%
  ungroup() %>%
  ggplot(aes(dative, diff)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = diff-conf, ymax = diff+conf), width = 0.2) +
  scale_y_continuous(limits = c(-6.8, -5.6), breaks = scales::pretty_breaks()) +
  theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(
    legend.position = "None",
    panel.grid = element_blank(),
    axis.text = element_text(color = "black")
  ) +
  labs(
    x = "Exposure Dative",
    y = "Avg. DO Log Prob / token\non Generalization Set"
  )

ggsave("nature-submission/arunachalam-cross-structure.pdf", width = 4.51, height = 5.22, dpi = 300, device=cairo_pdf)


arunachalam  %>%
  group_by(dative, theme_animacy) %>%
  summarize(
    n = n(),
    sd = sd(logprob),
    conf = qt(1 - (0.05/2), n - 1) * sd/sqrt(n),
    logprob = mean(logprob)
  ) %>%
  ungroup() %>%
  filter(dative == "DO") %>%
  ggplot(aes(theme_animacy, logprob)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = logprob-conf, ymax=logprob+conf), width = 0.2)+
  # facet_grid(seed~dative) +
  # facet_wrap(~dative) +
  # facet_grid(givenness_order ~ dative) +
  scale_y_continuous(limits = c(-7, -6.2)) +
  theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(
    legend.position = "None",
    panel.grid = element_blank(),
    axis.text=element_text(color = "black")
  ) +
  labs(
    # y = "Logprob of DO (generalization)"
    x = "Theme Animacy",
    y = "Avg. DO Log Prob / token\non Generalization Set"
  )

ggsave("nature-submission/arunachalam-theme-animacy.pdf", width = 4.51, height = 5.22, dpi = 300, device=cairo_pdf)

