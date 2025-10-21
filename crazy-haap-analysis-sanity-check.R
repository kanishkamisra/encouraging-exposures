library(tidyverse)
library(fs)
library(lmerTest)
library(patchwork)
library(ggtext)

model_results <- read_csv("data/results/simulation-results/final-results-25-10-16.csv") %>%
  select(-file) %>%
  select(idx, seed, givenness_template, dative, do, pp)

multiverse2 <- fs::dir_ls("data/results/simulation-results/haap-25-10-18/") %>%
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
    code_score = case_when(haap_multiplier ==-1 ~ 8-code_score, TRUE ~ code_score),
    code_score_recipient = case_when(haap_recipient_multiplier ==-1 ~ 4-code_score_recipient, TRUE ~ code_score_recipient),
    code_score_theme = case_when(haap_theme_multiplier ==-1 ~ 4-code_score_theme, TRUE ~ code_score_theme),
    seed = factor(seed),
    givenness_template = factor(givenness_template)
  )

code2haap2 <- multiverse2 %>% 
  distinct(code_id, haap_do, haap_po, haap_do_theme, haap_po_theme, haap_do_recipient, haap_po_recipient, haap_multiplier, haap_theme_multiplier, haap_recipient_multiplier)

fits_do2 <- multiverse2 %>%
  filter(dative == "do") %>%
  group_by(code_id) %>%
  nest() %>%
  mutate(
    fit = map(data, function(x){
      fitted <- lmer(pp ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template), data = x)
    }),
    glanced = map(fit, function(x){
      broom.mixed::glance(x)
    }),
    tidied = map(fit, function(x){
      broom.mixed::tidy(x)
    })
  )

fits_pp2 <- multiverse2 %>%
  filter(dative == "pp") %>%
  group_by(code_id) %>%
  nest() %>%
  mutate(
    fit = map(data, function(x){
      fitted <- lmer(do ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template), data = x)
    }),
    glanced = map(fit, function(x){
      broom.mixed::glance(x)
    }),
    tidied = map(fit, function(x){
      broom.mixed::tidy(x)
    })
  )

fit.do.null <- lmer(pp ~ 1 + (1|seed) + (1|givenness_template), data = multiverse2 %>% filter(dative == "do", code_id == 0))
null_do <- broom.mixed::glance(fit.do.null)

fit.pp.null <- lmer(do ~ 1 + (1|seed) + (1|givenness_template), data = multiverse2 %>% filter(dative == "pp", code_id == 0))
null_pp <- broom.mixed::glance(fit.pp.null)

fits_do2 %>% 
  select(-data, -glanced) %>% 
  unnest(tidied) %>% 
  inner_join(code2haap2) %>% View()

pp_fit <- fits_pp2 %>% 
  select(-data, -fit, -tidied) %>% 
  unnest(glanced) %>% 
  inner_join(code2haap2)

pp_fit %>%
  mutate(
    coding = case_when(
      haap_po == TRUE ~ "HAAP-Both",
      haap_po_theme == TRUE ~ "HAAP-Theme",
      haap_po_recipient == TRUE ~ "HAAP-Recip",
      TRUE ~ "Other"
    ),
    metric = logLik - null_pp$logLik,
  ) %>%
  ungroup() %>%
  arrange(metric) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(id, metric, color = coding, shape = coding)) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 16, base_family = "Times") +
  theme(
    axis.title.y = element_markdown()
  ) +
  labs(x = "Code ID", y = "&Delta;LogLik")

do_fit <- fits_do2 %>% 
  select(-data, -fit, -tidied) %>% 
  unnest(glanced) %>% 
  inner_join(code2haap2)

do_fit%>%
  mutate(
    coding = case_when(
      haap_do == TRUE ~ "HAAP-Both",
      haap_do_theme == TRUE ~ "HAAP-Theme",
      haap_do_recipient == TRUE ~ "HAAP-Recip",
      TRUE ~ "Other"
    ),
    metric = logLik - null_do$logLik,
  ) %>%
  ungroup() %>%
  arrange(metric) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(id, metric, color = coding, shape = coding)) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 16, base_family = "Times") +
  theme(
    axis.title.y = element_markdown()
  ) +
  labs(x = "Code ID", y = "&Delta;LogLik")


fits_pp2 %>% 
  select(-data, -fit, -glanced) %>% 
  unnest(tidied) %>% 
  filter(effect == "fixed", term != "(Intercept)") %>%
  inner_join(code2haap2) %>%
  ungroup() %>%
  mutate(
    coding = case_when(
      haap_po == TRUE ~ "HAAP-Both",
      haap_po_theme == TRUE ~ "HAAP-Theme",
      haap_po_recipient == TRUE ~ "HAAP-Recip",
      TRUE ~ "Other"
    )
  ) %>% 
  filter(coding %in% c("HAAP-Theme", "HAAP-Both")) %>%
  mutate(
    type = case_when(
      coding == "HAAP-Theme" ~ "HAAP-Theme",
      TRUE ~ "HAAP-Both"
    )
  ) %>%
  ggplot(aes(estimate, term, color=type)) +
  # ggplot(aes(estimate, term, color = coding)) +
  geom_point(position = position_jitter(height = 0.1, width = 0.01, seed = 1024)) +
  theme_bw(base_size = 17) +
  theme(
    legend.position = "top"
  )

fits_do2 %>% 
  select(-data, -fit, -glanced) %>% 
  unnest(tidied) %>% 
  filter(effect == "fixed", term != "(Intercept)") %>%
  inner_join(code2haap2) %>%
  ungroup() %>%
  mutate(
    coding = case_when(
      haap_do == TRUE ~ "HAAP-Both",
      haap_do_theme == TRUE ~ "HAAP-Theme",
      haap_do_recipient == TRUE ~ "HAAP-Recip",
      TRUE ~ "Other"
    )
  ) %>%
  filter(coding %in% c("HAAP-Recip", "HAAP-Both")) %>%
  mutate(
    type = case_when(
      coding == "HAAP-Recip" ~ "HAAP-Recip",
      TRUE ~ "HAAP-Both"
    )
  ) %>%
  ggplot(aes(estimate, term, color=type)) +
  # ggplot(aes(estimate, term, color = coding)) +
  geom_point(position = position_jitter(height = 0.1, width = 0.01, seed = 1024)) +
  theme_bw(base_size = 17) +
  theme(
    legend.position = "top"
  )



