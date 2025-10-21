library(tidyverse)
library(fs)
library(lmerTest)
library(patchwork)
library(ggtext)

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

fit.do.null <- lmer(pp ~ 1 + (1|seed) + (1|givenness_template), data = multiverse %>% filter(dative == "do", code_id == 0))
null_do <- broom.mixed::glance(fit.do.null)

fit.pp.null <- lmer(do ~ 1 + (1|seed) + (1|givenness_template), data = multiverse %>% filter(dative == "pp", code_id == 0))
null_pp <- broom.mixed::glance(fit.pp.null)


fits_do <- multiverse %>%
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

fits_pp <- multiverse %>%
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

pp_fit <- fits_pp %>% 
  select(-data) %>% 
  unnest(fit) %>% 
  inner_join(code2haap)

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

# 5.14 x 3.43

do_fit <- fits_do %>% 
  select(-data) %>% 
  unnest(fit) %>% 
  inner_join(code2haap)

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




