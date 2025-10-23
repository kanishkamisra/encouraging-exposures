library(tidyverse)
library(fs)
library(lmerTest)
library(patchwork)
library(ggtext)

model_results <- read_csv("data/results/simulation-results/final-results-25-10-16.csv") %>%
  select(-file) %>%
  select(idx, seed, givenness_template, dative, do, pp)

multiverse <- fs::dir_ls("data/results/simulation-results/haap-25-10-18/") %>%
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
    givenness_template = factor(givenness_template),
    item = factor(item)
  )

multiverse %>%
  filter((haap_do==TRUE & dative=="do") | (haap_po==TRUE & dative == "pp"), givenness_template==1,seed==42) %>%
  mutate(haap_score = code_score+length_score, dative = case_when(dative == "pp" ~ "PO", TRUE ~ "DO")) %>%
  ggplot(aes(haap_score, fill = dative)) +
  geom_histogram() + 
  facet_wrap(~dative) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(legend.position = "none", axis.text = element_text(color = "black")) +
  labs(
    x = "HAAP"
  )

code2haap <- multiverse %>% 
  distinct(code_id, haap_do, haap_po, haap_do_theme, haap_po_theme, haap_do_recipient, haap_po_recipient, haap_multiplier, haap_theme_multiplier, haap_recipient_multiplier)

fits_do <- multiverse %>%
  filter(dative == "do") %>%
  group_by(code_id) %>%
  nest() %>%
  mutate(
    fit = map(data, function(x){
      lmer(pp ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template), data = x)
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
      lmer(do ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template), data = x)
    }),
    # comparison = map2(data, fit, function(x, y) {
    #   fit.null = lmer(do ~ 1 + (1|seed) + (1|givenness_template), data = x)
    #   p_val = anova(y, fit.null) %>% tidy() %>% filter(!is.na(p.value)) %>% pull(p.value)
    #   return(p_val)
    # }),
    glanced = map(fit, function(x){
      broom.mixed::glance(x)
    }),
    tidied = map(fit, function(x){
      broom.mixed::tidy(x)
    })
  )

fit.do.null <- lmer(pp ~ 1 + (1|seed) + (1|givenness_template), data = multiverse %>% filter(dative == "do", code_id == 15))
null_do <- broom.mixed::glance(fit.do.null)

# fit.haap <- lmer(pp ~ 1 + (1|seed) + (1|givenness_template), data = multiverse %>% filter(dative == "do", code_id == 15))

# anova(fit.do.null, fit.haap) %>% tidy() %>% filter(!is.na(p.value)) %>% pull(p.value)

fit.pp.null <- lmer(do ~ 1 + (1|seed) + (1|givenness_template), data = multiverse %>% filter(dative == "pp", code_id == 0))
null_pp <- broom.mixed::glance(fit.pp.null)

fits_do %>% 
  select(-data, -glanced) %>% 
  unnest(tidied) %>% 
  inner_join(code2haap) %>% View()

pp_fit <- fits_pp %>% 
  select(-data, -fit, -tidied) %>% 
  unnest(glanced) %>% 
  inner_join(code2haap)

pp_fit %>%
  mutate(
    coding = case_when(
      haap_po == TRUE ~ "HAAP-Both",
      haap_po_theme == TRUE ~ "HAAP-Theme",
      haap_po_recipient == TRUE ~ "HAAP-Recipient",
      # haap_po_theme == TRUE & haap_do_recipient == FALSE ~ "HAAP-Theme",
      # haap_po_theme == TRUE & haap_do_recipient == TRUE ~ "HAAP-Theme + InvHAAP-Recip",
      # haap_po_recipient == TRUE & haap_do_theme == TRUE ~ "HAAP-Recip + InvHAAP-Theme",
      # haap_po_recipient == TRUE & haap_do_theme == FALSE ~ "HAAP-Recip",
      # haap_do_recipient == TRUE & haap_do_theme == TRUE ~ "InvHAAP-Both",
      TRUE ~ "Other"
    ),
    metric = logLik - null_pp$logLik,
  ) %>%
  ungroup() %>%
  arrange(metric) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(id, metric, color = coding, shape = coding, fill = coding)) +
  geom_point(size = 2) +
  # scale_shape_manual(values = c(21,22,23,24,25,8,7)) +
  scale_shape_manual(values = c(23, 21,22,4)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  # scale_color_brewer(palette = "Dark2", aesthetics = c("color", 'fill')) +
  scale_color_manual(
    # values = c("#d95f02", "#e6ab02", "#CC79A7",  "#1f78b4", "#7570b3", "#1b9e77", "darkgrey"),
    values = c("#d95f02", "#1f78b4", "#1b9e77", "darkgrey"),
    aesthetics = c("color", "fill")
  ) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(
    axis.title.y = element_markdown()
  ) +
  labs(x = "Code ID", y = "&Delta;LogLik")

# 579 349

do_fit <- fits_do %>% 
  select(-data, -fit, -tidied) %>% 
  unnest(glanced) %>% 
  inner_join(code2haap)

do_fit%>%
  mutate(
    coding = case_when(
      haap_do == TRUE ~ "HAAP-Both",
      haap_do_theme == TRUE ~ "HAAP-Theme",
      haap_do_recipient == TRUE ~ "HAAP-Recipient",
      TRUE ~ "Other"
    ),
    metric = logLik - null_do$logLik,
  ) %>%
  ungroup() %>%
  arrange(metric) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(id, metric, color = coding, shape = coding, fill=coding)) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(23, 21,22,4)) +
  scale_color_manual(
    # values = c("#d95f02", "#e6ab02", "#CC79A7",  "#1f78b4", "#7570b3", "#1b9e77", "darkgrey"),
    values = c("#d95f02", "#1f78b4", "#1b9e77", "darkgrey"),
    aesthetics = c("color", "fill")
  ) +
  # scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(300,900), breaks = scales::pretty_breaks()) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(
    axis.title.y = element_markdown()
  ) +
  labs(x = "Code ID", y = "&Delta;LogLik")


fits_pp %>% 
  select(-data, -fit, -glanced) %>% 
  unnest(tidied) %>% 
  filter(effect == "fixed", term != "(Intercept)") %>%
  inner_join(code2haap) %>%
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
    ),
    term = case_when(
      term == "length_score" ~ "&Delta;Length",
      term == "code_score_theme" ~ "Theme",
      term == "code_score_recipient" ~ "Recipient",
    ),
    term = factor(term, levels = rev(c("&Delta;Length", "Theme", "Recipient")))
  ) %>%
  ggplot(aes(estimate, term, color=type, shape=type, fill=type)) +
  # ggplot(aes(estimate, term, color = coding)) +
  geom_point(size = 2, position = position_jitter(height = 0.1, width = 0.01, seed = 1024)) +
  geom_vline(xintercept = 0.0, linetype = "dashed") +
  scale_shape_manual(values = c(23, 22)) +
  scale_color_manual(
    values = c("#d95f02", "#1b9e77"),
    aesthetics = c("color", "fill")
  ) +
  theme_bw(base_size = 17) +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),
    axis.text.y = element_markdown(color = "black")
  ) +
  labs(
    x = "Estimate",
    y = "Term",
    color = "Coding",
    fill = "Coding",
    shape = "Coding"
  )

fits_do %>% 
  select(-data, -fit, -glanced) %>% 
  unnest(tidied) %>% 
  filter(effect == "fixed", term != "(Intercept)") %>%
  inner_join(code2haap) %>%
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
    ),
    term = case_when(
      term == "length_score" ~ "&Delta;Length",
      term == "code_score_theme" ~ "Theme",
      term == "code_score_recipient" ~ "Recipient",
    ),
    term = factor(term, levels = rev(c("&Delta;Length", "Theme", "Recipient")))
  ) %>%
  ggplot(aes(estimate, term, color=type, shape = type, fill = type)) +
  # ggplot(aes(estimate, term, color = coding)) +
  geom_point(size = 2, position = position_jitter(height = 0.1, width = 0.01, seed = 1024)) +
  geom_vline(xintercept = 0.0, linetype = "dashed") +
  scale_shape_manual(values = c(23, 21)) +
  scale_color_manual(
    # values = c("#d95f02", "#e6ab02", "#CC79A7",  "#1f78b4", "#7570b3", "#1b9e77", "darkgrey"),
    values = c("#d95f02", "#1f78b4"),
    aesthetics = c("color", "fill")
  ) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),
    axis.text.y = element_markdown(color = "black")
  ) +
  labs(
    x = "Estimate",
    y = "Term",
    color = "Coding",
    fill = "Coding",
    shape = "Coding"
  )



