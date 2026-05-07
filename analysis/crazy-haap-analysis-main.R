library(tidyverse)
library(fs)
library(lmerTest)
library(patchwork)
library(ggtext)

strings <- expand.grid(c("p", "n"), c("a", "i"), c("d", "i"), c("g", "n")) %>%
  unite(string, c(Var1, Var2, Var3, Var4), sep="")

possible_hypotheses <- expand_grid(strings, strings, .name_repair = "universal") %>%
  janitor::clean_names() %>%
  mutate(
    hyp_string = glue::glue("{string_1}_{string_2}")
  ) %>%
  select(hyp_string)

model_results_raw <- read_csv("data/results/simulation-results/final-results-25-10-16.csv") %>%
  select(-file)

model_results <- model_results_raw %>%
  select(idx, hypothesis_id, hypothesis_item, seed, givenness_template, dative, do, pp)

hypothesis_stats <- model_results_raw %>%
  mutate(
    theme_string = glue::glue(
      "{substring(theme_pronominality, 1,1)}{substring(theme_animacy, 1,1)}{substring(theme_definiteness, 1,1)}{substring(theme_givenness, 1,1)}",
    ),
    recipient_string = glue::glue(
      "{substring(recipient_pronominality, 1,1)}{substring(recipient_animacy, 1,1)}{substring(recipient_definiteness, 1,1)}{substring(recipient_givenness, 1,1)}"
    ),
    hyp_string =glue::glue("{theme_string}_{recipient_string}")
  ) %>%
  filter(dative=="do", givenness_template==1,seed==1024) %>%
  count(hyp_string, length_diff) %>% 
  add_count(hyp_string, name="nn")

unique_hypotheses <- hypothesis_stats %>%
  distinct(hyp_string)

leftover <- possible_hypotheses %>%
  anti_join(unique_hypotheses)

leftover %>%
  mutate(
    def_given = str_detect(hyp_string, "(dn|ig)")
  )
# %>%
  # count(def_given) %>% 
  # View()

756

verbhood_delta <- model_results_raw %>%
  mutate(
    dative = case_when(dative == "do" ~ "DO", TRUE ~ "PO")
  ) %>%
  # group_by(seed, givenness_template, dative) %>%
  group_by(seed, dative) %>%
  summarize(
    n = n(),
    sd = sd(verbhood_diff),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    diff = mean(verbhood_diff),
    epoch = mean(best_epoch),
    acc = mean(verbhood_diff > 0)
  ) %>%
  ggplot(aes(dative, diff)) +
  geom_point(position=position_jitter(seed = 1024, width = 0.1)) +
  geom_linerange(aes(ymin = diff-cb, ymax = diff+cb), position=position_jitter(seed = 1024, width = 0.1)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_y_continuous(limits = c(0, 2.5)) +
  theme_classic(base_size = 16, base_family = "Helvetica Neue") +
  theme(
    legend.position = "None",
    panel.grid = element_blank(),
    # axis.text = element_markdown(color = "black")
    axis.text = element_text(color = "black"),
    axis.title.y = element_markdown()
  ) +
  labs(
    x = "Exposure Dative",
    # y = "Relative Verbhood"
    y = "Verbhood &Delta;"
  )

ggsave("nature-submission/verbhood-delta-results.pdf", width = 3.3, height = 3, dpi = 300, device = cairo_pdf)

# 382w, 342h

verbhood_acc <- model_results_raw %>%
  mutate(
    dative = case_when(dative == "do" ~ "DO", TRUE ~ "PO")
  ) %>%
  # group_by(seed, givenness_template, dative) %>%
  group_by(seed, dative) %>%
  summarize(
    n = n(),
    sd = sd(verbhood_diff),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    diff = mean(verbhood_diff),
    epoch = mean(best_epoch),
    acc = mean(verbhood_diff > 0)
  ) %>%
  ggplot(aes(dative, acc)) +
  geom_point(position = position_jitter(seed = 10, width = 0.1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent_format(suffix = "")) +
  theme_classic(base_size = 16, base_family = "Helvetica Neue") +
  theme(
    legend.position = "None",
    panel.grid = element_blank(),
    # axis.text = element_markdown(color = "black")
    axis.text = element_text(color = "black"),
    axis.title.y = element_markdown()
  ) +
  labs(
    x = "Exposure Dative",
    # y = "Relative Verbhood"
    y = "Verbhood Accuracy (%)"
  )

ggsave("nature-submission/verbhood-accuracy-results.pdf", width = 3.3, height = 3, dpi = 300, device = cairo_pdf)

verbhood_delta / verbhood_acc

ggsave("nature-submission/verbhood-results.pdf", height = 5.53, width = 3.3, dpi = 300, device = cairo_pdf)

redundant <- read_csv("data/results/simulation-results/redundant-haaps.csv") %>%
  mutate(
    red = TRUE
  )

multiverse_raw <- fs::dir_ls("data/results/simulation-results/haap-25-10-18/") %>%
  map_df(read_csv, .id = "file") 

multiverse <- multiverse_raw %>%
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
    code_score_og = code_score,
    code_score = case_when(haap_multiplier ==-1 ~ 8-code_score, TRUE ~ code_score),
    code_score_recipient = case_when(haap_recipient_multiplier ==-1 ~ 4-code_score_recipient, TRUE ~ code_score_recipient),
    code_score_theme = case_when(haap_theme_multiplier ==-1 ~ 4-code_score_theme, TRUE ~ code_score_theme),
    seed = factor(seed),
    givenness_template = factor(givenness_template),
    item = factor(item),
    hypothesis_id = factor(hypothesis_id),
    hypothesis_item = factor(hypothesis_item),
    length_score = case_when(
      pronominality_direction == "reversed" & haap_theme_multiplier == 1 & haap_recipient_multiplier == 1 ~ -length_score,
      TRUE ~ length_score
    ),
    # hypothesis_item = factor(hypothesis_item),
    # hamming_do = case_when(
    #   haap_multiplier == -1 ~ 8-hamming_do,
    #   TRUE ~ hamming_do
    # ),
    # hamming_po = case_when(
    #   haap_multiplier == -1 ~ 8-hamming_po,
    #   TRUE ~ hamming_po
    # ),
    score = code_score+length_score
  )
  # %>%
  # left_join(redundant) %>%
  # filter(is.na(red))

multiverse %>%
  filter((haap_do==TRUE & dative=="do") | (haap_po==TRUE & dative == "pp"), givenness_template==1,seed==42) %>%
  mutate(
    haap_score = code_score+length_score, 
    dative = case_when(dative == "pp" ~ "PO", TRUE ~ "DO"), 
    hypothesis_id = as.numeric(hypothesis_id),
    hypothesis_item = as.numeric(hypothesis_item),
    seed = as.numeric(seed),
    givenness_template = as.numeric(givenness_template),
    item = as.numeric(item)
  ) %>%
  inner_join(model_results_raw %>% select(-do, -pp))

# multiverse %>%
#   filter((haap_do==TRUE & dative=="do") | (haap_po==TRUE & dative == "pp"), givenness_template==1,seed==42) %>% 
#   count(idx)

haaps <- model_results_raw %>% 
  mutate(
    givenness_template = factor(givenness_template),
    seed = factor(seed),
    hypothesis_id = factor(hypothesis_id),
    hypothesis_item = factor(hypothesis_item)
  ) %>%
  # filter(givenness_template==1, seed == 211) %>%
  inner_join(
    multiverse %>%
      filter(
        (haap_do==TRUE & dative=="do") | (haap_po==TRUE & dative == "pp"), 
        # givenness_template==1, seed==42
      ) %>%
      mutate(haap_score = code_score+length_score) %>%
      select(idx, dative, givenness_template, seed, hypothesis_id, hypothesis_item, code_id, code_score, length_score, haap_score, score)
  ) %>%
  mutate(
    dative = case_when(dative == "pp" ~ "PO", TRUE ~ "DO")
  ) 

haaps %>%
  filter(theme_animacy=="inanimate", recipient_animacy == "animate") %>%
  mutate(
    altform = case_when(
      dative == "DO" ~ pp,
      TRUE ~ do
    )
  ) %>%
  ggplot(aes(haap_score, altform, color = glue::glue("{givenness_template}-{seed}"))) + 
  # geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~dative, scales = "free")

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

# code2haap %>% count(haap_do, haap_do_theme, haap_do_recipient) %>% View()

fits_do <- multiverse %>%
  filter(dative == "do") %>%
  left_join(redundant) %>%
  filter(is.na(red)) %>%
  group_by(code_id) %>%
  nest() %>%
  mutate(
    fit = map(data, function(x){
      # lmer(pp ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template), data = x)
      lmer(pp ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = x)
      # lmer(pp ~ score + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = x)
    }),
    glanced = map(fit, function(x){
      broom.mixed::glance(x)
    }),
    tidied = map(fit, function(x){
      broom.mixed::tidy(x, conf.int = TRUE)
    })
  )

fits_do_score <- multiverse %>%
  filter(dative == "do") %>%
  # left_join(redundant) %>%
  # filter(is.na(red)) %>%
  group_by(code_id) %>%
  nest() %>%
  mutate(
    fit = map(data, function(x){
      # lmer(pp ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template), data = x)
      # lmer(pp ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = x)
      lmer(pp ~ score + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = x)
    }),
    glanced = map(fit, function(x){
      broom.mixed::glance(x)
    }),
    tidied = map(fit, function(x){
      broom.mixed::tidy(x, conf.int = TRUE)
    })
  )

fits_pp <- multiverse %>%
  filter(dative == "pp") %>%
  left_join(redundant) %>%
  filter(is.na(red)) %>%
  group_by(code_id) %>%
  nest() %>%
  mutate(
    fit = map(data, function(x){
      # lmer(do ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template), data = x)
      lmer(do ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = x)
      # lmer(do ~ score + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = x)
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
      broom.mixed::tidy(x, conf.int = TRUE)
    })
  )

multiverse %>%
  filter(dative == "pp", code_id==75, givenness_template == 1, seed==42) %>% View()

fits_pp_score <- multiverse %>%
  filter(dative == "pp") %>%
  group_by(code_id) %>%
  nest() %>%
  mutate(
    fit = map(data, function(x){
      # lmer(do ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template), data = x)
      # lmer(do ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = x)
      lmer(do ~ score + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = x)
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
      broom.mixed::tidy(x, conf.int = TRUE)
    })
  )

# fit_do_haap <- lmer(pp ~ code_score_recipient + code_score_theme + length_score + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = multiverse %>% filter(dative == "do", code_id == 15))
# summary(fit_do_haap)



# fit_pp_haap <- lmer(do ~ code_score + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = multiverse %>% filter(dative == "pp", code_id == 75))
# summary(fit_pp_haap)

# broom.mixed::glance(fit_pp_haap)
# null_pp

# fit.do.null <- lmer(pp ~ 1 + (1|seed) + (1|givenness_template), data = multiverse %>% filter(dative == "do", code_id == 15))
fit.do.null <- lmer(pp ~ 1 + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = multiverse %>% filter(dative == "do", code_id == 15))
null_do <- broom.mixed::glance(fit.do.null)

fit.do.null_scores <- lmer(pp ~ 1 + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = multiverse %>% filter(dative == "do", code_id == 15))
null_do_score <- broom.mixed::glance(fit.do.null_scores)

# fit.haap <- lmer(pp ~ 1 + (1|seed) + (1|givenness_template), data = multiverse %>% filter(dative == "do", code_id == 15))

# anova(fit.do.null, fit.haap) %>% tidy() %>% filter(!is.na(p.value)) %>% pull(p.value)

fit.pp.null <- lmer(do ~ 1 + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = multiverse %>% filter(dative == "pp", code_id == 15))
# fit.pp.null <- lmer(do ~ 1 + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = multiverse %>% filter(dative == "pp", code_id == 15))
null_pp <- broom.mixed::glance(fit.pp.null)

fit.pp.null_scores <- lmer(do ~ 1 + (1|seed) + (1|givenness_template) + (1|hypothesis_id:hypothesis_item), data = multiverse %>% filter(dative == "pp", code_id == 15))
null_pp_score <- broom.mixed::glance(fit.pp.null_scores)

# fits_do %>% 
#   select(-data, -glanced) %>% 
#   unnest(tidied) %>% 
#   inner_join(code2haap) %>% View()
# 
# fits_do %>% 
#   select(-data, -tidied) %>% 
#   unnest(glanced) %>% 
#   inner_join(code2haap) %>% View()

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
  # View("PO")
  ggplot(aes(id, metric, color = coding, shape = coding, fill = coding)) +
  geom_point(size = 2) +
  # scale_shape_manual(values = c(21,22,23,24,25,8,7)) +
  scale_shape_manual(values = c(23, 21,22,4)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  # scale_color_brewer(palette = "Dark2", aesthetics = c("color", 'fill')) +
  # scale_y_continuous(limits = c(0,1100), breaks = c(0,200,400,600,800)) +
  scale_color_manual(
    # values = c("#d95f02", "#e6ab02", "#CC79A7",  "#1f78b4", "#7570b3", "#1b9e77", "darkgrey"),
    # values = c("#d95f02", "#1f78b4", "#1b9e77", "darkgrey"),
    values = c("#d95f02", "#674ea7", "#6aa84f", "darkgrey"),
    aesthetics = c("color", "fill")
  ) +
  guides(
    fill=guide_legend(nrow=4, position = "inside"), 
    color=guide_legend(nrow=4, position = "inside"),
    shape=guide_legend(nrow=4, position = "inside"),  
  ) +
  theme_classic(base_size = 16, base_family = "Helvetica Neue") +
  theme(
    axis.title.y = element_markdown(),
    panel.grid = element_blank(),
    legend.position.inside = c(0.25,0.8),
    # legend.position = "none"
    # legend.position = "top",
    # legend.title.position = "top",
    # legend.title.align = 0.5,
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent"),
    axis.text = element_text(color="black"),
    plot.title = element_markdown()
  ) +
  labs(x = "Coding Scheme", y = "&Delta;LogLik")

ggsave("nature-submission/coding-schemes-pp-do.pdf", height = 3.49, width = 4.05, dpi=300, device=cairo_pdf)
ggsave("nature-submission/coding-schemes-pp-do.svg", height = 3.49, width = 4.05, dpi=300)

# 579 349

# FULL SCORE based model
fits_pp_score %>% 
  select(-data, -fit, -tidied) %>% 
  unnest(glanced) %>% 
  inner_join(code2haap) %>%
  mutate(
    coding = case_when(
      haap_po == TRUE ~ "HAAP-Both",
      haap_po_theme == TRUE ~ "HAAP-Theme",
      haap_po_recipient == TRUE ~ "HAAP-Recipient",
      TRUE ~ "Other"
    ),
    coding_full = case_when(
      haap_po == TRUE ~ "HAAP",
      TRUE ~ "Counterfactual"
    ),
    coding_full = factor(coding_full, levels = c("HAAP", "Counterfactual")),
    metric = logLik - null_pp_score$logLik,
  ) %>%
  ungroup() %>%
  arrange(metric) %>%
  mutate(id = row_number()) %>%
  # View("PO-score")
  # ggplot(aes(id, metric, color = coding, shape = coding, fill = coding)) +
  ggplot(aes(id, metric, color = coding_full, shape = coding_full, fill = coding_full)) +
  geom_point(size = 2) +
  # scale_shape_manual(values = c(23, 21,22,4)) +
  scale_shape_manual(values = c(23, 4)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  # scale_color_brewer(palette = "Dark2", aesthetics = c("color", 'fill')) +
  scale_color_manual(
    # values = c("#d95f02", "#e6ab02", "#CC79A7",  "#1f78b4", "#7570b3", "#1b9e77", "darkgrey"),
    # values = c("#d95f02", "#1f78b4", "#1b9e77", "darkgrey"),
    # values = c("#d95f02", "#674ea7", "#6aa84f", "darkgrey"),
    values = c("#d95f02", "darkgrey"),
    aesthetics = c("color", "fill")
  ) +
  guides(
    fill=guide_legend(nrow=4, position = "inside"), 
    color=guide_legend(nrow=4, position = "inside"),
    shape=guide_legend(nrow=4, position = "inside"),  
  ) +
  theme_classic(base_size = 16, base_family = "Helvetica Neue") +
  theme(
    axis.title.y = element_markdown(),
    panel.grid = element_blank(),
    legend.position.inside = c(0.25,0.8),
    # legend.position = "none"
    # legend.position = "top",
    # legend.title.position = "top",
    # legend.title.align = 0.5,
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent"),
    axis.text = element_text(color="black"),
    plot.title = element_markdown()
  ) +
  labs(x = "Coding Scheme", y = "&Delta;LogLik")

ggsave("nature-submission/coding-schemes-pp-do-score.pdf", height = 3.49, width = 4.05, dpi=300, device=cairo_pdf)
ggsave("nature-submission/coding-schemes-pp-do-score.svg", height = 3.49, width = 4.05, dpi=300)

do_fit <- fits_do %>% 
  select(-data, -fit, -tidied) %>% 
  unnest(glanced) %>% 
  inner_join(code2haap)

do_fit%>%
  mutate(
    coding = case_when(
      haap_do == TRUE ~ "HAAP",
      haap_do_theme == TRUE ~ "HAAP (Theme only)",
      haap_do_recipient == TRUE ~ "HAAP (Recipient only)",
      TRUE ~ "Other"
    ),
    metric = logLik - null_do$logLik,
    metric2 = AIC - null_do$AIC
  ) %>%
  ungroup() %>%
  arrange(metric) %>%
  left_join(redundant) %>%
  filter(is.na(red)) %>%
  mutate(id = row_number()) %>%
  # View("DO")
  ggplot(aes(id, metric, color = coding, shape = coding, fill=coding)) +
  # ggplot(aes(hamming_do, metric, color = coding, shape = coding, fill=coding)) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(23, 21,22,4)) +
  scale_color_manual(
    # values = c("#d95f02", "#e6ab02", "#CC79A7",  "#1f78b4", "#7570b3", "#1b9e77", "darkgrey"),
    # values = c("#d95f02", "#1f78b4", "#1b9e77", "darkgrey"),
    values = c("#d95f02", "#674ea7", "#6aa84f", "darkgrey"),
    aesthetics = c("color", "fill")
  ) +
  # scale_color_brewer(palette = "Dark2") +
  # scale_y_continuous(limits = c(300,900), breaks = scales::pretty_breaks()) +
  # scale_y_continuous(limits = c(-10,250), breaks = scales::pretty_breaks()) +
  scale_y_continuous(limits = c(100,350), breaks = scales::pretty_breaks()) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  # guides(fill=guide_legend(nrow=2), color=guide_legend(nrow=2), shape = guide_legend(nrow=2)) +
  guides(
    fill=guide_legend(nrow=4, position = "inside"), 
    color=guide_legend(nrow=4, position = "inside"),
    shape=guide_legend(nrow=4, position = "inside"),  
  ) +
  theme_classic(base_size = 16, base_family = "Helvetica Neue") +
  theme(
    axis.title.y = element_markdown(),
    panel.grid = element_blank(),
    legend.position.inside = c(0.29,0.8),
    # legend.position = "none"
    # legend.position = "top",
    # legend.title.position = "top",
    # legend.title.align = 0.5,
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent"),
    axis.text = element_text(color="black"),
    plot.title = element_markdown()
  ) +
  labs(x = "Coding Scheme", y = "&Delta;LogLik")
  # ggtitle(expression(paste("MPG ", rightarrow, " Weight analysis")))
  # ggtitle("DO  PO")
  # ggtitle(expression("DO" + symbol('\256') + "PO"))

ggsave("nature-submission/coding-schemes-do-pp.pdf", height = 3.49, width = 4.05, dpi=300, device=cairo_pdf)
ggsave("nature-submission/coding-schemes-do-pp.svg", height = 3.49, width = 4.05, dpi=300)

fits_do_score %>% 
  select(-data, -fit, -tidied) %>% 
  unnest(glanced) %>% 
  inner_join(code2haap) %>%
  mutate(
    coding = case_when(
      haap_do == TRUE ~ "HAAP",
      haap_do_theme == TRUE ~ "HAAP (Theme only)",
      haap_do_recipient == TRUE ~ "HAAP (Recipient only)",
      TRUE ~ "Other"
    ),
    coding_full = case_when(
      haap_do == TRUE ~ "HAAP",
      TRUE ~ "Counterfactual"
    ),
    coding_full = factor(coding_full, levels = c("HAAP", "Counterfactual")),
    metric = logLik - null_do_score$logLik
  ) %>%
  ungroup() %>%
  arrange(metric) %>%
  # left_join(redundant) %>%
  # filter(is.na(red)) %>%
  mutate(id = row_number()) %>%
  # View("DO-score")
  # ggplot(aes(id, metric, color = coding, shape = coding, fill=coding)) +
  # ggplot(aes(hamming_do, metric, color = coding, shape = coding, fill=coding)) +
  ggplot(aes(id, metric, color = coding_full, shape = coding_full, fill = coding_full)) +
  geom_point(size = 2) +
  # scale_shape_manual(values = c(23, 21,22,4)) +
  scale_shape_manual(values = c(23, 4)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(limits = c(-10,260), breaks = c(0,50,100,150,200,250)) +
  # scale_color_brewer(palette = "Dark2", aesthetics = c("color", 'fill')) +
  scale_color_manual(
    # values = c("#d95f02", "#e6ab02", "#CC79A7",  "#1f78b4", "#7570b3", "#1b9e77", "darkgrey"),
    # values = c("#d95f02", "#1f78b4", "#1b9e77", "darkgrey"),
    # values = c("#d95f02", "#674ea7", "#6aa84f", "darkgrey"),
    values = c("#d95f02", "darkgrey"),
    aesthetics = c("color", "fill")
  ) +
  guides(
    fill=guide_legend(nrow=4, position = "inside"), 
    color=guide_legend(nrow=4, position = "inside"),
    shape=guide_legend(nrow=4, position = "inside"),  
  ) +
  theme_classic(base_size = 16, base_family = "Helvetica Neue") +
  theme(
    axis.title.y = element_markdown(),
    panel.grid = element_blank(),
    legend.position.inside = c(0.25,0.8),
    # legend.position = "none"
    # legend.position = "top",
    # legend.title.position = "top",
    # legend.title.align = 0.5,
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent"),
    axis.text = element_text(color="black"),
    plot.title = element_markdown()
  ) +
  labs(x = "Coding Scheme", y = "&Delta;LogLik")

ggsave("nature-submission/coding-schemes-do-pp-score.pdf", height = 3.49, width = 4.05, dpi=300, device=cairo_pdf)
ggsave("nature-submission/coding-schemes-do-pp-score.svg", height = 3.49, width = 4.05, dpi=300)


pp_haap2 <- lmer(do ~ code_score_theme + code_score_recipient + length_score + 
                  (1|seed) + (1 | hypothesis_id:hypothesis_item),
                data = multiverse %>% filter(dative == "pp", haap_po==TRUE))

summary(pp_haap2)

pp_haap_main <- lmer(do ~ score + 
                   (1|seed) + (1|givenness_template) + (1 | hypothesis_id:hypothesis_item),
                 data = multiverse %>% filter(dative == "pp", haap_po==TRUE))


multiverse %>% filter(dative == "pp", haap_po==TRUE) %>%
  select(dative, do, seed, givenness_template, hypothesis_id, hypothesis_item)

haaps %>% 
  filter(dative == "PO") %>%
  select(dative, do, seed, givenness_template, hypothesis_id, hypothesis_item)

summary(pp_haap_main)

pp_haap_main2 <- lmer(pp ~ score + (1 |seed) + (1|givenness_template) + (1+ score |hypothesis_id:hypothesis_item), 
                      data = haaps %>% 
                        filter(dative == "DO") %>% 
                        mutate(
                          altform = do,
                        ))

summary(pp_haap_main2)

do_haap <- lmer(pp ~ code_score_theme + code_score_recipient + length_score + 
                  (1|seed) + (1|givenness_template) + (1 | hypothesis_id:hypothesis_item),
                data = multiverse %>% filter(dative == "do", haap_do==TRUE))

summary(do_haap)

multiverse %>% filter(dative == "pp", haap_po==TRUE) %>%
  group_by(seed, givenness_template) %>%
  nest() %>%
  mutate(
    cor = map(data, function(x){
      cor.test(x$code_score, x$do, method = "spearman") %>% tidy()
    })
  ) %>%
  unnest(cor)

multiverse %>% filter(dative == "do", haap_do==TRUE) %>%
  group_by(seed, givenness_template) %>%
  nest() %>%
  mutate(
    cor = map(data, function(x){
      cor.test(x$code_score, x$pp, method = "spearman") %>% tidy()
    })
  ) %>%
  unnest(cor)


fits_pp %>% 
  select(-data, -fit, -glanced) %>% 
  unnest(tidied) %>% 
  filter(effect == "fixed", term != "(Intercept)") %>%
  inner_join(code2haap) %>%
  left_join(redundant) %>%
  filter(is.na(red)) %>%
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
  # View()
  ggplot(aes(estimate, term, color=type, shape=type, fill=type)) +
  # ggplot(aes(estimate, term, color = coding)) +
  geom_point(size = 2, position = position_jitter(height = 0.2, width = 0.01, seed = 1024)) +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position = position_jitter(height = 0.2, width = 0.01, seed = 1024), width = 0.1) +
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



# fits_pp %>% 
#   select(-data, -fit, -glanced) %>% 
#   unnest(tidied) %>% 
#   filter(effect == "fixed", term != "(Intercept)") %>%
#   inner_join(code2haap) %>%
#   left_join(redundant) %>%
#   filter(is.na(red)) %>%
#   ungroup() %>%
#   mutate(
#     coding = case_when(
#       haap_po == TRUE ~ "HAAP-Both",
#       haap_po_theme == TRUE ~ "HAAP-Theme",
#       haap_po_recipient == TRUE ~ "HAAP-Recip",
#       TRUE ~ "Other"
#     )
#   ) %>% 
#   filter(coding %in% c("HAAP-Theme", "HAAP-Both")) %>%
#   mutate(
#     type = case_when(
#       coding == "HAAP-Theme" ~ "HAAP-Theme",
#       TRUE ~ "HAAP-Both"
#     ),
#     term = case_when(
#       term == "length_score" ~ "&Delta;Length",
#       term == "code_score_theme" ~ "Theme",
#       term == "code_score_recipient" ~ "Recipient",
#     ),
#     term = factor(term, levels = rev(c("&Delta;Length", "Theme", "Recipient")))
#   ) %>% 
#   filter(code_id == 75) %>%
#   # View()
#   # ggplot(aes(term, estimate, color=type, shape=type, fill=type)) +
#   ggplot(aes(term, estimate)) +
#   # ggplot(aes(estimate, term, color = coding)) +
#   geom_point(size = 2, color = "#d95f02", fill = "#d95f02", shape = 23) +
#   geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width = 0.1, color = "#d95f02") +
#   # geom_point(size = 2, position = position_jitter(width = 0.1, seed = 1024)) +
#   # geom_errorbar(aes(ymin=conf.low, ymax=conf.high), position = position_jitter(width = 0.1, seed = 1024), width = 0.1) +
#   geom_hline(yintercept = 0.0, linetype = "dashed") +
#   scale_shape_manual(values = c(23, 22)) +
#   scale_color_manual(
#     values = c("#d95f02", "#1b9e77"),
#     aesthetics = c("color", "fill")
#   ) +
#   theme_bw(base_size = 17, base_family = "Helvetica") +
#   theme(
#     legend.position = "top",
#     panel.grid = element_blank(),
#     axis.text.x = element_markdown(color = "black"),
#     axis.text = element_text(color = "black")
#   ) +
#   labs(
#     y = "Estimate",
#     x = "Term"
#   )


# just haap

fits_pp %>% 
  select(-data, -fit, -glanced) %>% 
  unnest(tidied) %>% 
  filter(effect == "fixed", term != "(Intercept)") %>%
  inner_join(code2haap) %>%
  left_join(redundant) %>%
  filter(is.na(red)) %>%
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
  filter(code_id == 75) %>%
  ggplot(aes(term, estimate)) +
  geom_hline(yintercept = 0.0, linetype = "dashed", linewidth = 0.4) +
  geom_point(size = 2, color = "#d95f02", fill = "#d95f02", shape = 23) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width = 0.1, color = "#d95f02") +
  scale_y_continuous(limits = c(-0.004,0.204)) +
  theme_classic(base_size = 18, base_family = "Helvetica Neue") +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),
    axis.text.x = element_markdown(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  labs(
    y = "Estimate",
    x = "Term"
  )

ggsave("nature-submission/lmer-result-haap-po.pdf", height = 4.68, width = 3.8, dpi = 300, device=cairo_pdf)
ggsave("nature-submission/lmer-result-haap-po.svg", height = 4.68, width = 3.8, dpi = 300)


# ------ ALTERNATE VERSION ------ #

fits_pp %>% 
  select(-data, -fit, -glanced) %>% 
  unnest(tidied) %>% 
  filter(effect == "fixed", term != "(Intercept)") %>%
  inner_join(code2haap) %>%
  left_join(redundant) %>%
  filter(is.na(red)) %>%
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
  filter(code_id %in% c(78, 76, 75)) %>%
  ggplot(aes(term, estimate, color=type, shape=type, fill=type, group = code_id)) +
  # ggplot(aes(estimate, term, color = coding)) +
  geom_point(size = 2, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(0.5), width = 0.1) +
  geom_hline(yintercept = 0.0, linetype = "dashed") +
  scale_shape_manual(values = c(23, 22)) +
  scale_color_manual(
    values = c("#d95f02", "#1b9e77"),
    aesthetics = c("color", "fill")
  ) +
  theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),
    axis.text.x = element_markdown(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  labs(
    y = "Estimate",
    x = "Term"
  )

# ------ ALTERNATE VERSION ------ #


multiverse %>%
  filter(code_id == 25, dative == "do") %>%
  group_by(idx, score = code_score+length_score) %>%
  summarize(pp = mean(pp)) %>%
  ggplot(aes(score, pp)) +
  geom_point() +
  geom_smooth(method = "lm") 

fits_do %>% 
  select(-data, -fit, -glanced) %>% 
  unnest(tidied) %>% 
  filter(effect == "fixed", term != "(Intercept)") %>%
  inner_join(code2haap) %>%
  left_join(redundant) %>%
  filter(is.na(red)) %>%
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
  theme_bw(base_size = 16, base_family = "Helvetica Neue") +
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

# just haap

fits_do %>% 
  select(-data, -fit, -glanced) %>% 
  unnest(tidied) %>% 
  filter(effect == "fixed", term != "(Intercept)") %>%
  left_join(redundant) %>%
  filter(is.na(red)) %>%
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
  filter(coding %in% c("HAAP-Both")) %>%
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
  ggplot(aes(term, estimate)) +
  geom_hline(yintercept = 0.0, linetype = "dashed", linewidth = 0.4) +
  geom_point(size = 2, color = "#d95f02", fill = "#d95f02", shape = 23) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width = 0.1, color = "#d95f02") +
  scale_y_continuous(limits = c(-0.004,0.204)) +
  theme_classic(base_size = 18, base_family = "Helvetica Neue") +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),
    axis.text.x = element_markdown(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  labs(
    y = "Estimate",
    x = "Term"
  )

ggsave("nature-submission/lmer-result-haap-do.pdf", height = 4.68, width = 3.8, dpi = 300, device=cairo_pdf)
ggsave("nature-submission/lmer-result-haap-do.svg", height = 4.68, width = 3.8, dpi = 300)
