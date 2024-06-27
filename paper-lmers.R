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
library(ggeffects)
library(marginaleffects)

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

generalization_set <- stream_in(file(glue("data/experiments/single_stimuli_dative_simulation_valtest_vbd_neutral_discourse/generalization.jsonl"))) %>%
  as_tibble()

generalization_set %>% count(dative)

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
  filter(!theme %in% c("bear", "bear over there", "cat", "dog", "cat outside the house", "cup", "dog outside the house", "football", "book", "cookies", "pencils", "legos", "cheerios")) %>% 
  filter(!recipient %in% c("bear", "bear over there", "cat", "dog", "cat outside the house", "cup", "dog outside the house", "football", "book", "cookies", "cheerios")) %>%
  filter(dative == "do") %>%
  group_by(feature_config_small, hypothesis_instance) %>%
  nest() %>%
  mutate(
    sampled = map(data, function(x){
      x %>%
        sample_n(1)
    })
  )

sampled_items %>% 
  select(-data) %>%
  unnest(sampled) %>%
  inner_join(adaptation_items) %>%
  mutate(
    sentence = str_replace(sentence, "it was a nice day . ", "")
  ) %>%
  select(-theme_markedness, -recipient_markedness, -sentence, -dative) %>%
  stream_out(file("data/experiments/single_stimuli_dative_simulation_final_items/adaptation.jsonl"))
  
generalization_set %>%
  stream_out(file("data/experiments/single_stimuli_dative_simulation_final_items/generalization.jsonl"))

unique_ids <- sampled_items %>%
  select(-data) %>%
  unnest(sampled) %>%
  pull(unique_id)

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


nc_do_pp <- coded_results %>%
  filter(context %in% c("No Context")) %>%
  filter(adaptation_dative == "do", generalization_dative == "pp")

nc_pp_do <- coded_results %>%
  filter(context %in% c("No Context")) %>%
  filter(adaptation_dative == "pp", generalization_dative == "do")

fit_nc_dopp <- lmer(
  real_logprob ~ 
    theme_pronominality + theme_animacy + 
    theme_definiteness + theme_length +
    recipient_pronominality + recipient_animacy +
    recipient_definiteness + recipient_length +
    theme_pronominality:recipient_pronominality +
    theme_animacy:recipient_animacy +
    theme_pronominality:theme_animacy +
    recipient_pronominality:recipient_animacy +
    (1 | model), 
  data = nc_do_pp 
  # %>% filter(unique_id %in% unique_ids)
)

summary(fit_nc_dopp, correlation = FALSE)

fit_nc_ppdo <- lmer(
  real_logprob ~ 
    theme_pronominality + theme_animacy + 
    theme_definiteness + theme_length +
    recipient_pronominality + recipient_animacy +
    recipient_definiteness + recipient_length +
    theme_pronominality:recipient_pronominality +
    theme_animacy:recipient_animacy +
    theme_pronominality:theme_animacy +
    recipient_pronominality:recipient_animacy +
    (1 | model),  
  data = nc_pp_do 
  %>% filter(unique_id %in% unique_ids)
)

summary(fit_nc_ppdo, correlation = FALSE)


given_do_pp <- coded_results %>%
  filter(context %in% c("Theme Given", "Recipient Given")) %>%
  filter(adaptation_dative == "do", generalization_dative == "pp")

given_pp_do <- coded_results %>%
  filter(context %in% c("Theme Given", "Recipient Given")) %>%
  filter(adaptation_dative == "pp", generalization_dative == "do")

fit_given_dopp <- lmer(
  real_logprob ~ 
    theme_pronominality * theme_animacy * 
    theme_definiteness + theme_length +
    recipient_pronominality * recipient_animacy *
    recipient_definiteness + recipient_length +
    theme_given + (1 | model),  
  data = given_do_pp 
  %>% filter(unique_id %in% unique_ids) 
  %>% mutate(
    theme_definiteness = case_when(theme_definiteness == 1 ~ 0.5, TRUE ~ -0.5),
    recipient_definiteness = case_when(recipient_definiteness == 1 ~ 0.5, TRUE ~ -0.5),
  )
  # %>%
    # mutate(theme_given = case_when(theme_given == 1 ~ 0.5, TRUE ~ -0.5))
)

summary(fit_given_dopp, correlation = FALSE)

fit_given_dopp %>%
  tidy() %>%
  select(-effect, -group, -df) %>%
  filter(!str_detect(term, "sd__")) %>%
  mutate(
    stars = case_when(
      p.value <= 0.001 ~ "***",
      p.value <= 0.01  ~ "**",
      p.value <= 0.05 ~ "*",
      TRUE ~ ""
    ),
    statistic = format(round(statistic, 3), nsmall = 3),
    estimate = format(round(estimate, 3), nsmall = 3), 
    std.error = format(round(std.error, 3), nsmall = 3),
    p = case_when(
      p.value <= 0.001 ~ "$<.001$",
      TRUE ~ glue("${round(p.value, digits=3)}$")
    )
    # statistic = glue("{statistic}{stars}")
  ) %>%
  select(-stars, -p.value) %>%
  mutate(
    term = term %>% 
      str_replace("(Intercept)", r"(\\textrm{Intercept})") %>%
      str_replace(":", r"( \\times )") %>%
      str_replace("theme_pronominality", r"(\\textrm{pron}_{thm})") %>%
      str_replace("theme_animacy", r"(\\textrm{anim}_{thm})") %>%
      str_replace("theme_definiteness", r"(\\textrm{def}_{thm})") %>%
      str_replace("theme_length", r"(\\textrm{length}_{thm})") %>%
      str_replace("theme_given", r"(\\textrm{given}_{thm})") %>%
      str_replace("recipient_pronominality", r"(\\textrm{pron}_{recip})") %>%
      str_replace("recipient_animacy", r"(\\textrm{anim}_{recip})") %>%
      str_replace("recipient_definiteness", r"(\\textrm{def}_{recip})") %>%
      str_replace("recipient_length", r"(\\textrm{length}_{recip})"),
    term = glue("${term}$")
  ) %>%
  View()

emmip(fit_given_dopp, theme_pronominality ~ theme_definiteness | theme_animacy, CIs = TRUE, plotit = FALSE) %>%
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
  scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(-5.6, -4.6)) +
  labs(
    x = "Theme Definiteness",
    y = "PP-generalization (EMM)",
    color = "Theme Pronominality",
    shape = "Theme Pronominality",
    linetype = "Theme Pronominality"
  )

ggsave("paper/dopp_givenness_theme_interaction.pdf", height = 4.96, width = 9.68, dpi=300, device=cairo_pdf)

emmip(fit_given_dopp, recipient_pronominality ~ recipient_definiteness | recipient_animacy, CIs = TRUE, plotit = FALSE) %>%
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
  scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(-5.41, -4.4)) +
  labs(
    x = "Recipient Definiteness",
    y = "PP-generalization (EMM)",
    color = "Recipient Pronominality",
    fill = "Recipient Pronominality",
    shape = "Recipient Pronominality",
    linetype = "Recipient Pronominality"
  )

ggsave("paper/dopp_givenness_recipient_interaction.pdf", height = 4.96, width = 9.68, dpi=300, device=cairo_pdf)

# emmeans(fit_given_dopp, ~ theme_pronominality * theme_animacy * theme_definiteness) %>%
#   contrast(simple = "each", combine = TRUE, adjust = "bonferroni")

emmeans(fit_given_dopp, ~ theme_pronominality * theme_animacy * theme_definiteness) %>%
  contrast(simple = "each", combine = TRUE, adjust = "bonferroni") %>%
  as_tibble() %>%
  mutate(
    theme_pronominality = case_when(
      theme_pronominality == -0.5 ~ "nominal",
      theme_pronominality == 0.5 ~ "pronoun",
      TRUE ~ "."
    ),
    theme_animacy = str_replace(theme_animacy, "0.5", "animate") %>%
      str_replace("-", "in"),
    theme_definiteness = str_replace(theme_definiteness, "0.5", "definite") %>%
      str_replace("-", "in")
    # contrast = str_replace(contrast, "(theme_pronominality-0.5)", "theme")
  ) %>%
  filter(!str_detect(contrast, "-")) %>%
  mutate(
    estimate = 2 * estimate, # to account for difference
    SE = 2 * SE, # to account for difference
  ) %>%
  mutate(
    contrast = str_remove(contrast, "0.5"),
    z.ratio = format(round(z.ratio, 3), nsmall = 3),
    estimate = format(round(estimate, 3), nsmall = 3), 
    SE = format(round(SE, 3), nsmall = 3),
    p = case_when(
      p.value <= 0.001 ~ "$<.001$",
      TRUE ~ glue("${round(p.value, digits=3)}$")
    )
  ) %>%
  rename(z = z.ratio) %>%
  select(-df, -p.value) %>%
  View()

emmeans(fit_given_dopp, ~ recipient_pronominality * recipient_animacy * recipient_definiteness) %>%
  contrast(simple = "each", combine = TRUE, adjust = "bonferroni") %>%
  as_tibble() %>%
  mutate(
    recipient_pronominality = case_when(
      recipient_pronominality == -0.5 ~ "nominal",
      recipient_pronominality == 0.5 ~ "pronoun",
      TRUE ~ "."
    ),
    recipient_animacy = str_replace(recipient_animacy, "0.5", "animate") %>%
      str_replace("-", "in"),
    recipient_definiteness = str_replace(recipient_definiteness, "0.5", "definite") %>%
      str_replace("-", "in")
  ) %>%
  filter(!str_detect(contrast, "-")) %>%
  mutate(
    estimate = 2 * estimate, # to account for difference
    SE = 2 * SE, # to account for difference
  ) %>%
  mutate(
    contrast = str_remove(contrast, "0.5"),
    z.ratio = format(round(z.ratio, 3), nsmall = 3),
    estimate = format(round(estimate, 3), nsmall = 3), 
    SE = format(round(SE, 3), nsmall = 3),
    p = case_when(
      p.value <= 0.001 ~ "$<.001$",
      TRUE ~ glue("${round(p.value, digits=3)}$")
    )
  ) %>%
  rename(z = z.ratio) %>%
  select(-df, -p.value) %>%
  View()


emmeans(fit_given_dopp, ~ recipient_pronominality * recipient_definiteness) %>%
  contrast(simple = "each", combine = TRUE, adjust = "bonferroni") %>%
  as_tibble()

# emmeans(fit_given_dopp, pairwise ~ theme_pronominality | theme_animacy)
emmip(fit_given_dopp, theme_pronominality ~ theme_definiteness | theme_animacy, CIs = TRUE)
emmip(fit_given_dopp, recipient_pronominality ~ recipient_definiteness | recipient_animacy, CIs = TRUE)
emmeans(fit_given_dopp, pairwise ~ recipient_pronominality | recipient_animacy, CIs = TRUE)

# avg_slopes(fit_given_dopp, variables = "theme_animacy")
df <- predict_response(fit_given_dopp, terms = c("theme_pronominality", "theme_definiteness", "theme_animacy"))

df

fit_given_ppdo <- lmer(
  real_logprob ~ 
    theme_pronominality * theme_animacy * 
    theme_definiteness + theme_length +
    recipient_pronominality * recipient_animacy *
    recipient_definiteness + recipient_length +
    theme_given + (1 | model),  
  data = given_pp_do 
  %>% filter(unique_id %in% unique_ids)
  %>% mutate(
    theme_definiteness = case_when(theme_definiteness == 1 ~ 0.5, TRUE ~ -0.5),
    recipient_definiteness = case_when(recipient_definiteness == 1 ~ 0.5, TRUE ~ -0.5),
  )
)

summary(fit_given_ppdo, correlation = FALSE)
fit_given_ppdo %>%
  tidy() %>%
  select(-effect, -group, -df) %>%
  filter(!str_detect(term, "sd__")) %>%
  mutate(
    stars = case_when(
      p.value <= 0.001 ~ "***",
      p.value <= 0.01  ~ "**",
      p.value <= 0.05 ~ "*",
      TRUE ~ ""
    ),
    statistic = format(round(statistic, 3), nsmall = 3),
    estimate = format(round(estimate, 3), nsmall = 3), 
    std.error = format(round(std.error, 3), nsmall = 3),
    p = case_when(
      p.value <= 0.001 ~ "$<.001$",
      TRUE ~ glue("${round(p.value, digits=3)}$")
    )
    # statistic = glue("{statistic}{stars}")
  ) %>%
  select(-stars, -p.value) %>%
  mutate(
    term = term %>% 
      str_replace("(Intercept)", r"(\\textrm{Intercept})") %>%
      str_replace(":", r"( \\times )") %>%
      str_replace("theme_pronominality", r"(\\textrm{pron}_{thm})") %>%
      str_replace("theme_animacy", r"(\\textrm{anim}_{thm})") %>%
      str_replace("theme_definiteness", r"(\\textrm{def}_{thm})") %>%
      str_replace("theme_length", r"(\\textrm{length}_{thm})") %>%
      str_replace("theme_given", r"(\\textrm{given}_{thm})") %>%
      str_replace("recipient_pronominality", r"(\\textrm{pron}_{recip})") %>%
      str_replace("recipient_animacy", r"(\\textrm{anim}_{recip})") %>%
      str_replace("recipient_definiteness", r"(\\textrm{def}_{recip})") %>%
      str_replace("recipient_length", r"(\\textrm{length}_{recip})"),
    term = glue("${term}$")
  ) %>%
  View()

emmip(fit_given_ppdo, theme_pronominality ~ theme_definiteness | theme_animacy, CIs = TRUE, plotit = FALSE) %>%
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
  scale_y_continuous(limits = c(-5.8, -4.8), breaks = scales::pretty_breaks()) +
  labs(
    x = "Theme Definiteness",
    y = "DO-generalization (EMM)",
    color = "Theme Pronominality",
    shape = "Theme Pronominality",
    linetype = "Theme Pronominality"
  )

ggsave("paper/ppdo_givenness_theme_interaction.pdf", height = 4.96, width = 9.68, dpi=300, device=cairo_pdf)

# emmeans(fit_given_ppdo, pairwise ~recipient_definiteness | recipient_pronominality)
# emmip(fit_given_ppdo, theme_definiteness ~ theme_animacy | theme_pronominality, CIs = TRUE)
emmip(fit_given_ppdo, recipient_pronominality ~ recipient_definiteness, CIs = TRUE)

emmip(fit_given_ppdo, recipient_pronominality ~ recipient_definiteness | recipient_animacy, CIs = TRUE, plotit = FALSE) %>%
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
  scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(-5.6, -5.1)) +
  labs(
    x = "Recipient Definiteness",
    y = "DO-generalization (EMM)",
    color = "Recipient Pronominality",
    fill = "Recipient Pronominality",
    shape = "Recipient Pronominality",
    linetype = "Recipient Pronominality"
  )

ggsave("paper/ppdo_givenness_recipient_interaction.pdf", height = 4.96, width = 9.68, dpi=300, device=cairo_pdf)

emmip(fit_given_ppdo, recipient_pronominality ~ recipient_definiteness, CIs = TRUE, plotit = FALSE) %>%
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
  scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(-5.6, -5.1)) +
  labs(
    x = "Recipient Definiteness",
    y = "DO-generalization (EMM)",
    color = "Recipient Pronominality",
    fill = "Recipient Pronominality",
    shape = "Recipient Pronominality",
    linetype = "Recipient Pronominality"
  )

ggsave("paper/ppdo_givenness_recipient_two-way-interaction.pdf", height = 5.03, width = 5.73, dpi=300, device=cairo_pdf)

# emmeans(fit_given_dopp, ~ theme_given + theme_definiteness + recipient_definiteness, CIs=TRUE)%>%
  # contrast(simple = "each", combine = TRUE, adjust = "bonferroni")

emmeans(fit_given_ppdo, pairwise ~ theme_pronominality * theme_animacy * theme_definiteness)

emmeans(fit_given_ppdo, ~ theme_pronominality * theme_animacy * theme_definiteness) %>%
  contrast(simple = "each", combine = TRUE, adjust = "bonferroni") %>%
  as_tibble() %>%
  mutate(
    theme_pronominality = case_when(
      theme_pronominality == -0.5 ~ "nominal",
      theme_pronominality == 0.5 ~ "pronoun",
      TRUE ~ "."
    ),
    theme_animacy = str_replace(theme_animacy, "0.5", "animate") %>%
      str_replace("-", "in"),
    theme_definiteness = str_replace(theme_definiteness, "0.5", "definite") %>%
      str_replace("-", "in")
    # contrast = str_replace(contrast, "(theme_pronominality-0.5)", "theme")
  ) %>%
  filter(!str_detect(contrast, "-")) %>%
  mutate(
    estimate = 2 * estimate, # to account for difference
    SE = 2 * SE, # to account for difference
  ) %>%
  mutate(
    contrast = str_remove(contrast, "0.5"),
    z.ratio = format(round(z.ratio, 3), nsmall = 3),
    estimate = format(round(estimate, 3), nsmall = 3), 
    SE = format(round(SE, 3), nsmall = 3),
    p = case_when(
      p.value <= 0.001 ~ "$<.001$",
      TRUE ~ glue("${round(p.value, digits=3)}$")
    )
  ) %>%
  rename(z = z.ratio) %>%
  select(-df, -p.value) %>%
  View()

emmeans(fit_given_ppdo, ~ recipient_pronominality * recipient_definiteness) %>%
  contrast(simple = "each", combine = TRUE, adjust = "bonferroni") %>%
  as_tibble() %>%
  mutate(
    recipient_pronominality = case_when(
      recipient_pronominality == -0.5 ~ "nominal",
      recipient_pronominality == 0.5 ~ "pronoun",
      TRUE ~ "."
    ),
    recipient_definiteness = str_replace(recipient_definiteness, "0.5", "definite") %>%
      str_replace("-", "in")
  ) %>%
  filter(!str_detect(contrast, "-")) %>%
  mutate(
    estimate = 2 * estimate, # to account for difference, alternate method = use "consec" in contrast()
    SE = 2 * SE, # to account for difference
  ) %>%
  mutate(
    contrast = str_remove(contrast, "0.5"),
    z.ratio = format(round(z.ratio, 3), nsmall = 3),
    estimate = format(round(estimate, 3), nsmall = 3), 
    SE = format(round(SE, 3), nsmall = 3),
    p = case_when(
      p.value <= 0.001 ~ "$<.001$",
      TRUE ~ glue("${round(p.value, digits=3)}$")
    )
  ) %>%
  rename(z = z.ratio) %>%
  select(-df, -p.value) %>%
  View()

emmeans(fit_given_ppdo, ~ recipient_pronominality * recipient_animacy * recipient_definiteness) %>%
  contrast(simple = "each", combine = TRUE, adjust = "bonferroni") %>%
  as_tibble() %>%
  mutate(
    recipient_pronominality = case_when(
      recipient_pronominality == -0.5 ~ "nominal",
      recipient_pronominality == 0.5 ~ "pronoun",
      TRUE ~ "."
    ),
    recipient_animacy = str_replace(recipient_animacy, "0.5", "animate") %>%
      str_replace("-", "in"),
    recipient_definiteness = str_replace(recipient_definiteness, "0.5", "definite") %>%
      str_replace("-", "in")
  ) %>%
  filter(!str_detect(contrast, "-")) %>%
  mutate(
    contrast = str_remove(contrast, "0.5"),
    z.ratio = format(round(z.ratio, 3), nsmall = 3),
    estimate = format(round(estimate, 3), nsmall = 3), 
    SE = format(round(SE, 3), nsmall = 3),
    p = case_when(
      p.value <= 0.001 ~ "$<.001$",
      TRUE ~ glue("${round(p.value, digits=3)}$")
    )
  ) %>%
  rename(z = z.ratio) %>%
  select(-df, -p.value) %>%
  View()

emmeans(fit_given_ppdo, ~ theme_pronominality * theme_animacy * theme_definiteness, adjust="bonferroni") %>%
  eff_size(sigma = sigma(fit_given_ppdo), edf = df.residual(fit_given_ppdo))

emmip(fit_given_ppdo, recipient_pronominality ~ recipient_definiteness | recipient_animacy, CIs = TRUE) 
emmeans(fit_given_ppdo, pairwise ~ recipient_pronominality | recipient_definiteness + recipient_animacy)

  # for PP to DO: 

# given_pp_do %>%
#   filter(unique_id %in% unique_ids) %>%
#   filter(adaptation_dative != generalization_dative) %>%
#   group_by(adaptation_dative, generalization_dative, theme_given) %>%
#   summarize(
#     ste = 1.96 * plotrix::std.error(real_logprob),
#     logprob = mean(real_logprob)
#   )





