library(tidyverse)
library(lmerTest)
library(ggtext)

nabananas <- read_csv("data/naba-nana-sentences-240428.csv")

remove_list <- c("charged", "shot")

ours = "smolm-aochildes-vocab_8192-layers_8-attn_8-hidden_256-inter_1024-lr_1e-3-seed_1024"

nabanana_results <- fs::dir_ls("data/nabanana/systematic-search/", regexp = "*.csv") %>%
  map_df(read_csv, .id = "model") %>%
  mutate(
    # seed = as.integer(str_extract(file, "(?<=seed_)(.*)(?=\\.csv)"))
    model = str_extract(model, "(?<=data/nabanana/systematic-search/)(.*)(?=\\.csv)")
  ) %>%
  inner_join(nabananas) %>%
  separate(verb_type, into = c("behavior", "dative"), sep = "-") %>%
  mutate(
    behavior = case_when(
      verb == "carried" ~ "nana",
      TRUE ~ behavior
    ),
    diff = case_when(
      dative=="do" ~ pp_score - do_score,
      TRUE ~ do_score - pp_score
    )
  ) %>%
  filter(!verb %in% remove_list)

nabanana_results %>%
  group_by(model, dative, behavior) %>%
  summarize(
    n = n(),
    sd = sd(diff),
    conf = qt(1 - (0.05/2), n - 1) * sd/sqrt(n),
    diff = mean(diff)
  ) %>%
  ungroup() %>%
  mutate(
    version = case_when(
      model == ours ~ "Final",
      TRUE ~ "Other"
    )
  ) %>%
  ggplot(aes(behavior, diff, color = version)) +
  geom_point() +
  geom_linerange(aes(ymin = diff-conf, ymax = diff+conf)) +
  facet_grid(dative ~ model, scales = "free_y")

nabanana_results %>%
  group_by(model, dative, behavior) %>%
  summarize(
    diff = mean(diff)
  ) %>%
  pivot_wider(names_from = behavior, values_from = diff) %>%
  mutate(
    diff_diff = naba - nana
  ) %>%
  select(-naba, -nana) %>%
  ungroup() %>%
  mutate(
    version = case_when(
      model == ours ~ "Final",
      TRUE ~ "Other"
    ),
    dative = case_when(
      dative == "do" ~ "DO",
      dative == "pp" ~ "PO"
    )
  ) %>%
  ggplot(aes(dative, diff_diff, color = version, group = model)) +
  geom_point(size = 2) +
  geom_line() +
  geom_hline(yintercept = 0.0, linetype = "dashed") +
  scale_color_manual(values = c("#0868ac","#bdbdbd")) +
  theme_bw(base_size = 16, base_family = "Times") +
  theme(
    axis.text = element_text(color = "black"),
    panel.grid = element_blank()
  ) +
  labs(
    x = "Dative",
    y = "Preference Difference\n(NABA - NANA)",
    color = "Model"
  )
  # pivot_wider(names_from = dative, values_from = diff_diff) %>%
  # mutate(
  #   prod = do * pp
  # ) %>%
  # View()
