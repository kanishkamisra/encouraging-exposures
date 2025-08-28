library(tidyverse)
library(fs)

ours = "smolm-aochildes-vocab_8192-layers_8-attn_8-hidden_256-inter_1024-lr_1e-3-seed_1024"

metadata <- read_csv("data/zorro_metadata.csv") %>%
  mutate(
    phenomenon = factor(phenomenon, levels = c("Determiner Noun Agreement",
                                               "Subject Verb Agreement",
                                               "Anaphor Agreement",
                                               "Argument Structure",
                                               "Binding", "Case", "Ellipsis",
                                               "Filler Gap", "Irregular Verb",
                                               "Island Effects", "Local Attractor",
                                               "NPI licensing", "Quantifiers", "Overall"))
  )

metadata

systematic_results <- dir_ls("data/zorro_results/systematic-search/", regexp = "*.csv") %>%
  map_df(read_csv, .id = "model") %>%
  mutate(
    model = str_extract(model, "(?<=data/zorro_results/systematic-search/)(.*)(?=\\.csv)")
  ) %>%
  rename(phenomenon_string = phenomenon) %>%
  inner_join(metadata) %>%
  group_by(model, phenomenon) %>%
  summarize(accuracy = mean(accuracy)) %>%
  ungroup() %>%
  mutate(
    phenomenon = factor(phenomenon, levels = c("Determiner Noun Agreement",
                                               "Subject Verb Agreement",
                                               "Anaphor Agreement",
                                               "Argument Structure",
                                               "Binding", "Case", "Ellipsis",
                                               "Filler Gap", "Irregular Verb",
                                               "Island Effects", "Local Attractor",
                                               "NPI licensing", "Quantifiers", "Overall"),
                        labels = c("Determiner-Noun\nAgreement",
                                   "Subject-Verb\nAgreement",
                                   "Anaphor\nAgreement",
                                   "Argument\nStructure",
                                   "Binding", "Case", "Ellipsis",
                                   "Filler Gap", "Irregular Verb",
                                   "Island Effects", "Local Attractor",
                                   "NPI licensing", "Quantifiers", "Overall"))
  )

# systematic_results %>%
#   group_by(phenomenon) %>%
#   filter(accuracy == max(accuracy)) %>%
#   ungroup() %>%
#   count(model)

systematic_results %>%
  group_by(model) %>%
  summarize(accuracy = mean(accuracy))

dir_ls("data/zorro_results/systematic-search/", regexp = "*.csv") %>%
  map_df(read_csv, .id = "model") %>%
  mutate(
    model = str_extract(model, "(?<=data/zorro_results/systematic-search/)(.*)(?=\\.csv)")
  ) %>%
  rename(phenomenon_string = phenomenon) %>%
  inner_join(metadata) %>%
  group_by(model) %>%
  summarize(
    accuracy = mean(accuracy)
  ) %>%
  ungroup() %>%
  mutate(
    version = case_when(
      model == ours ~ "Final",
      TRUE ~ "Other"
    )
  ) %>%
  ggplot(aes(x = "LM", y = accuracy, color = version)) +
  geom_jitter(width = 0.1) +
  scale_y_continuous(limit = c(0.5, 0.8))
