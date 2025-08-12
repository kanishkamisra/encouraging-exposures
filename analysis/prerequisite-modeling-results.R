library(tidyverse)
library(patchwork)
library(fs)

ours = "smolm-aochildes-vocab_8192-layers_8-attn_8-hidden_256-inter_1024-lr_1e-3-seed_1024"

zorro_metadata <- read_csv("data/zorro_metadata.csv") %>%
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

zorro_raw_results <- dir_ls("data/zorro_results/systematic-search/", regexp = "*.csv") %>%
  map_df(read_csv, .id = "model") %>%
  mutate(
    model = str_extract(model, "(?<=data/zorro_results/systematic-search/)(.*)(?=\\.csv)")
  ) %>%
  rename(phenomenon_string = phenomenon) %>%
  inner_join(zorro_metadata) %>%
  mutate(
    version = case_when(
      model == ours ~ "Final",
      TRUE ~ "Other"
    )
  )

phenomena_wise_results <- zorro_raw_results %>%
  group_by(model, version, phenomenon) %>%
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

zorro_overall_plot <- zorro_raw_results %>%
  group_by(model, version) %>%
  summarize(
    accuracy = mean(accuracy)
  ) %>% 
  ungroup() %>%
  ggplot(aes(x = "Overall", y = accuracy, color = version, fill = version, shape = version)) +
  geom_point(position = position_jitter(seed = 42, width = 0.1), size = 2.5, alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(limit = c(0.5, 0.8), breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("#0868ac","#bdbdbd"), aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(23, 21)) +
  theme_bw(base_size = 16, base_family = "Times") +
  theme(
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(color = "black")
  ) +
  labs(
    y = "Zorro Accuracy", 
    color = "Model",
    fill = "Model",
    shape = "Model"
  )

#451x308

nabananas <- read_csv("data/naba-nana-sentences-240428.csv")

remove_list <- c("charged", "shot")

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

nabanana_overall_plot <- nabanana_results %>%
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
  ggplot(aes(dative, diff_diff, color = version, fill = version, shape = version, group = model)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_line(show.legend = FALSE) +
  geom_hline(yintercept = 0.0, linetype = "dashed") +
  scale_color_manual(values = c("#0868ac","#bdbdbd"), aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(23, 21)) +
  scale_y_continuous(limits = c(-0.25, 0.5)) +
  theme_bw(base_size = 16, base_family = "Times") +
  theme(
    axis.text = element_text(color = "black"),
    panel.grid = element_blank(),
    legend.position = "top"
  ) +
  labs(
    x = "Dative",
    y = "Preference Difference\n(NABA - NANA)",
    color = "Model",
    fill = "Model",
    shape = "Model"
  )

layout <-"ABB
ABB"
combined <- zorro_overall_plot + nabanana_overall_plot & theme(legend.position = "top") 
combined + plot_layout(guides = "collect", design=layout)

# nabanana_results %>%
#   group_by(model, dative, behavior) %>%
#   summarize(
#     n = n(),
#     sd = sd(diff),
#     conf = qt(1 - (0.05/2), n - 1) * sd/sqrt(n),
#     diff = mean(diff)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     version = case_when(
#       model == ours ~ "Final",
#       TRUE ~ "Other"
#     ),
#     dative = case_when(
#       dative == "do" ~ "DO",
#       dative == "pp" ~ "PO"
#     )
#   ) %>%
#   ggplot(aes(behavior, diff, color = version, shape = version, group = model)) +
#   geom_point() +
#   geom_linerange(aes(ymin = diff - conf, ymax = diff + conf)) +
#   geom_line() +
#   facet_wrap(~dative, scales = "free_y")
  

