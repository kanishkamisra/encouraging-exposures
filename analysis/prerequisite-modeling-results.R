library(tidyverse)
library(patchwork)
library(fs)
library(ggtext)
library(glue)
library(ggrepel)
library(lmerTest)

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
    n = n(),
    sd = sd(accuracy),
    cb = qt(1 - (0.05/2), n - 1) * sd/sqrt(n),
    accuracy = mean(accuracy)
  ) %>% 
  ungroup() %>%
  mutate(
    version = factor(version, levels = c("Final", "Other"))
  ) %>%
  ggplot(aes(x = "Overall", y = accuracy, color = version, fill = version, shape = version)) +
  geom_point(position = position_jitter(seed = 42, width = 0.1), size = 2.5, alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(limit = c(0.5, 0.8), breaks = scales::pretty_breaks(), labels = scales::percent_format(suffix = "")) +
  scale_color_manual(values = c("#0868ac","#bdbdbd"), aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(23, 21)) +
  theme_classic(base_size = 18, base_family = "Helvetica Neue") +
  theme(
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(color = "black")
  ) +
  labs(
    y = "Avg. Zorro Accuracy (in %)", 
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
  theme_classic(base_size = 18, base_family = "Helvetica Neue") +
  theme(
    axis.text = element_text(color = "black"),
    panel.grid = element_blank(),
    legend.position = "top"
  ) +
  labs(
    x = "Dative",
    y = "Diff. in Alternation Preference\n(NABA - NANA)",
    color = "Model",
    fill = "Model",
    shape = "Model"
  )

nabanana_joint_plot <- nabanana_results %>%
  group_by(model, dative, behavior) %>%
  summarize(
    diff = mean(diff)
  ) %>%
  pivot_wider(names_from = behavior, values_from = diff) %>%
  mutate(
    diff_diff = naba - nana
  ) %>%
  select(-naba, -nana) %>%
  pivot_wider(names_from = dative, values_from = diff_diff) %>%
  mutate(
    prod = (do + pp)/2
  ) %>%
  ungroup() %>%
  mutate(
    version = case_when(
      model == ours ~ "Final",
      TRUE ~ "Other"
    )
  ) %>%
  ggplot(aes(x = "Joint (DO and PO)", prod, color = version, fill = version, shape = version, group = model)) +
  # geom_point(size = 2.5, alpha = 0.8) +
  geom_line(show.legend = FALSE) +
  geom_point(position = position_jitter(seed = 42, width = 0.1), size = 2.5, alpha = 0.8) +
  geom_hline(yintercept = 0.0, linetype = "dashed") +
  scale_color_manual(values = c("#0868ac","#bdbdbd"), aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(23, 21)) +
  scale_y_continuous(limits = c(-0.1, 0.4)) +
  theme_classic(base_size = 18, base_family = "Helvetica Neue") +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.x = element_markdown(color = "black"),
    panel.grid = element_blank(),
    legend.position = "top",
    axis.title.x = element_blank(),
  ) +
  labs(
    y = "Diff. in Alternation Preference\n(NABA - NANA)",
    color = "Model",
    fill = "Model",
    shape = "Model"
  )

layout <-"ABBC
ABBC"
combined <- zorro_overall_plot + nabanana_overall_plot + nabanana_joint_plot & theme(legend.position = "top") 
combined + plot_layout(guides = "collect", design=layout)

ggsave("nature-submission/prereq-selection-results.pdf", width = 10.46, height = 4.92, dpi=300, device=cairo_pdf)


# reduced plot

(zorro_overall_plot + nabanana_joint_plot & theme(legend.position = "top")) + plot_layout(guides = "collect")

zorro_aggregate <- zorro_raw_results %>%
  group_by(model, version) %>%
  summarize(
    accuracy = mean(accuracy)
  ) %>% 
  ungroup() %>%
  mutate(
    version = factor(version, levels = c("Final", "Other"))
  )


zorro_aggregate %>% 
  filter(version == "Other") %>%
  ggplot(aes(x = "Overall", y = accuracy)) +
  geom_point(position = position_jitter(seed = 42, width = 0.1), size = 2.5, alpha = 0.8, color = "#bdbdbd", fill = "#bdbdbd") +
  geom_point(data = zorro_aggregate %>% filter(version == "Final"), color = "#0868ac", fill = "#0868ac", size = 2.5, alpha = 0.8, shape = 23) +
  # geom_curve(
  #   xend = 0.2, yend = 0.7,
  #   x = 0, y = 0.78,
  #   # curvature = -0.3,
  #   arrow = arrow(length = unit(2, "mm")),
  #   color = "grey"
  # ) +
  geom_text_repel(
    data = zorro_aggregate %>% filter(version == "Final"),
    label = "Final\nModel",
    color = "#0868ac",
    size = 4,
    family = "Helvetica Neue",
    fontface = "bold",
    # min.segment.length = 10
    # max.overlaps = Inf
    nudge_x = 0.3
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(limit = c(0.5, 0.8), breaks = scales::pretty_breaks(), labels = scales::percent_format(suffix = "")) +
  scale_shape_manual(values = c(23, 21)) +
  theme_classic(base_size = 18, base_family = "Helvetica Neue") +
  theme(
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(color = "black")
  ) +
  labs(
    y = "Avg. Zorro Accuracy (%)"
  )

ggsave("nature-submission/zorro-labeled.pdf", height = 4, width = 2.94, dpi = 300, device=cairo_pdf)


nabanana_results_agg <- nabanana_results %>%
  group_by(model, dative, behavior) %>%
  summarize(
    diff = mean(diff)
  ) %>%
  pivot_wider(names_from = behavior, values_from = diff) %>%
  mutate(
    diff_diff = naba - nana
  ) %>%
  select(-naba, -nana) %>%
  pivot_wider(names_from = dative, values_from = diff_diff) %>%
  mutate(
    prod = (do + pp)/2
  ) %>%
  ungroup() %>%
  mutate(
    version = case_when(
      model == ours ~ "Final",
      TRUE ~ "Other"
    )
  ) 

nabanana_results_agg %>%
  filter(version == "Other") %>%
  ggplot(aes(x = "Joint (DO and PO)", prod)) +
  # geom_point(size = 2.5, alpha = 0.8) +
  geom_point(position = position_jitter(seed = 42, width = 0.1), size = 2.5, alpha = 0.8, color = "#bdbdbd") +
  geom_point(data = nabanana_results_agg %>% filter(version=="Final"), size = 2.5, alpha = 0.8, color = "#0868ac", fill = "#0868ac", shape = 23) +
  geom_hline(yintercept = 0.0, linetype = "dashed") +
  geom_text_repel(
    data = nabanana_results_agg %>% filter(version == "Final"),
    label = "Final\nModel",
    color = "#0868ac",
    size = 4,
    family = "Helvetica Neue",
    fontface = "bold",
    # min.segment.length = 10
    # max.overlaps = Inf
    nudge_x = 0.3
  ) +
  # scale_color_manual(values = c("#0868ac","#bdbdbd"), aesthetics = c("color", "fill")) +
  # scale_shape_manual(values = c(23, 21)) +
  scale_y_continuous(limits = c(-0.1, 0.4)) +
  theme_classic(base_size = 18, base_family = "Helvetica Neue") +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.x = element_markdown(color = "black"),
    panel.grid = element_blank(),
    legend.position = "top",
    axis.title.x = element_blank(),
  ) +
  labs(
    y = "Diff. in Alternation Preference\n(NABA - NANA)",
    color = "Model",
    fill = "Model",
    shape = "Model"
  )

ggsave("nature-submission/nabanana-labeled.pdf", height = 4, width = 3.2, dpi = 300, device=cairo_pdf)



# final LMs

nabanana_final_results <- fs::dir_ls("data/nabanana/final/", regexp = "*.csv") %>%
  map_df(read_csv, .id = "model") %>%
  mutate(
    seed = as.integer(str_extract(model, "(?<=seed_)(.*)(?=\\.csv)")),
    model = str_extract(model, "(?<=data/nabanana/final/)(.*)(?=\\.csv)")
  ) %>%
  filter(seed %in% c(1709, 1024, 42, 211, 2409)) %>%
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

nabanana_final_results %>%
  mutate(
    dative = case_when(dative == "pp" ~ "PO", TRUE ~ dative)
  ) %>%
  # group_by(seed, dative, behavior) %>%
  group_by(dative) %>%
  mutate(
    diff = scale(diff)
  ) %>%
  ungroup() %>%
  group_by(dative, behavior) %>%
  summarize(
    n = n(),
    sd = sd(diff),
    conf = qt(1 - (0.05/2), n - 1) * sd/sqrt(n),
    diff = mean(diff)
  ) %>%
  ungroup() %>%
  mutate(
    dative = glue::glue("{str_to_upper(dative)} verbs"),
    behavior = str_to_upper(behavior),
    behavior_val = case_when(
      dative == "DO verbs" & behavior == "NABA" ~ glue("{behavior}<br>(e.g., <i>assign</i>)"),
      dative == "DO verbs" & behavior == "NANA" ~ glue("{behavior}<br>(e.g., <i>cost</i>)"),
      dative == "PO verbs" & behavior == "NABA" ~ glue("{behavior}<br>(e.g., <i>kick</i>)"),
      dative == "PO verbs" & behavior == "NANA" ~ glue("{behavior}<br>(e.g., <i>explain</i>)")
    )
  ) %>%
  ggplot(aes(behavior_val, diff, color = behavior)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = diff-conf, ymax = diff + conf), width = 0.2) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  # scale_color_manual(values = c("#6C0345", "#2D9596"), aesthetics = c("color", "fill")) +
  scale_color_manual(values = c("#e7298a", "#66a61e")) +
  facet_wrap(~dative, scales = "free") +
  theme_classic(base_size = 18, base_family = "Helvetica Neue") +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.x = element_markdown(color = "black"),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    # strip.text = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 16, family = "Helvetica Neue"),
    legend.position = "none"
  ) +
  labs(
    x = "Alternation Class",
    y = "Alternation Preference\n(z-scored)"
  )

ggsave("nature-submission/nabanana-final.pdf", width = 6.62, height = 4, dpi = 300, device = cairo_pdf)

nabanana_reg <- nabanana_final_results %>%
  mutate(
    behavior = factor(behavior, levels=c("nana", "naba"))
  )
# nabanana_final_results
nabanana_fit <- lmer(diff ~ dative * behavior + (1 | seed), data = nabanana_reg)
summary(nabanana_fit)

fit_do <- lmer(diff ~ behavior + (1 | seed), data = nabanana_reg %>% filter(dative == "do"))
fit_do_null <- lmer(diff ~ 1 + (1|seed), data = nabanana_reg %>% filter(dative == "do"))
anova(fit_do, fit_do_null)

fit_pp <- lmer(diff ~ behavior + (1 | seed), data = nabanana_reg %>% filter(dative == "pp"))
fit_pp_null <- lmer(diff ~ 1 + (1|seed), data = nabanana_reg %>% filter(dative == "pp"))
anova(fit_pp, fit_pp_null)

summary(fit_do)
summary(fit_pp)

nabanana_final_results %>%
  # group_by(seed, dative, behavior) %>%
  group_by(dative) %>%
  mutate(
    diff = scale(diff)
  ) %>%
  ungroup() %>%
  # group_by(dative, behavior) %>%
  # summarize(
  #   n = n(),
  #   sd = sd(diff),
  #   conf = qt(1 - (0.05/2), n - 1) * sd/sqrt(n),
  #   diff = mean(diff)
  # ) %>%
  ungroup() %>%
  mutate(
    dative = glue::glue("{str_to_upper(dative)} verbs"),
    behavior = str_to_upper(behavior),
    behavior_val = case_when(
      dative == "DO verbs" & behavior == "NABA" ~ glue("{behavior}\n(e.g., assign)"),
      dative == "DO verbs" & behavior == "NANA" ~ glue("{behavior}\n(e.g., cost)"),
      dative == "PP verbs" & behavior == "NABA" ~ glue("{behavior}\n(e.g., kick)"),
      dative == "PP verbs" & behavior == "NANA" ~ glue("{behavior}\n(e.g., explain)")
    )
  ) %>%
  ggplot(aes(diff, color = behavior_val,fill=behavior_val)) +
  geom_density(alpha = 0.2) +
  # geom_line() +
  # geom_point(size = 0.3, position=position_jitter(seed=1024,width=0.1)) +
  # geom_boxplot() +
  # geom_linerange(aes(ymin = diff-conf, ymax = diff + conf)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  facet_wrap(~dative, scales = "free") +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.x = element_markdown(color = "black"),
    panel.grid = element_blank(),
    legend.position = "top"
  ) +
  labs(
    x = "Alternation Class",
    y = "Alternation Behavior\n(z-scored)"
  )

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
  

