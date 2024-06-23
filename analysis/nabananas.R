library(tidyverse)
library(lmerTest)
library(latex2exp)
library(ggtext)

real_datives <- read_csv("data/nana-naba-dative-contexts.csv")

nabananas <- read_csv("data/naba-nana-sentences-240428.csv")

lemma_counts <- real_datives %>%
  count(lemma)

leftovers <- nabananas %>% distinct(verb) %>%
  anti_join(real_datives %>%
              add_count(lemma) %>%
              distinct(verb, lemma, n)) %>%
  mutate(
    lemma = case_when(
      verb == "wished" ~ "wish",
      verb == "batted" ~ "bat",
      verb == "dragged" ~ "drag",
      verb == "hauled" ~ "haul",
      verb == "kicked" ~ "kick",
      verb == "addressed" ~ "address",
      verb == "announced" ~ "announce",
      verb == "described" ~ "describe",
      verb == "returned" ~ "return",
      verb == "assigned" ~ "assign",
      verb == "guaranteed" ~ "guarantee",
      verb == "owed" ~ "owe",
      verb == "promised" ~ "promise",
      verb == "rented" ~ "rent",
      verb == "traded" ~ "trade"
    )
  )

nabanana_counts <- bind_rows(
  nabananas %>% distinct(verb) %>%
    inner_join(real_datives %>%
                distinct(verb, lemma)),
  leftovers
) %>%
  inner_join(lemma_counts)


# remove_list <- c("charged", "traded", "shot")
remove_list <- c("charged", "shot")
# remove_list <- c("shot")
# remove_list <- c("")

nabanana_results <- fs::dir_ls("data/results/nabanana/", regexp = "*.csv") %>%
  map_df(read_csv, .id = "file") %>%
  mutate(
    seed = as.integer(str_extract(file, "(?<=seed_)(.*)(?=\\.csv)"))
  ) %>%
  select(-file) %>%
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
  # group_by(seed, dative) %>%
  # mutate(diff = scale(diff)) %>%
  # ungroup() %>%
  filter(!verb %in% remove_list)


# alt-form - observed form agg

agg_results <- nabanana_results %>% 
  group_by(dative, behavior) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(diff),
    delta = mean(diff)
  ) %>%
  mutate(
    dative = glue::glue("{str_to_upper(dative)} verbs"),
    behavior = str_to_upper(behavior)
  )

var.test(diff ~ behavior, data = nabanana_results %>% filter(dative == "do"))

var.test(diff ~ behavior, data = nabanana_results %>% filter(dative == "pp"))


nabanana_results %>% 
  group_by(dative, behavior) %>%
  summarize(
    n = n(),
    ste = 1.96 * plotrix::std.error(diff),
    delta = mean(diff)
  ) %>%
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
  ggplot(aes(behavior_val, delta, color = behavior)) +
  geom_point(size = 3) +
  # geom_jitter(size = 3, width = 0.2, alpha=0.3) +
  # geom_point(aes(behavior, delta, color = behavior, fill=behavior), shape = 23, 
             # data = agg_results, size = 3) + 
  # geom_linerange(aes(behavior, delta, ymin=delta-ste,ymax=delta+ste, color = behavior),
             # data = agg_results) +
  geom_errorbar(aes(ymin = delta-ste, ymax = delta+ste), width = 0.1) +
  scale_y_continuous(breaks = seq(-1.00, 0.60, by = 0.05)) +
  # scale_color_brewer(palette = "Set5", aesthetics = c("color", "fill")) +
  scale_color_manual(values = c("#6C0345", "#2D9596"), aesthetics = c("color", "fill")) +
  facet_wrap(~dative, scales = "free") +
  theme_bw(base_size = 17, base_family = "Times") + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color="black"),
    axis.title.y = element_markdown(),
  ) +
  labs(
    x = "Alternation Behavior",
    # y = TeX("$\\log\\frac{\\textit{p}(ALT-FORM)}{p()}$")
    # y = TeX("$\\Lambda$")
    # y = "log <i>p</i>(<span style='font-size: 11pt;'>ALT-FORM</span>) - log <i>p</i>(<span style='font-size: 11pt;'>OBSERVED-FORM</span>)"
    # y = TeX("$\\Delta_{SMOLM}$ (95% CI)")
    y = "Avg. &Delta; (95% CI)"
  ) +
  guides(color="none", shape="none", fill="none")

# nabanana_results %>%
  # filter(dative == "do") %>%

# plot

nabanana_results %>% 
  group_by(dative, behavior) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(diff),
    delta = mean(diff)
  ) %>%
  mutate(
    dative = glue::glue("{str_to_upper(dative)} verbs"),
    behavior = str_to_upper(behavior)
  ) %>%
  ggplot(aes(behavior, delta, color = behavior, shape = dative)) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = delta-ste, ymax = delta+ste)) + 
  scale_y_continuous(breaks = seq(-0.90, 0.50, by = 0.05)) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~dative, scales = "free_y") +
  theme_bw(base_size = 17, base_family = "CMU Serif") + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color="black"),
    axis.title.y = element_markdown(),
  ) +
  labs(
    x = "Alternation Behavior",
    # y = TeX("$\\log\\frac{\\textit{p}(ALT-FORM)}{p()}$")
    # y = TeX("$\\Lambda$")
    # y = "log <i>p</i>(<span style='font-size: 11pt;'>ALT-FORM</span>) - log <i>p</i>(<span style='font-size: 11pt;'>OBSERVED-FORM</span>)"
    # y = TeX("$\\Delta_{SMOLM}$ (95% CI)")
    y = "Avg. &Delta; (95% CI)"
  ) +
  guides(color="none", shape="none")

ggsave("paper/nabanana-mean.pdf", width=9.84,height=4.36, dpi=300,device=cairo_pdf)


plot(TeX(r'(A $\LaTeX$ formula: $\frac{2hc^2}{\lambda^5}\frac{1}{e^{\frac{hc}{\lambda k_B T}} - 1}$)'), cex=2, main="")
  
nabanana_results %>% 
  group_by(behavior) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(diff),
    delta = mean(diff)
  )

nabanana_results %>%
  ggplot(aes(diff, color = behavior, fill = behavior)) +
  geom_density(alpha = 0.2)



naba_do <- results %>%
  filter(behavior == "naba", dative == "do") %>%
  pull(diff)

nana_do <- results %>%
  filter(behavior == "nana", dative == "do") %>%
  pull(diff)

naba_pp <- results %>%
  filter(behavior == "naba", dative == "pp") %>%
  pull(diff)

nana_pp <- results %>%
  filter(behavior == "nana", dative == "pp") %>%
  pull(diff)

nanas <- results %>%
  filter(behavior == "nana") %>%
  pull(diff)

nabas <- results %>%
  filter(behavior == "naba") %>%
  pull(diff)

# t-tests, across datives, and overall
t.test(naba_do, nana_do)
t.test(naba_pp, nana_pp)
t.test(nabas, nanas)

# lmers, accounting for seed variation

coded <- nabanana_results %>%
  mutate(
    verb = factor(verb),
    seed = factor(seed),
    # behavior = case_when(
    #   behavior == "naba" ~ 1,
    #   TRUE ~ 0
    # )
  )

fit <- lmer(diff ~ behavior + (1 + behavior | seed), data = coded)

summary(fit)

fit_do <- lmer(diff ~ behavior + (1 + behavior | seed), 
               data = coded %>% filter(dative == "do"))

summary(fit_do)

fit_pp <- lmer(diff ~ behavior + (1 | seed), 
               data = coded %>% filter(dative == "pp"))

summary(fit_pp)

## alt-diff for naba > nana
## smolms are more likely to accept alt-forms for nabas than they are for nanas.
## beta = 0.459 (this is the same as the avg. diff between altdiff_naba and altdiff_nana)
## p < .0001, accounts for random effects of the model-seeds
## confirmed also using t-tests

nabanana_results %>%
  # select(verb, behavior, dative, do_score, pp_score) %>%
  select(-diff, -do, -pp) %>%
  pivot_longer(do_score:pp_score, names_to = "target", values_to = "score") %>%
  group_by(verb, behavior, dative, target) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(score),
    score = mean(score)
  ) %>%
  ungroup() %>%
  mutate(
    verb_stats = glue::glue("{verb}: {behavior}-{dative}"),
    target_class = case_when(
      dative == str_remove(target, "_score") ~ "observed",
      TRUE ~ "alt"
    )
  ) %>%
  ggplot(aes(target_class, score)) +
  geom_point() + 
  facet_wrap(~ verb_stats)

nabanana_results %>%
  # select(verb, behavior, dative, do_score, pp_score) %>%
  select(-diff, -do, -pp) %>%
  pivot_longer(do_score:pp_score, names_to = "target", values_to = "score") %>%
  group_by(verb, behavior, dative, target) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(score),
    score = mean(score)
  ) %>%
  ungroup() %>%
  mutate(
    verb_stats = glue::glue("{verb}: {behavior}-{dative}"),
    target_class = case_when(
      dative == str_remove(target, "_score") ~ "observed",
      TRUE ~ "alt"
    ),
    behavior_stat_chr = factor(
      glue::glue("{behavior}-{dative}"),
      levels = c("nana-do", "nana-pp", "naba-do", "naba-pp")
    ),
    behavior_stat = case_when(
      glue::glue("{behavior}-{dative}") == "nana-do" ~ 1,
      glue::glue("{behavior}-{dative}") == "nana-pp" ~ 2,
      glue::glue("{behavior}-{dative}") == "naba-do" ~ 3,
      TRUE ~ 4
    ),
    verb = fct_reorder(factor(verb), behavior_stat)
    # verb = reorder(verb, behavior_stat)
  ) %>%
  ggplot(aes(verb, score, group = target, color = behavior_stat_chr, shape = target_class)) +
  geom_point(size = 2) +
  theme_bw(base_size = 15, base_family = "Times") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
    legend.position = "top",
    axis.text.y = element_text(color = "black") 
  )



nabanana_results %>%
  group_by(verb, behavior, dative) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(diff),
    diff = mean(diff)
  ) %>%
  ungroup() %>%
  inner_join(nabanana_counts) %>%
  mutate(
    verb_stats = glue::glue("{verb}: {behavior}-{dative}"),
    behavior_stat_chr = factor(
      glue::glue("{behavior}-{dative}"),
      levels = c("nana-do", "nana-pp", "naba-do", "naba-pp")
    ),
    behavior_stat = case_when(
      glue::glue("{behavior}-{dative}") == "nana-do" ~ 1,
      glue::glue("{behavior}-{dative}") == "naba-do" ~ 2,
      glue::glue("{behavior}-{dative}") == "nana-pp" ~ 3,
      TRUE ~ 4
    ),
    verb = fct_reorder(factor(verb), behavior_stat),
    verb_name = glue::glue("{verb} ({n})"),
    verb_name = fct_reorder(factor(verb_name), behavior_stat)
  ) %>%
  ggplot(aes(verb_name, diff, color = behavior, shape = dative)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = diff - ste, ymax = diff + ste)) +
  theme_bw(base_size = 15, base_family = "Times") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
    legend.position = "top",
    axis.text.y = element_text(color = "black") 
  )
  
  
