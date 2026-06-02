library(tidyverse)

haap <- read_csv("data/haaps-w-lm-results.csv") %>%
  mutate(
    theme_length = str_count(theme, " ") + 1,
    recipient_length = str_count(recipient, " ") + 1
  )

haap %>%
  filter(length_diff==theme_length-recipient_length)

haap_annotated <- haap %>%
  filter(theme_animacy=="inanimate", recipient_animacy=="animate") %>%
  mutate(
    experiment_id = case_when(
      dative == "PO" & theme_pronominality=="pronoun" & theme_definiteness=="definite" & 
        theme_givenness == "given" & theme_length == 1 & 
        recipient_pronominality=="noun" & recipient_definiteness=="indefinite" & 
        recipient_givenness == "new" & recipient_length %in% c(4,5,6) ~ "HAAP-max_PO2DO",
      # --
      dative == "PO" & recipient_pronominality=="pronoun" & recipient_definiteness=="definite" & 
        recipient_givenness == "given" & recipient_length == 1 & 
        theme_pronominality=="noun" & theme_definiteness=="indefinite" & 
        theme_givenness == "new" & theme_length %in% c(4,5,6) ~ "HAAP-min_PO2DO",
      # --
      dative == "PO" & theme_pronominality=="pronoun" & theme_definiteness=="definite" & 
        theme_givenness == "given" & theme_length == 1 & 
        recipient_pronominality=="pronoun" & recipient_definiteness=="definite" & 
        recipient_givenness == "given" & recipient_length == 1 ~ "HAAP-1pv_PO2DO",
      # --
      dative == "PO" & theme_pronominality=="noun" & theme_definiteness=="indefinite" & 
        theme_givenness == "new" & theme_length %in% c(2,3,4) & 
        recipient_pronominality=="noun" & recipient_definiteness=="indefinite" & 
        recipient_givenness == "new" & recipient_length %in% c(2,3,4) &
        theme_length == recipient_length ~ "HAAP-2pv_PO2DO",
      # --
      dative == "DO" & recipient_pronominality=="pronoun" & recipient_definiteness=="definite" & 
        recipient_givenness == "given" & recipient_length == 1 & 
        theme_pronominality=="noun" & theme_definiteness=="indefinite" & 
        theme_givenness == "new" & theme_length %in% c(4,5,6) ~ "HAAP-max_DO2PO",
      # --
      dative == "DO" & theme_pronominality=="pronoun" & theme_definiteness=="definite" & 
        theme_givenness == "given" & theme_length == 1 & 
        recipient_pronominality=="noun" & recipient_definiteness=="indefinite" & 
        recipient_givenness == "new" & recipient_length %in% c(4,5,6) ~ "HAAP-min_DO2PO",
      # --
      dative == "DO" & recipient_pronominality=="pronoun" & recipient_definiteness=="definite" & 
        recipient_givenness == "given" & recipient_length == 1 & 
        theme_pronominality=="pronoun" & theme_definiteness=="definite" & 
        theme_givenness == "given" & theme_length == 1 ~ "HAAP-1pv_DO2PO",
      # --
      dative == "DO" & recipient_pronominality=="noun" & recipient_definiteness=="indefinite" & 
        recipient_givenness == "new" & recipient_length %in% c(2,3,4) & 
        theme_pronominality=="noun" & theme_definiteness=="indefinite" & 
        theme_givenness == "new" & theme_length %in% c(2,3,4) &
        theme_length == recipient_length~ "HAAP-2pv_DO2PO",
      TRUE ~ "ignore"
    )
  ) 

agg <- haap_annotated %>%
  mutate(
    altform = case_when(
      dative == "DO" ~ pp,
      TRUE ~ do
    )
  ) %>%
  group_by(dative) %>%
  mutate(
    altform = scale(altform)
  ) %>%
  ungroup() %>%
  group_by(dative, experiment_id) %>%
  summarize(
    n = n(),
    sd = sd(altform),
    cb = qt(0.05/2, n-1, lower.tail = FALSE) * sd/sqrt(n),
    altform = mean(altform)
  ) 

agg %>%
  ggplot(aes(experiment_id, altform)) +
  geom_point() +
  geom_linerange(aes(ymin = altform-cb, ymax = altform+cb)) +
  facet_wrap(~dative, scale = "free_x") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

agg %>%
  filter(str_detect(experiment_id, "min|max")) %>%
  separate(experiment_id, into = c("haap_type", "exp"), sep = "_") %>%
  mutate(
    exp = case_when(
      exp == "DO2PO" ~ "DO \u2192 PO",
      TRUE ~ "PO \u2192 DO"
    ),
    info_struct = case_when(
      haap_type == "HAAP-max" ~ "Expected",
      TRUE ~ "Violated"
    ),
    # info_struct = factor(info_struct, levels = c("Same", "Opposite"))
  ) %>%
  ggplot(aes(info_struct, altform, group = exp, color = info_struct)) +
  geom_point() +
  geom_errorbar(aes(ymin = altform-cb, ymax = altform+cb), width = 0.2) +
  # geom_line() +
  # geom_ribbon(aes(ymin = altform-cb, ymax = altform+cb), color = NA, alpha = 0.2) +
  facet_wrap(~exp, scales = "free") +
  scale_color_manual(values = c("#f1c232", "#cc4125")) +
  ggh4x::facetted_pos_scales(
    y = list(
      exp == "DO \u2192 PO" ~ scale_y_continuous(limits = c(-0.44, 0.44), breaks = scales::pretty_breaks()),
      exp == "PO \u2192 DO" ~ scale_y_continuous(limits = c(-0.44, 0.44), breaks = scales::pretty_breaks())
    )
  ) +
  theme_classic(base_size = 16, base_family = "DM Sans") +
  theme(
    strip.background = element_blank(),
    # strip.text.x = element_markdown(face = "bold", family = "DM Sans"),
    strip.text.x = element_text(face = "bold"),
    legend.position = "none"
  ) +
  labs(
    x = "Information Structure of Exposure",
    y = "How likely does the model\nfind the alternate form?"
  )



showtext::showtext_opts(dpi = 300)
ggsave("slides/infostructure-minmax-plot.png", height = 3.93, width = 8.20, dpi = 300, type = "cairo")
  
