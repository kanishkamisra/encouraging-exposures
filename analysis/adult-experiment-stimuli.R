library(tidyverse)

haap <- read_csv("data/haaps.csv") %>%
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

haap_annotated %>%
  count(experiment_id, theme_length, recipient_length, score) %>% View()


haap_annotated %>%
  filter(!experiment_id == "ignore") %>% 
  select(
    experiment_id, idx, dative, givenness_template, hypothesis_id, hypothesis_item, template_type, template, stimulus, agent, theme, recipient,
    theme_animacy, recipient_animacy, theme_pronominality, recipient_pronominality, theme_definiteness, recipient_definiteness,
    theme_givenness, recipient_givenness, theme_length, recipient_length, binary_score = code_score, length_score, haap_score
  ) %>%
  separate(stimulus, into = c("context", "utterance"), sep = "<s>") %>%
  write_csv("data/haap-annotated-human-exps.csv")

  