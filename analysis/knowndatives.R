library(tidyverse)
library(jsonlite)

# stream_in(file("data/results/real_verbs/smolm_autoreg_bpe-seed_111/logprobs.jsonl"))
real_logprobs <- fs::dir_ls("data/results/real_verbs/", recurse = TRUE, regexp = "*.jsonl") %>%
  map_df(function(x) {stream_in(file(x))}, .id = "model") %>%
  as_tibble() %>%
  mutate(
    model = str_extract(model, "(?<=verbs/)(.*)(?=/logprobs.jsonl)")
  )

generalization <- stream_in(file("data/experiments/single_stimuli_dative_simulation/generalization.jsonl")) %>%
  as_tibble()

real_logprobs %>%
  # filter(str_detect(sentence, " the ")) %>%
  # filter(lemma == "kick") %>%
  filter(lemma != "carry") %>%
  inner_join(generalization %>% filter(theme_animacy == "inanimate") %>% select(-sentence)) %>%
  group_by(model, lemma, dative) %>%
  summarize(
    logprob = mean(score)
  ) %>% 
  ungroup() %>%
  pivot_wider(names_from = dative, values_from = logprob) %>% 
  inner_join(proportion_distributions %>% select(lemma, dative, type, empirically_alternating)) %>%
  distinct() %>% 
  ungroup() %>% 
  filter(type != "do-only", dative == "pp") %>%
  mutate(
    supertype = case_when(
      empirically_alternating == "Empirically Alternating" & type == "alternating" ~ "EA+A",
      empirically_alternating == "Empirically Non-alternating" & type == "alternating" ~ "ENAbA",
      empirically_alternating == "Empirically Non-alternating" & type == "pp-only" ~ "ENA+NA",
    )
  ) %>% 
  ggplot(aes(supertype, do)) +
  stat_summary(aes(group = model), fun = mean, geom="point")

real_logprobs %>%
  select(-sentence) %>%
  pivot_wider(names_from = dative, values_from = score) %>% 
  inner_join(proportion_distributions %>% distinct(lemma, type, dative, empirically_alternating) %>% filter(dative == "pp")) %>%
  group_by(lemma, type, empirically_alternating) %>%
  summarize(
    logprob = mean(do, na.rm = TRUE),
    ste = 1.96 * plotrix::std.error(do, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    supertype = case_when(
      empirically_alternating == "Empirically Alternating" & type == "alternating" ~ "EA+A",
      empirically_alternating == "Empirically Non-alternating" & type == "alternating" ~ "ENAbA",
      empirically_alternating == "Empirically Non-alternating" & type == "pp-only" ~ "ENA+NA",
    ),
    supertype = factor(supertype, levels = c("ENAbA", "ENA+NA", "EA+A"))
  ) %>%
  ggplot(aes(lemma, logprob, fill = supertype, color = supertype)) +
  geom_col() +
  facet_wrap(~supertype, scales="free_x") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
    legend.position = "top",
    axis.text.y = element_text(color = "black") 
  )
  

pos2cat <- tribble(
  ~pos,~category,
  "NN","NP",
  "NNS","NP",
  "CD","NP",
  "JJ","NP",
  "JJR","NP",
  "NNP","Proper Noun",
  "NNPS","Proper Noun",
  "PRP","Pronoun",
  "DT","DT"
)

alternating_verbs = "feed,give,lease,lend,loan,pass,pay,peddle,refund,render,rent,repay,sell,serve,trade,advance,allocate,allot,assign,award,bequeath,cede,concede,extend,grant,guarantee,issue,leave,offer,owe,promise,vote,will,yield,bring,take,forward,hand,mail,post,send,ship,slip,smuggle,sneak,bounce,float,roll,slide,carry,drag,haul,heave,heft,hoise,kick,lug,pull,push,schlep,shove,tote,tow,tug,barge,bus,cart,drive,ferry,fly,row,shuttle,truck,wheel,wire,bash,bat,bunt,catapult,chuck,flick,fling,flip,hit,hurl,kick,lob,pass,pitch,punt,shoot,shove,slam,slap,sling,throw,tip,toss,ask,cite,pose,preach,quote,read,relay,show,teach,tell,write,cable,email,e-mail,fax,modem,netmail,phone,radio,relay,satellite,semaphore,sign,signal,telephone,telecast,telegraph,telex,wire,wireless" %>% 
  str_split(",", simplify=TRUE) %>%
  .[1,] %>%
  unique()

do_only_verbs = "accord,ask,bear,begrudge,bode,cost,deny,envy,flash,forbid,forgive,guarantee,issue,refuse,save,spare,strike,vouchsafe,wish,write,bet,bill,charge,fine,mulct,overcharge,save,spare,tax,tip,undercharge,wager,acknowledge,adopt,appoint,consider,crown,deem,designate,elect,esteeem,imagine,mark,nominate,ordain,proclaim,rate,recon,report,want,anoint,baptize,brand,call,christen,consecrate,crown,decree,dub,label,make,name,nickname,pronounce,rule,stamp,style,term,vote,adjudge,adjudicate,assume,avow,believe,confess,declare,fancy,find,judge,presume,profess,prove,suppose,think,warrant" %>% 
  str_split(",", simplify = TRUE) %>%
  .[1,] %>%
  unique()

pp_only_verbs = "address,administer,broadcast,convey,contribute,delegate,deliver,denounce,demonstrate,describe,dictate,dispatch,display,distribute,donate,elucidate,exhibit,express,explain,explicate,forfeit,illustrate,introduce,narrate,portray,proffer,recite,recommend,refer,reimburse,remit,restore,return,sacrifice,submit,surrender,transfer,transport,admit,allege,announce,articulate,assert,communicate,confess,convey,declare,mention,propose,recount,repeat,report,reveal,say,state,babble,bark,bawl,bellow,bleat,boom,bray,burble,cackle,call,carol,chant,chatter,chrip,cluck,coo,croak,croon,crow,cry,drawl,drone,gabble,gibber,groan,growl,grumble,grunt,hiss,holler,hoot,howl,jabber,lilt,lisp,moan,mumble,murmur,mutter,purr,rage,rasp,roar,rumble,scream,screech,shout,shriek,sing,snap,snarl,snuffle,splutter,squall,squawk,squeak,squeal,stammer,stutter,thunder,tisk,trill,trumpet,twitter,wail,warble,wheeze,whimper,whine,whisper,whistle,whoop,yammer,yap,yell,yelp,yodel,drop,hoist,lift,lower,raise,credit,entrust,furnish,issue,leave,present,provide,serve,supply,trust" %>% 
  str_split(",", simplify=TRUE) %>%
  .[1,] %>%
  unique()

verb_types <- tibble(
  lemma = c(alternating_verbs, do_only_verbs, pp_only_verbs),
  type = c(rep("alternating", length(alternating_verbs)), rep("do-only", length(do_only_verbs)), rep("pp-only", length(pp_only_verbs)))
) %>%
  add_count(lemma) %>%
  mutate(
    type = case_when(
      n > 1 ~ "alternating",
      TRUE ~ type
    )
  ) %>%
  select(-n) %>%
  distinct()

verb_types %>% count(lemma, sort = TRUE) %>% filter(n > 1)

dos <- read_csv("data/aochildes_dos.csv")
pps <- read_csv("data/aochildes_pps.csv")

pps_filtered <- pps %>%
  filter(
    theme_pos %in% c("NN", "NNP", "NNS", "PRP", "NNPS", "DT", "CD", "JJ", "JJR") &
      recipient_pos %in% c("NN", "NNS", "NNP", "NNPS", "PRP")
  ) %>%
  inner_join(verb_types) %>%
  filter(type != "do-only")

dos_filtered <- dos %>%
  filter(
    theme_pos %in% c("NN", "NNP", "NNS", "PRP", "NNPS", "DT", "CD", "JJ", "JJR") &
      recipient_pos %in% c("NN", "NNS", "NNP", "NNPS", "PRP")
  ) %>% inner_join(verb_types) %>%
  filter(type != "pp-only")

combined <- bind_rows(
  pps_filtered %>% mutate(dative = "pp"),
  dos_filtered %>% mutate(dative = "do")
)

combined %>%
  count(lemma, sort=TRUE) %>%
  write_csv("data/dative_lemmas.csv")

combined |>
  count(dative, verb_pos) %>%
  group_by(dative) %>%
  mutate(proportion = n/sum(n))

combined %>% 
  mutate(question = str_detect(sentence, "\\?")) %>% 
  count(dative, question, verb_pos) %>% 
  group_by(dative,  verb_pos) %>% 
  mutate(proportion = n/sum(n))

distributions <- combined %>%
  # filter(type == "alternating") %>%
  count(lemma, type, dative) %>%
  group_by(lemma, type) %>%
  mutate(
    total = sum(n)
  ) %>%
  group_by(dative, .add=TRUE) %>%
  mutate(
    proportion = n/total,
  ) %>%
  ungroup()

proportion_distributions <- distributions %>%
  mutate(
    lemma = fct_reorder2(lemma, type, proportion),
    # lemma = fct_reorder(lemma, proportion)
    empirically_alternating = case_when(
      proportion != 1.0 ~ "Empirically Alternating",
      TRUE ~ "Empirically Non-alternating"
    )
  )

proportion_distributions %>%
  ggplot(aes(lemma, proportion, fill = dative, color = dative)) +
  geom_col(position = "fill") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  theme_bw(base_family = "Times", base_size = 13) +
  scale_x_discrete(expand = c(0.01,0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  scale_color_brewer(aesthetics = c("color", "fill"), palette = "Blues") +
  facet_wrap(~empirically_alternating, ncol = 1, scales="free_x") +
  # scale_color_viridis_d(aesthetics = c("color", "fill")) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
    legend.position = "top",
    axis.text.y = element_text(color = "black") 
  ) +
  labs(
    color = "Construction",
    fill = "Construction",
    x = "Verb Lemma",
    y = "Proportion"
  )

# 7.73, 5.3

distributions %>%
  filter(type == "alternating") %>%
  filter(proportion == 1)

combined %>%
  mutate(
    post_verbal = case_when(
      dative == "pp" ~ theme_pos,
      TRUE ~ recipient_pos
    )
  ) %>%
  inner_join(pos2cat %>% rename(post_verbal=pos)) %>%
  count(dative, category) %>%
  group_by(dative) %>%
  mutate(proportion = n/sum(n)) %>%
  ungroup() %>%
  mutate(
    category = fct_reorder(category, proportion) %>% fct_rev(),
    dative = str_to_upper(dative)
  ) %>%
  ggplot(aes(proportion, dative, fill = category, color = category)) +
  geom_col(position="fill") +
  geom_text(aes(label = round(proportion*100, 2)), position = position_stack(vjust=0.5), color = "white", family="Times") +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
  scale_x_continuous(labels=scales::percent_format(), expand = c(0.015,0)) +
  # scale_x_discrete() +
  # scale_y_discrete(expand = c(0, 0.1)) +
  theme_bw(base_size = 15, base_family = "Times") +
  theme(
    legend.position = "top",
    axis.text = element_text(color = "black")
  ) +
  labs(
    color = "Category",
    fill = "Category",
    x = "Proportion",
    y = "Dative"
  )
# 6.17, 3.04

combined %>%
  select(lemma, theme_pos, recipient_pos, type, dative) %>%
  pivot_longer(theme_pos:recipient_pos, names_to="argument", values_to="pos") %>%
  inner_join(pos2cat) %>%
  inner_join(
    proportion_distributions %>% 
      distinct(lemma, empirically_alternating)
  ) %>%
  count(empirically_alternating, dative, argument, category) %>%
  View()

combined %>%
  select(lemma, theme_pos, recipient_pos, type, dative) %>%
  inner_join(pos2cat %>% select(theme_pos = pos, theme_category=category)) %>%
  inner_join(pos2cat %>% select(recipient_pos = pos, recipient_category=category)) %>%
  inner_join(
    proportion_distributions %>% 
      distinct(lemma, empirically_alternating)
  ) %>%
  count(empirically_alternating, lemma, dative, theme_category, recipient_category) %>% View()
  pivot_wider(names_from = dative, values_from = n, values_fill = 0) %>% 
  mutate(
    ratio = case_when(
      pp == 0 | do == 0 ~ 0,
      TRUE ~ do/pp
    ),
    proportion = do/(do+pp),
    # product = pp + do
    min_metric = map2_int(do, pp, function(x, y){return(min(x, y))}),
    diff_ratio = abs(pp - do)/(pp+do)
  ) %>% 
  group_by(empirically_alternating, theme_category, recipient_category) %>%
  summarize(
    n = n(),
    min_metric = mean(min_metric),
    diff_ratio = mean(diff_ratio)
  ) %>% View()

pos2cat2 <- tribble(
  ~pos,~category,
  "NN","NP",
  "NNS","NP",
  "CD","NP",
  "JJ","NP",
  "JJR","NP",
  "NNP","NP",
  "NNPS","NP",
  "PRP","Pronoun",
  "DT","DT"
)

combined %>%
  count(dative, recipient, sort=TRUE)

combined %>%
  select(lemma, theme_pos, recipient_pos, type, dative) %>%
  inner_join(pos2cat2 %>% select(theme_pos = pos, theme_category=category)) %>%
  inner_join(pos2cat2 %>% select(recipient_pos = pos, recipient_category=category)) %>%
  inner_join(
    proportion_distributions %>% 
      distinct(lemma, empirically_alternating)
  ) %>%
  count(empirically_alternating,dative, theme_category, recipient_category) %>% View("aggregated")


combined %>%
  select(lemma, theme_pos, recipient_pos, type, dative) %>%
  inner_join(pos2cat2 %>% select(theme_pos = pos, theme_category=category)) %>%
  inner_join(pos2cat2 %>% select(recipient_pos = pos, recipient_category=category)) %>%
  inner_join(
    proportion_distributions %>% 
      distinct(lemma, empirically_alternating)
  ) %>%
  count(empirically_alternating, lemma, dative, theme_category, recipient_category) %>%
  inner_join(
    combined %>% 
      count(lemma, dative) %>%
      mutate(other_dative = case_when(dative == "do" ~ "pp", TRUE ~ "do")) %>%
      select(lemma, dative = other_dative, n_other = n)
  ) %>%
  group_by(
    empirically_alternating, dative, theme_category, recipient_category
  ) %>%
  summarize(
    config_freq = sum(n),
    alternating_freq = sum(n_other),
    metric = abs(0.5 - config_freq/(alternating_freq+config_freq))
  ) %>%
  ungroup()

combined %>%
  select(lemma, theme_pos, recipient_pos, type, dative) %>%
  inner_join(pos2cat2 %>% select(theme_pos = pos, theme_category=category)) %>%
  inner_join(pos2cat2 %>% select(recipient_pos = pos, recipient_category=category)) %>%
  inner_join(
    proportion_distributions %>% 
      distinct(lemma, empirically_alternating)
  ) %>%
  count(empirically_alternating, lemma, dative, theme_category, recipient_category)

combined %>%
  inner_join(proportion_distributions) %>%
  select(sentence, lemma, theme_pos, recipient_pos, dative, type, empirically_alternating) %>%
  inner_join(pos2cat2 %>% select(theme_pos = pos, theme_category=category)) %>%
  inner_join(pos2cat2 %>% select(recipient_pos = pos, recipient_category=category)) %>%
  count(empirically_alternating, dative, theme_category, recipient_category) %>% View()
# proportion_distributions
