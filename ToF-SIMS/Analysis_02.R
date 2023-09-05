# FOR ANALYTICAL CHEMISTRY
# --------------------------------------------------------------------------------
# TASK  01: MORE CONCISE PLOT OF VOTING
# TASK  02: DO PREFMOD PLOTTING
# --------------------------------------------------------------------------------


# --------------------------------------------------------------------------------
# TASK  01: MORE CONCISE PLOT OF VOTING
# --------------------------------------------------------------------------------
library(ggplot2)
library(viridis)
library(gridExtra)
library(stringr)
library(dplyr)
library(tidyr)

survey_filename <- "Survey_Results_06_13.csv" 
datori <- read.csv(survey_filename)
dat <- datori
colset2 <- colnames(dat)[6:14] %>%
  as_tibble() %>%
  rename(colN = value) 
colset3 <- substr(colset2$colN, 3, 20) %>%
  as_tibble() 
colset3[9,1] <- "Familiarity..A"
colnames <- colset3 %>% 
  separate_wider_delim(value,  
                       delim="..", 
                       names = c('first', 'last')) %>%
  select(first) %>%
  pull()
colnames(dat)[6:14] <- colnames 
survey_options <- read.csv("Survey_Options.csv")
survey_options <- survey_options[-5, ]
quest_names <- data.frame(Question = 1:8, Qname = colnames[-9])
quest_names$Qname <- stringr::str_replace(quest_names$Qname, "Capacitor", "Resistor")

Qnames <- paste(colnames[-9],c(rep("Retain", 4), rep("Remove", 4)), sep="")
Qnames <- stringr::str_remove_all(Qnames,'[0123456789]' )
stringr::str_replace(Qnames,'..', "-" )
quest_names$Qname <- Qnames

for(qq in 1:8){
  quest <- qq
  question <- dat[ ,(5 + quest)]
  gtitle <- colnames(dat)[5 + quest]
  if(quest == 4){
    gtitle <- stringr::str_replace(gtitle, "Capacitor", "Resistor")
  }
  sur <- survey_options[quest, c(1,3:9)] %>%
    pivot_longer(cols = 2:8 ) %>%
    rename(option = name, method = value)
  
  question
  q <- question
  
  for(rank in 1:7){
    pos <- 2*(rank - 1) + 1
    qi <- str_sub(q, pos, pos)
    df <- tibble(option = qi) %>%
      left_join(sur) %>%
      mutate(rank = rank)
    if(rank == 1){
      dfq <- df
    }else{
      dfq <- bind_rows(dfq, df)
    }
  }
  
  dfq <- dfq %>%
    mutate(Rank = as.factor(rank)) %>%
    mutate(across('method', str_replace, 'SeamlessStiching', 'SeamlessStitching')) %>%
    mutate(across('method', str_replace, 'LinearMulti', 'Linear_Multi')) %>%
    mutate(across('method', str_replace, 'Original', 'Control(Original)'))
  
  if(qq == 1){
    dfq_all <- dfq
  }else{
    dfq_all <- bind_rows(dfq_all, dfq)
  }
}

dfq_all2 <- full_join(dfq_all, quest_names)
positions <- c("Interpolation", "Averaging", "LDA", "Tensor", "Linear_Multi", "SeamlessStitching", "Control(Original)")


g1 <- ggplot(dfq_all2, aes(x = method, fill = Rank)) +
  geom_bar(position = 'stack') + 
  scale_fill_viridis(discrete = T, direction = -1) + 
  ylab("Votes") + 
  xlab("") + 
  scale_x_discrete(limits = positions)+
  facet_wrap(~Qname, nrow = 2) + 
  coord_flip() 
g1
# 7 x 3.5 inches

# --------------------------------------------------------------------------------
# TASK  02: DO PREFMOD PLOTTING
# --------------------------------------------------------------------------------
library(ggplot2)
worth_all <- read.csv("Prefmod_coefficients.csv")
worth <- worth_all %>%
  rename(F1_Low = F1, F2_High = F2) %>%
  mutate(data = rep(c(rep("Dye", 7), rep("Brain", 7), rep("Polymer", 7), rep("Resistor", 7) ),2),
         Q = c(rep('Retain', 28), rep('Remove', 28))) %>%
  mutate(across('Method', str_replace, 'Original', 'Control(Original)')) %>% 
  relocate(data, filename, Q, Method, F1_Low, F2_High) 
worthlng <- worth %>%
  pivot_longer(cols = 5:6) %>%
  rename(Familiarity = name, Worth = value) %>%
  mutate(fam = ifelse(Familiarity == 'Fam_1_Low', 1, 2))
positions <- rev(c("Interpolation", "Averaging", "LDA", "Tensor", "Linear_Multi", "SeamlessStitching", "Control(Original)"))


g1 <- ggplot(worthlng, aes(x = Method, 
                     y = Worth, 
                     shape = Q, 
                     color = data, 
                   size = Familiarity)) + 
  geom_point() + 
  scale_x_discrete(limits = positions)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g1
#scale_size("Familiarity",breaks=c(1,1.1),labels=c('Low','High')) + 

# By Remove/Retain
# ggplot(worthlng, aes(x = Method,
#                      y = Worth,
#                      shape = Familiarity,
#                      color = data)) +
#   geom_point() +
#   facet_wrap(~Q) +
#   scale_color_viridis_d() + # option = 'inferno'
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# By Familiarity
# ggplot(worthlng, aes(x = Method,
#                      y = Worth,
#                      shape = Q,
#                      color = data)) +
#   geom_point() +
#   facet_wrap(~Familiarity) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


g2 <- ggplot(worthlng, aes(x = Method, 
                     y = Worth, 
                     shape = Familiarity, 
                     color = Q
                     )) + 
  geom_point() + 
  facet_wrap(~data, nrow = 1) + 
  scale_x_discrete(limits = positions) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
gridExtra::grid.arrange(g1, g2, nrow = 1, widths = c(1, 1.5))
# g2
# 7.5 x 4.5 inches