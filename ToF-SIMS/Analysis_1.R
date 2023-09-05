# --------------------------------------------------------------------------------
# SURVEY RESULTS INCLUDED IN THE PAPER
# --------------------------------------------------------------------------------

# --------------------------------------------------------------------------------
# TASK  01: PLOT GRAPH TO SHOW THE FAMILIARITY
# TASK  02: PLOT GRAPHS TO SHOW RANKINGS
# TASK  03: TABLE OF AVERAGE RANKINGS
# TASK  04: MORE CONCISE PLOT OF VOTING
# TASK  05: prefmod ANALYSIS
# --------------------------------------------------------------------------------

# --------------------------------------------------------------------------------
# TASK  01: PLOT GRAPH TO SHOW THE FAMILIARITY
# --------------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# NOTE: CHANGE FOLDER AS REQUIRED
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
head(dat)

ggplot(dat, aes(x = Familiarity))  + 
  geom_bar() + 
  scale_x_continuous(breaks=seq(1, 10, 1)) + 
  ylab("Count") + 
  theme_bw() 

# --------------------------------------------------------------------------------
# TASK  02: PLOT GRAPHS TO SHOW RANKINGS
# --------------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(tidyr)

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
survey_options <- survey_options[-5, ]  # remove blank line


# ---------------------------------------------------
# PLOT GRAPHS
library(ggplot2)
library(viridis)
library(gridExtra)
library(stringr)

qq <- 4
filenames <- c("Dye_127_Survey.pdf", "Brain_1838_Survey.pdf", "Polymer_23_Survey.pdf", "Resistor_35_Survey.pdf" )
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

positions <- c("Original", "Interpolation", "Averaging", "LDA", "Tensor", "Linear_Multi", "SeamlessStitching")
dfq <- dfq %>%
  mutate(Rank = as.factor(rank)) %>%
  mutate(across('method', str_replace, 'SeamlessStiching', 'SeamlessStitching')) %>%
  mutate(across('method', str_replace, 'LinearMulti', 'Linear_Multi'))
g1 <- ggplot(dfq, aes(x = method, fill = Rank)) +
  geom_bar(position = 'stack') + 
  scale_fill_viridis(discrete = T, direction = -1) + 
  xlab(gtitle) + 
  ylab("Count") + 
  theme(legend.position = "none") + 
  scale_x_discrete(limits = positions)+
  coord_flip() 
  
#  scale_color_hue(direction = -1, h.start=90) #
#   scale_fill_brewer(palette = 'Dark2') + 
g1 


quest <- quest + 4
question <- dat[ ,(5 + quest)]
gtitle <- colnames(dat)[5 + quest]
if(quest == 8){
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
  mutate(across('method', str_replace, 'LinearMulti', 'Linear_Multi'))

g2 <- ggplot(dfq, aes(x = method, fill = Rank)) +
  geom_bar(position = 'stack') + 
  scale_fill_viridis(discrete = T, direction = -1) + 
  xlab(gtitle) + 
  ylab("Count") + 
  scale_x_discrete(limits = positions)+
  coord_flip() 
g2 

pdf(file = paste(filenames[qq], sep=""),   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 2)

gridExtra::grid.arrange(g1, g2, nrow = 1, widths = c(1,1.2))
# saved as Resistor_35_Survey.pdf or similar
# dim 7 x 2 landscape
dev.off()

# --------------------------------------------------------------------------------
# TASK  03: TABLE OF AVERAGE RANKINGS
# --------------------------------------------------------------------------------


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
colnames(dat)[6:14] <-  colnames 

survey_options <- read.csv("Survey_Options.csv")
survey_options <- survey_options[-5, ]


for(qq in 1:8){
  quest <- qq
  question <- dat[ ,(5 + quest)]
  gtitle <- colnames(dat)[5 + quest]
  if(quest == 4 | quest == 8){
    gtitle <- stringr::str_replace(gtitle, "Capacitor", "Resistor")
  }
  sur <- survey_options[quest, c(1,3:9)] %>%
    pivot_longer(cols = 2:8 ) %>%
    rename(option = name, Method = value) %>%
    mutate(method = str_sub(Method, 1, 3))
  
  len <- length(question)
  q <- as.data.frame(question)
  rankdf <- q %>% separate_wider_delim(question, delim =';', names = c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8"))
  rankdf <- rankdf[ ,-8]
  rankdf <- rankdf %>%
    mutate(id = 1:dim(rankdf)[1]) %>% 
    relocate(id, .before=R1) 
  
  colnames(rankdf)[-1]<- 1:7
  rankdflng <- rankdf %>%
    pivot_longer(cols = 2:8)
  rankdf2 <- rankdflng %>%
    pivot_wider(names_from = value,
                values_from = name) %>%
    relocate(id, A, B, C, D, E, F, G) %>%
    select(-id) %>% 
    mutate(A = as.integer(A), 
           B = as.integer(B),
           C = as.integer(C),
           D = as.integer(D),
           E = as.integer(E),
           F = as.integer(F),
           G = as.integer(G))
  
  colnames(rankdf2) <- pull(sur, method)
  rankdf3 <- rankdf2 %>%
    mutate(familiarity = dat$Familiarity ) %>%
    mutate(famLevel = ifelse(familiarity <3, 1, 2)) %>%
    select(-familiarity)

  rankmean1 <- rankdf3 %>% 
    summarise_all(mean)
  
  rankmean2 <- rankdf3 %>% 
    summarise_all(mean)
  
  temp <- bind_rows(rankmean1, rankmean2) %>%
    mutate(filename = gtitle)
  if(qq == 1){
    rankmean <- temp
  }else{
    rankmean <- bind_rows(rankmean, temp)
  }
}

rankmean2 <- rankmean %>%
  relocate(filename) %>%
  arrange(filename) %>%
  relocate(filename, famLevel, Ori, Sea, Lin, Ten, LDA, Ave, Int) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  arrange(famLevel) %>%
  distinct()
rankmean2 %>% 
  print(n = 40)
# --------------------------------------------------------------------------------
# TASK  04: MORE CONCISE PLOT OF VOTING
# --------------------------------------------------------------------------------
library(ggplot2)
library(viridis)
library(gridExtra)
library(stringr)

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

Qnames <- paste(quest_names$Qname,c(rep("Retain", 4), rep("Remove", 4)), sep="")
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
  
  positions <- c("Original", "Interpolation", "Averaging", "LDA", "Tensor", "Linear_Multi", "SeamlessStitching")
  dfq <- dfq %>%
    mutate(Rank = as.factor(rank)) %>%
    mutate(across('method', str_replace, 'SeamlessStiching', 'SeamlessStitching')) %>%
    mutate(across('method', str_replace, 'LinearMulti', 'Linear_Multi'))
  
  if(qq == 1){
    dfq_all <- dfq
  }else{
    dfq_all <- bind_rows(dfq_all, dfq)
  }
}

dfq_all2 <- full_join(dfq_all, quest_names)

g1 <- ggplot(dfq_all2, aes(x = method, fill = Rank)) +
  geom_bar(position = 'stack') + 
  scale_fill_viridis(discrete = T, direction = -1) + 
  ylab("Votes") + 
  xlab("") + 
  scale_x_discrete(limits = positions)+
  facet_wrap(~Qname, nrow = 2) + 
  coord_flip() 
g1


g1


# --------------------------------------------------------------------------------
# TASK  05: prefmod ANALYSIS
# --------------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(tidyr)
library(prefmod)

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

Qnames <- paste(quest_names$Qname,c(rep("Retain", 4), rep("Remove", 4)), sep="")
Qnames <- stringr::str_remove_all(Qnames,'[0123456789]' )
stringr::str_replace(Qnames,'..', "-" )
quest_names$Qname <- Qnames


for(qq in 1:8){
  quest <- qq
  question <- dat[ ,(5 + quest)]
  
  sur <- survey_options[quest, c(1,3:9)] %>%
    pivot_longer(cols = 2:8 ) %>%
    rename(option = name, Method = value) %>%
    mutate(method = str_sub(Method, 1, 3)) %>%
    mutate(across('Method', str_replace, 'SeamlessStiching', 'SeamlessStitching')) %>%
    mutate(across('Method', str_replace, 'LinearMulti', 'Linear_Multi'))
  
  
  len <- length(question)
  q <- as.data.frame(question)
  rankdf <- q %>% separate_wider_delim(question, delim =';', names = c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8"))
  rankdf <- rankdf[ ,-8]
  
  rankdf <- rankdf %>%
    mutate(id = 1:dim(rankdf)[1]) %>% 
    relocate(id, .before=R1) #%>%
  colnames(rankdf)[-1] <- 1:7
  rankdflng <- rankdf %>%
    pivot_longer(cols = 2:8)
  rankdf2 <- rankdflng %>%
    pivot_wider(names_from = value,
                values_from = name) %>%
    relocate(id, A, B, C, D, E, F, G) %>%
    select(-id) %>% 
    mutate(A = as.integer(A), 
           B = as.integer(B),
           C = as.integer(C),
           D = as.integer(D),
           E = as.integer(E),
           F = as.integer(F),
           G = as.integer(G))
  
  colnames(rankdf2) <- pull(sur, Method)
  rankdf3 <- as.data.frame(rankdf2)
  rankdf4 <- rankdf2 %>%
    mutate(familiarity = dat[ ,14]) %>%
    mutate(F = ifelse(familiarity < 3, 1, 2 )) %>%
    select(-familiarity)
  
  rankdf5 <- as.data.frame(rankdf4)
  mod1 <- pattR.fit(rankdf5, nitems = 7, formel = ~F )
  mod1
  worth1 <- patt.worth(mod1)
  worth_mat <- patt.worth(mod1)
  
  worth_mat2 <- worth_mat %>% 
    as.data.frame() %>%
    mutate(filename = quest_names$Qname[qq], Method = rownames(worth_mat)) 
  if(qq == 1){
    worth_all <- worth_mat2
  }else{
    worth_all <- bind_rows(worth_all, worth_mat2)
  }
}

write.csv(worth_all, "Prefmod_coefficients.csv", row.names = FALSE)
