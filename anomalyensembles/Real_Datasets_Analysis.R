# ---------------------------------------------------------------------------
# TASK 0 : COMMON FUNCTIONS
# TASK 1 : T1 AND T2, CLUSTER RESULTS BY DATASET
# TASK 2 : CLUSTER RESULTS BY DATASET SOURCE
# TASK 3 : TABLE FROM SIGNIFICANCE TESTS CLUSTER OUTPUT
# TASK 4 : DATA IN PARAGRAPH IRT COMPARED WITH MAX FOR T1
# TASK 5 : SIMULATION - RANDOM 7 METHODS - SIGNIFICNCE BETWEEN THE TOP 2
# TASK 6 : DRAW BOX PLOTS FOR THE DATASET SOURCES IN T1 AND T2
# TASK 7 : AUC DIFFERENCES BETWEEN THE TOP 2 METHODS
# TASK 8 : CORRELATION GRAPH
# TASK 9 : CORRELATION COUNTS OF DATASETS
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# TASK 0 : COMMON FUNCTIONS
# ---------------------------------------------------------------------------
GetFileSources <- function(filenames){
  file_source <-c()
  for(ll in 1:length(filenames)){
    fname <- filenames[ll]
    regobj1 <- regexpr("_", fname)
    end.ind <- regobj1[1]-1
    file_source <- c(file_source, substring(fname, 1, end.ind))
  }
  return(file_source)
}


# ---------------------------------------------------------------------------
# TASK 1 : T1 AND T2, CLUSTER RESULTS BY DATASET
# ---------------------------------------------------------------------------
library(ggplot2)
library(grid)
library(gridExtra)

dat <- read.csv("anomalyensembles/EX_T1.csv")
dat <- dat[ ,-c(9:11)]
max_algo <- apply(dat[ ,-1], 1, which.max)


maxalgos <- c("IRT", "Average", "Greedy", "Greedy-Avg", "ICWA", "Max", "Thresh")[max_algo]
round( table(maxalgos)/length(maxalgos)*100, 1)
dfalgo1 <- as.data.frame(maxalgos)
dfalgo1 <- cbind.data.frame(as.vector(rep("T1", length(max_algo))), dfalgo1)
colnames(dfalgo1) <- c('Experiment', 'Algorithm')

g1 <- ggplot(dfalgo1, aes(Algorithm)) + geom_bar(width=0.5) + xlab("Best Performing Ensemble - T1") + ylab("Number of datasets") + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust=1))
g1


dat <- read.csv("anomalyensembles/EX_T2.csv")
dat <- dat[ ,-c(9:11)]
head(dat)
max_algo <- apply(dat[ ,-1], 1, which.max)


maxalgos <- c("IRT", "Average", "Greedy", "Greedy-Avg", "ICWA", "Max", "Thresh")[max_algo]
round( table(maxalgos)/length(maxalgos)*100, 1)
dfalgo1 <- as.data.frame(maxalgos)
dfalgo1 <- cbind.data.frame(as.vector(rep("T2", length(max_algo))), dfalgo1)
colnames(dfalgo1) <- c('Experiment', 'Algorithm')

g2 <- ggplot(dfalgo1, aes(Algorithm)) + geom_bar(width=0.5) + xlab("Best Performing Ensemble - T2") + ylab("Number of datasets") + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust=1))
g2


myplot1 <- gridExtra::arrangeGrob(g1, bottom = textGrob("(a)", x = unit(0.5, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))
myplot2 <- gridExtra::arrangeGrob(g2, bottom = textGrob("(b)", x = unit(0.55, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))


gridExtra::grid.arrange(myplot1, myplot2, widths=c(1,1))



# ---------------------------------------------------------------------------
# TASK 2  : CLUSTER RESULTS BY DATASET SOURCE
# ---------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

GetFileSources <- function(filenames){
  file_source <-c()
  for(ll in 1:length(filenames)){
    fname <- filenames[ll]
    regobj1 <- regexpr("_", fname)
    end.ind <- regobj1[1]-1
    file_source <- c(file_source, substring(fname, 1, end.ind))
  }
  return(file_source)
}


# T1 ---------------------------
dat <- read.csv("anomalyensembles/EX_T1.csv")
sources <- GetFileSources(dat[ ,1])
dat2 <- cbind.data.frame(sources, dat)
summs2 <- dat2 %>% group_by(sources) %>% select(IRT,  Average, Greedy, GreedyMean, ICWA, Max, Thres) %>% summarize(meanIRT = mean(IRT),  meanAvg = mean(Average), meanGrd = mean(Greedy), meanGA = mean(GreedyMean), meanICWA=mean(ICWA), meanMax = mean(Max), meanThres=mean(Thres))

maxalgo <- apply(summs2[ ,2:8], 1, which.max)
maxalgos <- c("IRT", "Average", "Greedy", "Greedy-Avg", "ICWA", "Max", "Thresh")[maxalgo]
round(table(maxalgos)/length(maxalgos)*100, 1)
dfalgo1 <- as.data.frame(maxalgos)
dfalgo1 <- cbind.data.frame(as.vector(rep("T1", length(maxalgo))), dfalgo1)
colnames(dfalgo1) <- c('Experiment', 'Algorithm')

g1 <- ggplot(dfalgo1, aes(Algorithm)) + geom_bar(width=0.5) + xlab("Best Performing Ensemble - T1") + ylab("Dataset Sources") + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust=1))
g1
table(dfalgo1[ ,2])

# table(dfalgo1[ ,2])
#
# Average     Greedy Greedy-Avg       ICWA        IRT        Max     Thresh
# 14         10         17         11         34         18         15

# T2 -----------------------------
dat <- read.csv("anomalyensembles/EX_T2.csv")
sources <- GetFileSources(dat[ ,1])
dat2 <- cbind.data.frame(sources, dat)
summs2 <- dat2 %>% group_by(sources) %>% select(IRT,  Average, Greedy, GreedyMean, ICWA, Max, Thres) %>% summarize(meanIRT = mean(IRT),  meanAvg = mean(Average), meanGrd = mean(Greedy), meanGA = mean(GreedyMean), meanICWA=mean(ICWA), meanMax = mean(Max), meanThres=mean(Thres))

maxalgo <- apply(summs2[ ,2:8], 1, which.max)
maxalgos <- c("IRT", "Average", "Greedy", "Greedy-Avg", "ICWA", "Max", "Thresh")[maxalgo]
round(table(maxalgos)/length(maxalgos)*100, 1)

dfalgo1 <- as.data.frame(maxalgos)
dfalgo1 <- cbind.data.frame(as.vector(rep("T2", length(maxalgo))), dfalgo1)
colnames(dfalgo1) <- c('Experiment', 'Algorithm')

g2 <- ggplot(dfalgo1, aes(Algorithm)) + geom_bar(width=0.5) + xlab(("Best Performing Ensemble - T2")) + ylab("Dataset Sources") + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust=1))
g2
table(dfalgo1[ ,2])
# Average     Greedy Greedy-Avg       ICWA        IRT        Max     Thresh
# 12         10         15          9         42         12          6


myplot1 <- gridExtra::arrangeGrob(g1, bottom = textGrob("(a)", x = unit(0.5, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))
myplot2 <- gridExtra::arrangeGrob(g2, bottom = textGrob("(b)", x = unit(0.5, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))


gridExtra::grid.arrange(myplot1, myplot2, widths=c(1,1))
# 500 275 - dims



# ---------------------------------------------------------------------------
# TASK 3 : TABLE FROM SIGNIFICANCE TESTS CLUSTER OUTPUT
# ---------------------------------------------------------------------------
library(dplyr)
# T1  --------------------------
dat <- read.csv("anomalyensembles/EX_T1.csv")
df <- dat[ ,2:8]

df2 <- df %>% mutate(IA = IRT - Average, IG= IRT - Greedy, IGM = IRT - GreedyMean, II = IRT - ICWA, IM = IRT - Max, IT = IRT - Thres) %>% select(IA, IG, IGM, II, IM, IT)

sources <- GetFileSources(dat[ ,1])
dat2 <- cbind.data.frame(sources, df2)
summs2 <- dat2 %>% group_by(sources) %>% select(IA, IG, IGM, II, IM, IT) %>% summarize(meanIA = mean(IA), sdIA = sd(IA),   meanIG = mean(IG), sdIG = sd(IG), meanIGM = mean(IGM), sdIGM = sd(IGM), meanII = mean(II), sdII = sd(II), meanIM=mean(IM), sdIM = sd(IM), meanIT = mean(IT), sdIT = sd(IT),)


# for each of the sources
len <- length(unique(sources))
max_perf <- data.frame(best_algo = rep(0, len), p_value=rep(0, len))
for(ii in 1:len){
  # get records of source ii
  inds <- which(sources == unique(sources)[ii])
  # which ensemble algorithm performs the best
  means <- apply(df[inds, ], 2, mean)
  best <- which.max(means)
  second_best <- order(means, decreasing = TRUE)[2]
  tt <- t.test(df[inds, best], df[inds, second_best], alternative="greater")
  mean_diff <- -1*diff(tt$estimate)
  max_perf[ii, ] <- as.vector(c(best, tt$p.value))

}

stars <- ifelse(max_perf$p_value < 0.05, ifelse(max_perf$p_value < 0.01, 2, 1), 0)
max_perf2 <- cbind.data.frame(unique(sources), max_perf, stars)

max_perf3 <- max_perf2[which(stars >0 ), ]
max_perf3$best_algo <- c("IRT", "Average", "Greedy", "Greedy-Avg", "ICWA", "Max", "Thresh")[max_perf3$best_algo]
max_perf3$roundedpval <- round(max_perf3$p_value, 3)
dim(max_perf3)
table(max_perf3$best_algo)
max_perf3

# T2 ----------------------------
dat <- read.csv("anomalyensembles/EX_T2.csv")
df <- dat[ ,2:8]

df2 <- df %>% mutate(IA = IRT - Average, IG= IRT - Greedy, IGM = IRT - GreedyMean, II = IRT - ICWA, IM = IRT - Max, IT = IRT - Thres) %>% select(IA, IG, IGM, II, IM, IT)

sources <- GetFileSources(dat[ ,1])
dat2 <- cbind.data.frame(sources, df2)
summs2 <- dat2 %>% group_by(sources) %>% select(IA, IG, IGM, II, IM, IT) %>% summarize(meanIA = mean(IA), sdIA = sd(IA),   meanIG = mean(IG), sdIG = sd(IG), meanIGM = mean(IGM), sdIGM = sd(IGM), meanII = mean(II), sdII = sd(II), meanIM=mean(IM), sdIM = sd(IM), meanIT = mean(IT), sdIT = sd(IT),)


# for each of the sources
len <- length(unique(sources))
max_perf <- data.frame(best_algo = rep(0, len), p_value=rep(0, len))
for(ii in 1:len){
  # get records of source ii
  inds <- which(sources == unique(sources)[ii])
  # which ensemble algorithm performs the best
  means <- apply(df[inds, ], 2, mean)
  best <- which.max(means)
  second_best <- order(means, decreasing = TRUE)[2]
  tt <- t.test(df[inds, best], df[inds, second_best], alternative="greater")
  mean_diff <- -1*diff(tt$estimate)
  max_perf[ii, ] <- as.vector(c(best, tt$p.value))

}

stars <- ifelse(max_perf$p_value < 0.05, ifelse(max_perf$p_value < 0.01, 2, 1), 0)
max_perf2 <- cbind.data.frame(unique(sources), max_perf, stars)

max_perf3 <- max_perf2[which(stars >0 ), ]
max_perf3$best_algo <- c("IRT", "Average", "Greedy", "Greedy-Avg", "ICWA", "Max", "Thresh")[max_perf3$best_algo]
max_perf3$roundedpval <- round(max_perf3$p_value, 3)
dim(max_perf3)
table(max_perf3$best_algo)
max_perf3

# ---------------------------------------------------------------------------
# TASK 4 : DATA IN PARAGRAPH IRT COMPARED WITH MAX FOR T1
# ---------------------------------------------------------------------------
library(dplyr)
dat <- read.csv("anomalyensembles/EX_T1.csv")
df <- dat[ ,2:8]
sources <- GetFileSources(dat[ ,1])

len <- length(unique(sources))
max_perf <- data.frame(best_algo = rep(0, len), p_value=rep(0, len))


for(ii in 1:len){
  # get records of source ii
  inds <- which(sources == unique(sources)[ii])
  # take irt and max
  if(mean(df[inds, 1]) > mean(df[inds, 6])){
    best <- 1
    tt <- t.test(df[inds, 1], df[inds, 6], alternative="greater")
  }else{
    best <- 6
    tt <- t.test(df[inds, 6], df[inds, 1], alternative="greater")
  }

  max_perf[ii, ] <- as.vector(c(best, tt$p.value))
}

stars <- ifelse(max_perf$p_value < 0.05, ifelse(max_perf$p_value < 0.01, 2, 1), 0)
max_perf2 <- cbind.data.frame(unique(sources), max_perf, stars)

max_perf3 <- max_perf2[which(stars >0 ), ]
max_perf3$roundedpval <- round(max_perf3$p_value, 3)
dim(max_perf3)
table(max_perf3$best_algo)
max_perf3


# ---------------------------------------------------------------------------
# TASK 5 : SIMULATION - RANDOM 7 METHODS - SIGNIFICNCE BETWEEN THE TOP 2
# ---------------------------------------------------------------------------
# RUN ONCE
set.seed(123)
Y <- rep(0, 1190)
for(i in 1:1190){
  X <- data.frame(x1 = runif(100), x2 = runif(100), x3 = runif(100), x4 = runif(100), x5 = runif(100), x6 = runif(100), x7 = runif(100))
  xmean <- apply(X, 2, mean)
  inds1 <- which.max(xmean)
  inds2 <- order(xmean, decreasing = TRUE)[2]
  tt <- t.test(X[ ,inds1], X[ ,inds2], alternative="greater")
  Y[i] <- tt$p.value
}
length(which(Y < 0.05))


# REPEAT 30 TIMES
set.seed(123)
lens <- rep(0, 30)
for(kk in 1:30){
  Y <- rep(0, 1190)
  for(i in 1:1190){
    X <- data.frame(x1 = runif(100), x2 = runif(100), x3 = runif(100), x4 = runif(100), x5 = runif(100), x6 = runif(100), x7 = runif(100))
    xmean <- apply(X, 2, mean)
    inds1 <- which.max(xmean)
    inds2 <- order(xmean, decreasing = TRUE)[2]
    tt <- t.test(X[ ,inds1], X[ ,inds2], alternative="greater")
    Y[i] <- tt$p.value
  }
  len[kk] <- length(which(Y < 0.05))
}
mean(len)
sd(len)


# ---------------------------------------------------------------------------
# TASK 6 : DRAW BOX PLOTS FOR THE DATASET SOURCES IN T1 AND T2
# ---------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# -------- T1
dat <-  read.csv("anomalyensembles/EX_T1.csv")
df <- dat[ ,2:8]

df2 <- df %>% mutate(IA = IRT - Average, IG= IRT - Greedy, IGM = IRT - GreedyMean, II = IRT - ICWA, IM = IRT - Max, IT = IRT - Thres) %>% select(IA, IG, IGM, II, IM, IT)

sources <- GetFileSources(dat[ ,1])
dat2 <- cbind.data.frame(sources, df2)
summs2 <- dat2 %>% group_by(sources) %>% select(IA, IG, IGM, II, IM, IT) %>% summarize(meanIA = mean(IA), sdIA = sd(IA),   meanIG = mean(IG), sdIG = sd(IG), meanIGM = mean(IGM), sdIGM = sd(IGM), meanII = mean(II), sdII = sd(II), meanIM=mean(IM), sdIM = sd(IM), meanIT = mean(IT), sdIT = sd(IT),)


# for each of the sources
len <- length(unique(sources))
max_perf <- data.frame(best_algo = rep(0, len), p_value=rep(0, len), mean_diff=rep(0,len), nn = rep(0, len))
sgdf <- c()
for(ii in 1:len){
  # get records of source ii
  inds <- which(sources == unique(sources)[ii])
  # which ensemble algorithm performs the best
  means <- apply(df[inds, ], 2, mean)
  best <- which.max(means)
  second_best <- order(means, decreasing = TRUE)[2]
  tt <- t.test(df[inds, best], df[inds, second_best], alternative="greater")

  if(tt$p.value < 0.05){
    sigdata <- cbind.data.frame(sources[inds], df[inds, ])
    sgdf <- rbind.data.frame(sgdf, sigdata)
  }

}
colnames(sgdf)[1] <- "Source"
head(sgdf)
colnames(sgdf)[5] <- "Greedy-Avg"

sgdflng <- pivot_longer(sgdf, cols=2:8)
sgdflng2 <- as.data.frame(sgdflng)
sourcelist <- unique(sgdflng2[ ,1])
sgsources <- sgdflng2[ ,1]

g1 <- ggplot(sgdflng, aes(name, value)) + geom_boxplot(aes(color=name)) + ylab("AUC")  + theme_bw() + facet_wrap(~Source, scales="free") + theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank())
g1


# -------- T2
dat <-  read.csv("anomalyensembles/EX_T2.csv")
df <- dat[ ,2:8]

df2 <- df %>% mutate(IA = IRT - Average, IG= IRT - Greedy, IGM = IRT - GreedyMean, II = IRT - ICWA, IM = IRT - Max, IT = IRT - Thres) %>% select(IA, IG, IGM, II, IM, IT)

sources <- GetFileSources(dat[ ,1])
dat2 <- cbind.data.frame(sources, df2)
summs2 <- dat2 %>% group_by(sources) %>% select(IA, IG, IGM, II, IM, IT) %>% summarize(meanIA = mean(IA), sdIA = sd(IA),   meanIG = mean(IG), sdIG = sd(IG), meanIGM = mean(IGM), sdIGM = sd(IGM), meanII = mean(II), sdII = sd(II), meanIM=mean(IM), sdIM = sd(IM), meanIT = mean(IT), sdIT = sd(IT),)


# for each of the sources
len <- length(unique(sources))
max_perf <- data.frame(best_algo = rep(0, len), p_value=rep(0, len), mean_diff=rep(0,len), nn = rep(0, len))
sgdf <- c()
for(ii in 1:len){
  # get records of source ii
  inds <- which(sources == unique(sources)[ii])
  # which ensemble algorithm performs the best
  means <- apply(df[inds, ], 2, mean)
  best <- which.max(means)
  second_best <- order(means, decreasing = TRUE)[2]
  tt <- t.test(df[inds, best], df[inds, second_best], alternative="greater")
  if(tt$p.value < 0.05){
    sigdata <- cbind.data.frame(sources[inds], df[inds, ])
    sgdf <- rbind.data.frame(sgdf, sigdata)
  }

}
colnames(sgdf)[1] <- "Source"
head(sgdf)
colnames(sgdf)[5] <- "Greedy-Avg"

sgdflng <- pivot_longer(sgdf, cols=2:8)
sgdflng2 <- as.data.frame(sgdflng)
sourcelist <- unique(sgdflng2[ ,1])

g2 <- ggplot(sgdflng, aes(name, value)) + geom_boxplot(aes(color=name)) + ylab("AUC")  + theme_bw() + facet_wrap(~Source, scales="free") +  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank())
g2


# ---------------------------------------------------------------------------
# TASK 7 : AUC DIFFERENCES BETWEEN THE TOP 2 METHODS
# ---------------------------------------------------------------------------
library(tidyr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# T1 ---------------
dat <-  read.csv("anomalyensembles/EX_T1.csv")
df <- dat[ ,2:8]

sources <- GetFileSources(dat[ ,1])
sourcelist <- unique(sources)
# for each of the sources
len <- length(unique(sourcelist))
max_perf <- data.frame(best_algo = rep(0, len), p_value=rep(0, len), mean_diff=rep(0,len))
newdf2 <- c()
for(ii in 1:len){
  # get records of source ii
  inds <- which(sources == unique(sourcelist)[ii])
  # which ensemble algorithm performs the best
  means <- apply(df[inds, ], 2, mean)
  best <- which.max(means)
  second_best <- order(means, decreasing = TRUE)[2]
  tt <- t.test(df[inds, best], df[inds, second_best], alternative="greater")
  mean_diff <- -1*diff(tt$estimate)
  max_perf[ii, ] <- as.vector(c(best, tt$p.value, mean_diff) )

  # Other dataframe
  diffbest <- df[inds, best] - df[inds, second_best]
  newdf <- cbind.data.frame(rep(ii, length(inds)), diffbest, rep(best, length(inds)) )
  colnames(newdf) <- c("Index", "PerfDiff", "BestAlgo")
  if(tt$p.value < 0.05){
    if(ii==1){
      newdf2 <- newdf
    }else{
      newdf2 <- rbind.data.frame(newdf2, newdf)
    }
  }

}

newdf2$BestAlgo <- c("IRT", "Average", "Greedy", "GreedyAvg", "ICWA", "Max", "Thresh")[newdf2$BestAlgo]
newdf2 <- cbind.data.frame(rep(sourcelist[unique(newdf2[ ,1])], as.vector(table(newdf2[ ,1])) ) , newdf2  )
colnames(newdf2)[1] <- "source"

group.colors <- c(IRT= "#00B6EB", Average = "#F8766D", Greedy ="#C49A00", GreedyAvg = "#53B400", ICWA = "#00C094", Max ="#A58AFF", Thres="#FB61D7")


g1 <- ggplot(newdf2, aes(source, PerfDiff))  + geom_boxplot(aes(color =BestAlgo)) + ylim(-0.5, 0.5)   + geom_hline(yintercept = 0, linetype="dashed") + coord_flip() +  scale_color_manual(values=group.colors) +   ylab("AUC Difference") + xlab("T1 Source") + theme_bw() +theme(legend.position="top")
g1


# T2 ---------------
dat <-  read.csv("anomalyensembles/EX_T2.csv")
df <- dat[ ,2:8]

sources <- GetFileSources(dat[ ,1])
sourcelist <- unique(sources)
# for each of the sources
len <- length(unique(sourcelist))
max_perf <- data.frame(best_algo = rep(0, len), p_value=rep(0, len), mean_diff=rep(0,len))
newdf2 <- c()
for(ii in 1:len){
  # get records of source ii
  inds <- which(sources == unique(sourcelist)[ii])
  # which ensemble algorithm performs the best
  means <- apply(df[inds, ], 2, mean)
  best <- which.max(means)
  second_best <- order(means, decreasing = TRUE)[2]
  tt <- t.test(df[inds, best], df[inds, second_best], alternative="greater")
  mean_diff <- -1*diff(tt$estimate)
  max_perf[ii, ] <- as.vector(c(best, tt$p.value, mean_diff ) )

  # Other dataframe
  diffbest <- df[inds, best] - df[inds, second_best]
  newdf <- cbind.data.frame(rep(ii, length(inds)), diffbest, rep(best, length(inds)) )
  colnames(newdf) <- c("Index", "PerfDiff", "BestAlgo")
  if(tt$p.value < 0.05){
    if(ii==1){
      newdf2 <- newdf
    }else{
      newdf2 <- rbind.data.frame(newdf2, newdf)
    }
  }

}

newdf2$BestAlgo <- c("IRT", "Average", "Greedy", "GreedyAvg", "ICWA", "Max", "Thresh")[newdf2$BestAlgo]
newdf2 <- cbind.data.frame(rep(sourcelist[unique(newdf2[ ,1])], as.vector(table(newdf2[ ,1])) ) , newdf2  )
colnames(newdf2)[1] <- "source"

group.colors <- c(IRT= "#00B6EB", Average = "#F8766D", Greedy ="#C49A00", GreedyAvg = "#53B400", ICWA = "#00C094", Max ="#A58AFF", Thres="#FB61D7")


g2 <- ggplot(newdf2, aes(source, PerfDiff))  + geom_boxplot(aes(color =BestAlgo)) + ylim(-0.5, 0.5)   + geom_hline(yintercept = 0, linetype="dashed") + coord_flip() +  scale_color_manual(values=group.colors) + ylab("AUC Difference") + xlab("T2 Source") + theme_bw() +theme(legend.position="top")
g2


myplot1 <- gridExtra::arrangeGrob(g1, bottom = textGrob("(a)", x = unit(0.6, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))
myplot2 <- gridExtra::arrangeGrob(g2, bottom = textGrob("(b)", x = unit(0.65, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))


gridExtra::grid.arrange(myplot1, myplot2, widths=c(1,1))
# 850 x 400





# ---------------------------------------------------------------------------
# TASK 8 : CORRELATION GRAPH
# ---------------------------------------------------------------------------
library(tidyr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# T1 -------------
dat <-  read.csv("anomalyensembles/EX_T1.csv")
df <- dat[ ,2:8]

sources <- GetFileSources(dat[ ,1])
sourcelist <- unique(sources)
# for each of the sources
len <- length(unique(sourcelist))
max_perf <- data.frame(best_algo = rep(0, len), p_value=rep(0, len), mean_diff=rep(0,len), cor90 = rep(0, len), cor80 = rep(0, len), cor70 = rep(0, len))
newdf2 <- c()
for(ii in 1:len){
  # get records of source ii
  inds <- which(sources == unique(sourcelist)[ii])
  # which ensemble algorithm performs the best
  means <- apply(df[inds, ], 2, mean)
  cormeans <- apply(dat[inds, 9:11], 2, mean)
  best <- which.max(means)
  second_best <- order(means, decreasing = TRUE)[2]
  tt <- t.test(df[inds, best], df[inds, second_best], alternative="greater")
  mean_diff <- -1*diff(tt$estimate)
  max_perf[ii, ] <- as.vector(c(best, tt$p.value, mean_diff, cormeans ) )

  # Other dataframe
  newdf <- cbind.data.frame(rep(ii, length(inds)), dat[inds, 9:11], rep(best, length(inds)) )
  colnames(newdf) <- c("Index", "Cor90", "Cor80", "Cor70", "BestAlgo")
  if(tt$p.value < 0.05){
    if(ii==1){
      newdf2 <- newdf
    }else{
      newdf2 <- rbind.data.frame(newdf2, newdf)
    }
  }

}

newdf2$BestAlgo <- c("IRT", "Average", "Greedy", "GreedyAvg", "ICWA", "Max", "Thresh")[newdf2$BestAlgo]
newdf2 <- cbind.data.frame(rep(sourcelist[unique(newdf2[ ,1])], as.vector(table(newdf2[ ,1])) ) , newdf2  )
colnames(newdf2)[1] <- "source"


group.colors <- c(IRT= "#00B6EB", Average = "#F8766D", Greedy ="#C49A00", GreedyAvg = "#53B400", ICWA = "#00C094", Max ="#A58AFF", Thres="#FB61D7")

dflong <- pivot_longer(newdf2, 3:5)
colnames(dflong)[4] <- "Correlation"
g1 <- ggplot(dflong, aes(x=source, y=value, color=BestAlgo) )  + geom_boxplot() + coord_flip() + facet_wrap(~Correlation,scales="free_x")  + xlab("T1 Source") + ylab("Correlation") +  scale_color_manual(values=group.colors)   + theme_bw() +theme(legend.position="top")
g1



# T2 -------------
dat <-  read.csv("anomalyensembles/EX_T2.csv")
df <- dat[ ,2:8]

sources <- GetFileSources(dat[ ,1])
sourcelist <- unique(sources)
# for each of the sources
len <- length(unique(sourcelist))
max_perf <- data.frame(best_algo = rep(0, len), p_value=rep(0, len), mean_diff=rep(0,len), cor90 = rep(0, len), cor80 = rep(0, len), cor70 = rep(0, len))
newdf2 <- c()
for(ii in 1:len){
  # get records of source ii
  inds <- which(sources == unique(sourcelist)[ii])
  # which ensemble algorithm performs the best
  means <- apply(df[inds, ], 2, mean)
  cormeans <- apply(dat[inds, 9:11], 2, mean)
  best <- which.max(means)
  second_best <- order(means, decreasing = TRUE)[2]
  tt <- t.test(df[inds, best], df[inds, second_best], alternative="greater")
  mean_diff <- -1*diff(tt$estimate)
  max_perf[ii, ] <- as.vector(c(best, tt$p.value, mean_diff, cormeans ) )

  # Other dataframe
  newdf <- cbind.data.frame(rep(ii, length(inds)), dat[inds, 9:11], rep(best, length(inds)) )
  colnames(newdf) <- c("Index", "Cor90", "Cor80", "Cor70", "BestAlgo")
  if(tt$p.value < 0.05){
    if(ii==1){
      newdf2 <- newdf
    }else{
      newdf2 <- rbind.data.frame(newdf2, newdf)
    }
  }

}

newdf2$BestAlgo <- c("IRT", "Average", "Greedy", "Greedy-Avg", "ICWA", "Max", "Thresh")[newdf2$BestAlgo]
newdf2 <- cbind.data.frame(rep(sourcelist[unique(newdf2[ ,1])], as.vector(table(newdf2[ ,1])) ) , newdf2  )
colnames(newdf2)[1] <- "source"


group.colors <- c(IRT= "#00B6EB", Average = "#F8766D", Greedy ="#C49A00", GreedyAvg = "#53B400", ICWA = "#00C094", Max ="#A58AFF", Thres="#FB61D7")

dflong <- pivot_longer(newdf2, 3:5)
colnames(dflong)[4] <- "Correlation"
g2 <- ggplot(dflong, aes(x=source, y=value, color=BestAlgo) )  + geom_boxplot() + coord_flip() + facet_wrap(~Correlation,scales="free_x")  + xlab("T2 Source") + ylab("Correlation") +  scale_color_manual(values=group.colors)   + theme_bw() +theme(legend.position="top")
g2
## 500 350 CorT2

myplot1 <- gridExtra::arrangeGrob(g1, bottom = textGrob("(a)", x = unit(0.6, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))
myplot2 <- gridExtra::arrangeGrob(g2, bottom = textGrob("(b)", x = unit(0.65, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))


gridExtra::grid.arrange(myplot1, myplot2, widths=c(1,1))




# ---------------------------------------------------------------------------
# TASK 9 : CORRELATION COUNTS OF DATASETS
# ---------------------------------------------------------------------------
# T1 -------------
dat <-  read.csv("anomalyensembles/EX_T1.csv")
cors <- dat[ ,9:11]
dat <- dat[ ,2:8]
head(dat)
max_algo <- apply(dat, 1, which.max)


maxalgos <- c("IRT", "Average", "Greedy", "Greedy-Avg", "ICWA", "Max", "Thresh")[max_algo]
dfalgo1 <- as.data.frame(maxalgos)
dfalgo1 <- cbind.data.frame(as.vector(rep("T1", length(max_algo))), dfalgo1)
colnames(dfalgo1) <- c('Experiment', 'Algorithm')

cors2 <- cbind.data.frame(maxalgos, cors)

inds <- which((cors2$Cor70 ==0) & (cors2$Cor80 ==0) & (cors2$Cor90==0 ) )
length(inds)
table(maxalgos[inds])/length(inds)
table(maxalgos[-inds])/length(maxalgos[-inds])


cordf <- cbind.data.frame(names(table(maxalgos[inds])), as.vector(table(maxalgos[inds])/length(inds) ), as.vector(table(maxalgos[-inds])/length(maxalgos[-inds])) , as.vector(table(maxalgos)/length(maxalgos)) )
colnames(cordf) <- c("Ensemble", "Low_Cor", "High_Cor", "All")

corslng <- pivot_longer(cordf, cols=2:4)
g1 <- ggplot(corslng, aes(Ensemble, value, group=Ensemble)) + geom_point(aes(shape=name, color=name), size=2) + geom_path(linetype="dashed") + xlab("Best Performing Ensemble - T1") + ylab("Dataset Proportion") + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust=1)) +  theme(legend.position="none")
g1

# T2 ---------------------
dat <-  read.csv("anomalyensembles/EX_T2.csv")
cors <- dat[ ,9:11]
dat <- dat[ ,2:8]
head(dat)
max_algo <- apply(dat, 1, which.max)


maxalgos <- c("IRT", "Average", "Greedy", "Greedy-Avg", "ICWA", "Max", "Thresh")[max_algo]
dfalgo1 <- as.data.frame(maxalgos)
dfalgo1 <- cbind.data.frame(as.vector(rep("T1", length(max_algo))), dfalgo1)
colnames(dfalgo1) <- c('Experiment', 'Algorithm')

cors2 <- cbind.data.frame(maxalgos, cors)

inds <- which((cors2$Cor70 ==0) & (cors2$Cor80 ==0) & (cors2$Cor90==0 ) )
length(inds)
table(maxalgos[inds])/length(inds)
table(maxalgos[-inds])/length(maxalgos[-inds])

cordf <- cbind.data.frame(names(table(maxalgos[inds])), as.vector(table(maxalgos[inds])/length(inds) ), as.vector(table(maxalgos[-inds])/length(maxalgos[-inds])) , as.vector(table(maxalgos)/length(maxalgos)) )
colnames(cordf) <- c("Ensemble", "Low_Cor", "High_Cor", "All")

corslng <- pivot_longer(cordf, cols=2:4)
colnames(corslng)[2] <- "Group"
g2 <- ggplot(corslng, aes(Ensemble, value)) + geom_point(aes(shape=Group, color=Group), size=2) + geom_path(linetype="dashed")  + xlab("Best Performing Ensemble - T2") + ylab("Dataset Proportion") + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust=1))
g2


myplot1 <- gridExtra::arrangeGrob(g1, bottom = textGrob("(a)", x = unit(0.5, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))
myplot2 <- gridExtra::arrangeGrob(g2, bottom = textGrob("(b)", x = unit(0.4, "npc"), y   = unit(0.5, "npc"), just=c("left","bottom"),gp=gpar(col="black")))


gridExtra::grid.arrange(myplot1, myplot2, widths=c(1,1.35))
