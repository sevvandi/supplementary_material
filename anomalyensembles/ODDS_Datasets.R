# ---------------------------------------------------------------------------
# TASK 1 : INDIVIDUAL METHODS ON ODDS REPO - DO IT
# ---------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
folder <- paste(here(), "/anomalyensembles/ODDS_Individual/", sep="")
files_list <- list.files(folder)

for(i in 1:length(files_list)){
  dat <- read.csv(paste(folder, files_list[i], sep=""))
  if(i==1){
    dat_all <- dat
  }else{
    dat_all <- full_join(dat_all, dat)
  }
}


dat2 <- dat_all[ ,1:11]
colnames(dat2) <- substring(colnames(dat2), 1, (nchar(colnames(dat2)) - 4 ))
dat3 <- cbind.data.frame(files_list, dat2)
colnames(dat3)[1] <- "filename"
df_long <- pivot_longer(dat3, 2:dim(dat3)[2])
g1 <- ggplot(df_long, aes(name, value)) + geom_boxplot(outlier.size = 0.8) + geom_point(color="black", size=0.8, alpha=0.9) + xlab("Individual Algorithm") + ylab("AUC") + theme_bw() +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g1

datens <- read.csv(paste(here(), "/anomalyensembles/Collated_EX2.csv", sep=""))
datens <- datens[ ,1:8]
colnames(datens)[1] <- "filename"
colnames(datens) <- substring(colnames(datens), 1, (nchar(colnames(datens)) - 4 ))
colnames(datens)[1] <- "filename"


df_long <- pivot_longer(datens, 2:dim(datens)[2])
g2 <- ggplot(df_long, aes(name, value, color=name)) + geom_boxplot(outlier.size = 0.8) + geom_point(aes(color=name), size=0.8, alpha=0.9) + xlab("Ensemble") + ylab("AUC") + ylim(c(0.3, 1)) + theme_bw() +  theme(legend.position = "none") +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2
gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(1.5,1))

