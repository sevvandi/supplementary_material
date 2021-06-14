# COMPUTES DIFFERENT ANOMALY DETECTION ENSEMBLES
library(RWeka)
library(pROC)
library(DDoutlier)
library(psych)
library(airt)

source(file="Functions.R")

args=(commandArgs(TRUE))

if(length(args)==0){
  print("No arguments supplied.")
  ##supply default values
  break;
  file_name = "Wilt_withoutdupl_02_v01.arff"
}else{
  file_name = args[1]
}

#----------------------------------------------------
# READ FILE AND PRE-PROCESSING
#----------------------------------------------------
data.folder <- "/mnt/lustre/projects/Mona0072/e103049/nlout/Data_Repository/To_GitHub/"
dat.o <- read.arff(paste(data.folder,file_name , sep=""))

# Remove duplicates
dat1 <- dat.o[!duplicated(dat.o), ]

X <- dat1[ ,-dim(dat1)[2]]
nn <- dim(dat1)[1]

labsori <- dat1[ ,dim(dat1)[2]]
labs <- rep(0, nn)
labs[labsori=="yes"] <- 1

X <- unitize(X)
# kk <- max(ceiling(dim(X)[1]/10), 50)

# DDoutlier scores
knnsc <- tryCatch(DDoutlier::KNN_AGG(X), error = function(e){
  print(sys.calls()) # , k_min=kk, k_max=kk+10
  knnsc <- rep(NA, nn)
  return(knnsc)})
lofsc <- tryCatch(DDoutlier::LOF(X), error = function(e){
  print(sys.calls())
  lofsc <- rep(NA, nn)
  return(lofsc)})
cofsc <- tryCatch(DDoutlier::COF(X), error = function(e){
  print(sys.calls())
  cofsc <- rep(NA, nn)
  return(cofsc)})
inflosc <- tryCatch(DDoutlier::INFLO(X), error = function(e){
  print(sys.calls())
  inflosc <- rep(NA, nn)
  return(inflosc)})
kdeossc <- tryCatch(DDoutlier::KDEOS(X), error = function(e){
  print(sys.calls())
  kdeossc <- rep(NA, nn)
  return(kdeossc)})
ldfsc <- tryCatch(DDoutlier::LDF(X)$LDF, error = function(e){
  print(sys.calls())
  ldfsc <- rep(NA, nn)
  return(ldfsc)})
ldofsc <- tryCatch(DDoutlier::LDOF(X), error = function(e){
  print(sys.calls())
  ldofsc <- rep(NA, nn)
  return(ldofsc)})

Y <- cbind(knnsc, lofsc, cofsc, inflosc, kdeossc, ldfsc, ldofsc)
na_cols <- which(apply(Y, 2, function(x) sum(is.na(x))) >0)
na_cols
if(length(na_cols)>0){
  Y<- Y[ ,-na_cols]
}

maxs <- apply(Y, 2, max)
mins <- apply(Y, 2, min)
divs <- maxs - mins
Y <- as.data.frame(Y)
Y <- sweep(Y, 2, mins)
Y <- sweep(Y, 2, divs, "/")

# Correlation values
coy <- cor(Y)
numones <- dim(coy)[1]
nineties <- sum(coy > 0.9) - numones
eighties <- sum(coy > 0.8) - numones
seventies <- sum(coy > 0.7) - numones


# IRT Ensemble
modout <- irt_ensemble(Y)
irt_ens_scores <- modout$scores
roc_obj <- pROC::roc(labs, irt_ens_scores, direction = "<")
irt_ens <- roc_obj$auc

# Taking the mean - Average ensemble
Ymean <- apply(Y, 1, mean)
roc_obj <- pROC::roc(labs, Ymean, direction = "<")
averages <- roc_obj$auc

# Greedy Ensemble - Schubert et al
gr_obj <- greedy_ensemble(Y, kk=1)
gr_score <- gr_obj$scores
roc_obj <- pROC::roc(labs, gr_score, direction = "<")
gr_ens <- roc_obj$auc

# Greedy Ensemble mean - Schubert et al
gr_score_one <- matrix(0, nrow=dim(Y)[1], ncol=10)
for(jj in 1:10){
  gr_obj <- greedy_ensemble(Y, kk=jj)
  gr_score_one[ ,jj] <- gr_obj$scores
}
gr_score_mean <- apply(gr_score_one, 1, mean)
roc_obj <- pROC::roc(labs, gr_score_mean, direction = "<")
gr_ens_mean <- roc_obj$auc


# ICWA Ensemble
icwa_score <- icwa_ensemble(Y)
roc_obj <- pROC::roc(labs, icwa_score, direction = "<")
icwa_ens <- roc_obj$auc

# MAX Ensemble
max_score <- max_ensemble(Y)
roc_obj <- pROC::roc(labs, max_score, direction = "<")
max_ens <- roc_obj$auc

# Threshold Ensemble
thres_score <- threshold_ensemble(Y)
roc_obj <- pROC::roc(labs, thres_score, direction = "<")
thres_ens <- roc_obj$auc

output <- cbind.data.frame(irt_ens, averages, gr_ens,  gr_ens_mean, icwa_ens, max_ens, thres_ens, nineties, eighties, seventies)
colnames(output) <- c("IRT", "Average", "Greedy", "GreedyMean", "ICWA", "Max", "Thres", "Cor90", "Cor80", "Cor70")


#----------------------------------------------------
# WRITE OUTPUT
#----------------------------------------------------
write_folder <- "/mnt/lustre/projects/Mona0072/e103049/IRTensemble/Data_Output/EX1/TAKE_1/"
file_name2 <- tools::file_path_sans_ext(file_name)

write_file_name <- paste(write_folder, file_name2,  ".csv", sep="")
write.csv(output, write_file_name, row.names = FALSE )
