#### CHANGE DATA FOLDER AND WRITE FOLDER FOR EACH TASK
data_folder_1 <-  "/mnt/lustre/projects/Mona0072/e103049/IRTensemble/Data_Output/EX1/TAKE_1/"
write_folder <- "/mnt/lustre/projects/Mona0072/e103049/IRTensemble/Data_Output/EX1/"


files_list <- list.files(data_folder_1)
for(i in 1:length(files_list)){
  dat <- read.csv(paste(data_folder_1,files_list[i], sep=""))
  file_name2 <- tools::file_path_sans_ext(files_list[i])
  if(i==1){
    features_summary <- cbind(file_name2,dat)
  }else{
    temp <- cbind(file_name2,dat)
    features_summary <- rbind.data.frame(features_summary,temp)
  }
}
colnames(features_summary)[1] <- "filename"

####  CHANGE FILE NAME APPROPRIATELY FOR EACH TASK
write.csv(features_summary,paste(write_folder, "Collated_EX1_Take1.csv", sep="" ), row.names = FALSE )
