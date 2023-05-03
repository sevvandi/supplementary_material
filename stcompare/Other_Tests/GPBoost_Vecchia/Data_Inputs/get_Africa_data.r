# Get prevalance data from all of Africa in 2009
# -----------------------------------------------------------------------
library(dplyr)
library(readr)
library(malariaAtlas)

isAvailable_pr(continent = "Africa")
Africa_pr_data <- getPR(continent = "Africa", species = "Pf")

Africa_pr_2009 <- Africa_pr_data %>% filter(year_start == 2009)

# Many of the datapoints have NA latitudes and longitudes, these come from DHS surveys
# Filter out NA location points here rather than in the model files

Africa_pr_2009 <- filter(Africa_pr_2009, !is.na(longitude))

saveRDS(Africa_pr_2009, file = "Africa_pf_pr_2009")
