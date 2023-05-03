# Get Kenya 2009 Pf only prevalence
# -----------------------------------------------------------------------
library(dplyr)
library(readr)
library(malariaAtlas)
library(raster)
# REQUIRED: Set the working directory to the "Data_Inputs" directory
setwd()

isAvailable_pr(country = "Kenya")
KNY_pr_data <- getPR(country = "Kenya", species = "Pf")

KNY_pr_2009 <- KNY_pr_data %>% filter(year_start == 2009)
saveRDS(KNY_pr_2009, "Kenya_pf_pr_2009.rds")

# Make a simplified version of the Kenya border to use in plotting
# -----------------------------------------------------------------------
# Get the Kenya border:
library(rmapshaper)
kenyaBorder <- getData('GADM', country = 'KENYA', level = 0) # Level 0 for country border only
kenyaBorder <- ms_simplify(kenyaBorder, keep = 0.05) # Simplify polygons for faster plotting
saveRDS(kenyaBorder, "kenya_border_simple.rds")
