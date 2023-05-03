AUTHOR: Spencer Wong

R scripts for the running each of INLA, GPBoost, FRK, and SpRF on different malaria prevalence datasets over Africa.

Models are run through the R/<model>_individual.R scripts, with a command line argument specifying which input dataset to use.

The scripts Data_Inputs/get_Africa_data.r, Data_Inputs/load_maps.R, Data_Inputs/make_grids.R and R/Binomial_data_simulation.R must be run first in order to download and generate the required datasets.