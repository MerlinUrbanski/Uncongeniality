#####load packages#####
library(MASS)
library(mice)
library(dplyr)
library(randomForest)
library(caret)
library(foreach)
library(parallel)
library(doParallel)
library(doRNG)

# Helpers ----------------------------------------------------------------------

source("code/simulations/own_device/functions.R")

# Simulation -------------------------------------------------------------------

run_iteration() #fill in the number of the iteration that you want to run
                #results are saved in the "results" folder

