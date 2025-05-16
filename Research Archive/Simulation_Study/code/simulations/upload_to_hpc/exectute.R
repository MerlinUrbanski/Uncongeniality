#####load packages#####
.libPaths(c("~/Rlibs", .libPaths()))

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

source("functions.R")

# Simulation -------------------------------------------------------------------

slurm_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
to_do    <- c(1:100)
iter     <- to_do[slurm_id]

run_iteration(iter)