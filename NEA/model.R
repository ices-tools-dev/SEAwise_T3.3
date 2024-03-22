
# 0. clean-up ----------------------------------------------------------------

rm(list = ls())


# 1. make directory ----------------------------------------------------------
dir.create("NEA/model", showWarnings = F)
dir.create("NEA/model/fit_by_stk_n_method", showWarnings = F)

# 2. required packages -------------------------------------------------------

library(emulator)
library(miscTools)
library(penalized)
library(quadprog)
library(lmmlasso)
library(lmmen) 
library(cv.glmmLasso)
library(doParallel)
library(glmmTMB)
library(MuMIn)

# source model scripts ----------------------------------------------------------
source("NEA/model_01.R")
source("NEA/model_02_full_dataset_model_selection.R")
source("NEA/model_03_full_dataset_forecast_evaluation.R")

