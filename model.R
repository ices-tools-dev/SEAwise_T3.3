
# 0. clean-up ----------------------------------------------------------------

rm(list = ls())


# 1. make directory ----------------------------------------------------------
dir.create("model", showWarnings = F)
dir.create("model/fit_by_stk_n_method", showWarnings = F)

# 2. required packages -------------------------------------------------------

library(emulator)
library(miscTools)
library(penalized)
library(quadprog)
library(growmod) # state-space model
library(lmmlasso)
library(lmmen) # fork that handles multiple RE
library(cv.glmmLasso) # fork that handles multiple RE
library(doParallel)
library(glmmTMB)
library(MuMIn)

# source model scripts ----------------------------------------------------------
source("model_01.R")
source("model_02_full_dataset_model_selection.R")
source("model_03_full_dataset_forecast_evaluation.R")

