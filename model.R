
# 0. clean-up ----------------------------------------------------------------

rm(list = ls())


# 1. make directory ----------------------------------------------------------
dir.create("model", showWarnings = F)

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


