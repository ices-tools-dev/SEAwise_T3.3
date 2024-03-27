# 0. clean-up ----------------------------------------------------------------

rm(list = ls())


# 1. make directory ----------------------------------------------------------
dir.create("NEA/output", showWarnings = F)
dir.create("NEA/output/figures", showWarnings = F)


# 2. required packages -------------------------------------------------------

library(ggplot2)
library(reshape2)
library(FLCore)

# source output scripts ---------------------------------------------------

source("NEA/output_01.R")
source("NEA/output_02_full_dataset_model_selection.R")
source("NEA/output_03_method_forecast_evaluation.R")
source("NEA/output_04_model_forecast_evaluation.R")
source("NEA/output_05_model_partial.effects.R")

