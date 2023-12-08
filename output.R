# 0. clean-up ----------------------------------------------------------------

rm(list = ls())


# 1. make directory ----------------------------------------------------------
dir.create("output", showWarnings = F)
dir.create("output/figures", showWarnings = F)


# 2. required packages -------------------------------------------------------

library(ggplot2)
library(reshape2)
library(FLCore)

# source output scripts ---------------------------------------------------

source("output_01.R")
