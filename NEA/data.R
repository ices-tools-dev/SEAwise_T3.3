
# 0. clean-up ----------------------------------------------------------------

rm(list = ls())

# 1. required packages -------------------------------------------------------

library(rfishbase)
library(FLCore)
library(ggplotFL)
library(patchwork)
library(data.table)


# 2. source data scripts --------------------------------------------------

source("NEA/data_01_lookup_table_raw_FLStocks.R")
source("NEA/data_02_make_list_of_FLStocks.R")
source("NEA/data_03_lookup_tables_YearAge_LWcoefs.R")
source("NEA/data_04_make_list_of_data.R")