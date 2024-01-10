# SEAwise_T3.3

## Description

Files should be run in order: `source("data.R"); source("model.R"); source("output.R");`

-   ***data.R*** - Script to source all data preparation scripts and loads required packages:
    -   *data_01_lookup_table_raw_FLStocks.R* - Creates look-up table (*lutFLStocks.Rdata*) for raw data objects, including standardized stock code, file path, case study, and FLStock object name.
    -   *data_02_make_list_of_FLStocks.R -* Creates a list of standardized FLStock objects (*FLStocks_list_stks.Rdata*) with additional identifiers (stock code, case study). Creates diagnostic summary plots of each stock object (*FLStock_summaries.pdf).*
    -   *data_03_lookup_tables_YearAge_LWcoefs.R* - 1. Creates a look-up table (*lutYearAge.Rdata*) with data filtering criteria for the growth model fitting (years x age combinations). The resulting data.frame can contain multiple lines per stock with different combinations of filters. 2. Creates look-up table of length-weight relationship coefficients by stock (*lutLWcoefs.Rdata*).
    -   *data_04_make_list_of_data.R* - Creates list of final data.frames by stock (*data_list_DF.Rdata*) for further use in the growth modelling analysis.
-   ***model.R*** - Script to source all model fitting scripts and loads required packages
    -   *model_01.R* - Creates summary data.frame of all model fits for each stock (*stockcode_res.Rdata*)
-   ***output.R*** - Script to source all output scripts and loads required packages
    -   *output_01.R* - Creates summary figures and tables of model fits for each stock

## Non-CRAN required packages

```{r}
library(devtools)
# Archived package on CRAN
install_version("lmmlasso", version = "0.1-2", repos = "http://cran.us.r-project.org")

# Development version needed for GAMMs
remotes::install_github("glmmTMB/glmmTMB/glmmTMB")

# Non-CRAN
# fork that handles multiple RE
install_github("https://github.com/bbolker/lmmen")
# fork that handles multiple RE
install_github("https://github.com/mebrooks/cv.glmmLasso")
# state-space model
install_github("mebrooks/growmod/growmod")
```
