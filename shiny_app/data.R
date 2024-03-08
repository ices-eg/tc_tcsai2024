## Before:
## After:

library(icesTAF)

mkdir("data")

# copy csvs to data dir
cp(taf.data.path("*.csv"), "data")
cp(taf.data.path("*.dat"), "data")

source("data_02_model_fitting.R")
source("data_06_VPA.R")

save(
  haddock, cod, herring, ssqs,
  cod_catch, cod_mat, cod_wt, cod_Year,
  haddock_catch, haddock_mat, haddock_wt, haddock_Year,
  df_old,
  file = "data/app_data.RData"
)
