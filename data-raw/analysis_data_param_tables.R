# ---- Make Parameter Tables for Standardising out-of-sample Predictions ----

#NOTE: relies on package being built after running `data-raw/osf_load_analyst_datasets.R`

library(tidyverse)
library(ManyEcoEvo)

analysis_data_param_tables <- 
  bind_rows(
    make_param_table(ManyEcoEvo::blue_tit_data) %>% 
      mutate(dataset = "blue tit"),
    make_param_table(ManyEcoEvo::euc_data) %>% 
      mutate(dataset = "eucalyptus")
  )


usethis::use_data(analysis_data_param_tables, internal = TRUE, overwrite = TRUE)
