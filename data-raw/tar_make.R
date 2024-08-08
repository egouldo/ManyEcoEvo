# ---- Load Libraries ----
library(tidyverse)
library(targets)
library(usethis)
library(crew)
library(tictoc)

# ---- Run targets pipeline ----
tar_destroy() # Uncomment and run if want to reproduce from scratch, run locally only
tic()
tar_make()
toc()
# tar_make(names = ManyEcoEvo_results)
# tar_make(shortcut = T, callr_function = NULL) #`callr_function` is set to NULL for debugging

# ---- Read targets and use data ----

# Load targets required for main manuscript / package:
tic()
tar_load(ManyEcoEvo)
tar_load(ManyEcoEvo_yi)
tar_load(ManyEcoEvo_results)
tar_load(ManyEcoEvo_yi_results)
tar_load(ManyEcoEvo_viz)
tar_load(ManyEcoEvo_yi_viz)
tar_load(ManyEcoEvo_study_summary)
toc()

# Write targets to data folder:
tic()
usethis::use_data(ManyEcoEvo,
  ManyEcoEvo_yi, # consider making internal
  ManyEcoEvo_results,
  ManyEcoEvo_yi_results, # consider making internal
  ManyEcoEvo_viz,
  ManyEcoEvo_yi_viz, # TODO consider making internal
  ManyEcoEvo_study_summary,
  overwrite = TRUE,
  compress = "gzip"
)
toc()

# TODO also need to add raw files to demonstrate pipeline / package functionality in package vignette and software manuscript.
