# ---- Load Libraries ----
library(tidyverse)
library(targets)
library(usethis)

# ---- Run targets pipeline ----
tar_destroy() # Uncomment and run if want to reproduce from scratch, run locally only
tar_make()

# ---- Read targets and use data ----

# Targets required for main manuscript:
tar_load(ManyEcoEvo)
tar_load(ManyEcoEvo_yi)
tar_load(ManyEcoEvo_results)
tar_load(ManyEcoEvo_yi_results)
tar_load(ManyEcoEvo_viz)
tar_load(ManyEcoEvo_yi_viz)

usethis::use_data(ManyEcoEvo, 
                  ManyEcoEvo_yi,
                  ManyEcoEvo_results,
                  ManyEcoEvo_yi_results,
                  ManyEcoEvo_viz,
                  ManyEcoEvo_yi_viz,
                  overwrite = TRUE)

# TODO also need to add raw files to demonstrate pipeline / package functionality in package vignette and software manuscript.

