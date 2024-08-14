# README: `data-raw/analysis_datasets`


This directory contains the analysis datasets used by analysis teams to
answer the project’s research questions.

- `blue_tit_data_updated_2020-04-18.csv` contains the Evolutionary
  Ecology or Blue tit dataset, stored at <https://osf.io/34fzc/>.
- `Euc_data.csv` contains the Ecology and Conservation or Eucalyptus
  dataset, stored at <https://osf.io/t76uy/>.

The two files are downloaded to this directory using the script
`osf_load_analyst_datasets.R`, which uses the [`osfr::`
package](https://github.com/ropensci/osfr).

For assistance with setting up authentication to run this script, check
out <https://cran.r-project.org/web/packages/osfr/vignettes/auth.html>.
Alternatively, simply download the files directly from the OSF using the
URL’s above. The script downloads the files, loads them into R, and then
calculates some additional variables, which were variables constructed
by analysts, but not present in the supplied analysis datasets.

This script creates the package data `euc_data` and `blue_tit_data`.

## A note on additional variable construction in `osf_load_analyst_datasets.R` and potential for implicit exclusion

Note that for inclusion in the out-of-sample predictions $y_i$
meta-analysis, parameter tables including the mean and SD for each
response variable are needed standardise the data. If the variable is
not present in either `euc_data` or `blue_tit_data` (i.e. it isn’t
calculated in `osf_load_analyst_datasets.R`, nor present in the original
data files downloaded from the OSF), then analyses using that response
variable are excluded from further meta-analysis. See table below for
constructed variables that are included/excluded in the out-of-sample
predictions meta-analysis:

<details class="code-fold">
<summary>Code</summary>

``` r
library(ManyEcoEvo)
```

</details>

    Warning: replacing previous import 'purrr::%@%' by 'rlang::%@%' when loading
    'ManyEcoEvo'

    Warning: replacing previous import 'purrr::flatten_lgl' by 'rlang::flatten_lgl'
    when loading 'ManyEcoEvo'

    Warning: replacing previous import 'purrr::splice' by 'rlang::splice' when
    loading 'ManyEcoEvo'

    Warning: replacing previous import 'purrr::flatten_chr' by 'rlang::flatten_chr'
    when loading 'ManyEcoEvo'

    Warning: replacing previous import 'purrr::flatten_raw' by 'rlang::flatten_raw'
    when loading 'ManyEcoEvo'

    Warning: replacing previous import 'purrr::flatten' by 'rlang::flatten' when
    loading 'ManyEcoEvo'

    Warning: replacing previous import 'purrr::flatten_dbl' by 'rlang::flatten_dbl'
    when loading 'ManyEcoEvo'

    Warning: replacing previous import 'purrr::invoke' by 'rlang::invoke' when
    loading 'ManyEcoEvo'

    Warning: replacing previous import 'purrr::flatten_int' by 'rlang::flatten_int'
    when loading 'ManyEcoEvo'

    Warning: replacing previous import 'recipes::fixed' by 'stringr::fixed' when
    loading 'ManyEcoEvo'

<details class="code-fold">
<summary>Code</summary>

``` r
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(tibble)
data("ManyEcoEvo")

# Constructed Variables Included in the ManyAnalysts meta-analysis
ManyEcoEvo_constructed_vars <-
  tribble(
    ~response_variable_name,
    "euc_sdlgs_all",
    "euc_sdlgs>50cm",
    "euc_sdlgs0_2m",
    "small*0.25+medium*1.25+large*2.5",
    "euc_sdlgs50cm_2m",
    "average.proportion.of.plots.containing.at.least.one.euc.seedling.of.any.size",
    "day_14_weight/(day_14_tarsus_length^2)",
    "day_14_weight/day_14_tarsus_length",
    "day_14_weight*day_14_tarsus_length"
  )

# Analyst Constructed Variables
all_constructed_vars <-
  ManyEcoEvo %>%
  pull(data, dataset) %>%
  list_rbind(names_to = "dataset") %>%
  filter(str_detect(response_variable_type, "constructed")) %>%
  distinct(response_variable_name) %>%
  drop_na() %>%
  arrange()

by <- join_by(response_variable_name)

all_constructed_vars %>%
  semi_join(ManyEcoEvo_constructed_vars, by) %>%
  mutate(included_in_yi = TRUE) %>%
  bind_rows({
    all_constructed_vars %>%
      anti_join(ManyEcoEvo_constructed_vars, by) %>%
      mutate(included_in_yi = FALSE)
  }) %>%
  knitr::kable(
    col.names = c(
      "Constructed Variable",
      "Included in $y_i$ meta-analysis?"
    ),
    format = "markdown"
  )
```

</details>

| Constructed Variable                                                         | Included in $y_i$ meta-analysis? |
|:-----------------------------------------------------------------------------|:---------------------------------|
| day_14_weight/day_14_tarsus_length                                           | TRUE                             |
| day_14_weight/(day_14_tarsus_length^2)                                       | TRUE                             |
| euc_sdlgs0_2m                                                                | TRUE                             |
| euc_sdlgs_all                                                                | TRUE                             |
| euc_sdlgs\>50cm                                                              | TRUE                             |
| small*0.25+medium*1.25+large\*2.5                                            | TRUE                             |
| average.proportion.of.plots.containing.at.least.one.euc.seedling.of.any.size | TRUE                             |
| SMI                                                                          | FALSE                            |
| day_14_tarsus_length_group_deviation                                         | FALSE                            |
| day_14_weight_group_deviation                                                | FALSE                            |
| PC1.day_14_weight.day_14_tarsus_length                                       | FALSE                            |
| day_14_tarsus_length_deviation                                               | FALSE                            |
| residual_day14_weight                                                        | FALSE                            |
| residual_day_14_weight_males                                                 | FALSE                            |
