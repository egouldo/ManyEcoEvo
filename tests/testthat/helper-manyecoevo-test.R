library(dplyr)
library(purrr)

create_manyecoevo_test <- function(
  x = ManyEcoEvo,
  datasets = c("blue tit", "eucalyptus"),
  n = 25,
  seed = 1
) {
  key_cols <- c(
    "id_col",
    "TeamIdentifier",
    "beta_estimate",
    "beta_SE",
    "sample_size",
    "adjusted_df",
    "dataset",
    "mixed_model"
  )

  set.seed(seed)

  x |>
    filter(dataset %in% datasets) |>
    mutate(
      data = map(
        data,
        ~ .x |>
          filter(if_all(all_of(key_cols), ~ !is.na(.))) |>
          arrange(TeamIdentifier, analysis_id) |>
          slice_sample(n = min(n, n()))
      ),
      diversity_data = map2(
        diversity_data,
        data,
        ~ semi_join(.x, .y, by = "id_col")
      )
    )
}

ManyEcoEvo_test <- create_manyecoevo_test()
