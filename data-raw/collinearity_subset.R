# ------- Create tibble of analysis IDs of analyses with collinear variables -------

library(tibble)
library(usethis)

collinearity_subset <-
  tibble::tribble(
    ~response_id,         ~id_col,   ~dataset,
    "R_DoCdsvLclGEF14Z", "Armadal-1-1-1", "blue tit",
    "R_eqzPwjXV6eOi2zv", "Babinda-1-1-1", "blue tit",
    "R_eqzPwjXV6eOi2zv", "Babinda-2-2-1", "blue tit",
    "R_2Tzp6JznvkvCY4h",  "Barham-1-1-1", "blue tit",
    "R_2Tzp6JznvkvCY4h",  "Barham-2-2-1", "blue tit",
    "R_126erjKKuN3IwSJ",    "Bega-1-1-1", "blue tit",
    "R_126erjKKuN3IwSJ",    "Bega-1-1-2", "blue tit",
    "R_126erjKKuN3IwSJ",    "Bega-2-2-1", "blue tit",
    "R_126erjKKuN3IwSJ",    "Bega-2-2-2", "blue tit",
    "R_210APF6gYFATf7Y",   "Borde-1-1-1", "blue tit",
    "R_1dzfML4yheLxG0D",    "Bruc-1-1-1", "blue tit",
    "R_3lMQ3NmmjrzpbM2",  "Caigun-1-1-1", "blue tit",
    "R_3lMQ3NmmjrzpbM2",  "Caigun-2-2-1", "blue tit"
  )


usethis::use_data(collinearity_subset, internal = TRUE, overwrite = TRUE)
