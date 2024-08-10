#' Clean response transformation variable
#' @description Cleans the response transformation variable to the names of the back-transformation functions
#'
#' @param response_transformation A character vector with the
#'
#' @return A character vector of cleaned response transformation values equal to the required `transformation` values in `conversion()`
#' @export
#'
#' @family back-transformation functions
clean_response_transformation <- function(response_transformation) {
  original_data <- tibble(transformation_orig = response_transformation)

  transformation_tbl <- tribble(
    ~transformation_orig, ~cleaned_transformation,
    "^2", "square",
    "power2", "square",
    "^3", "cube",
    "power3", "cube",
    "squared", "square",
    "cubed", "cube",
    "scaled and centered", "identity",
    "scaling and centering", "identity",
    "mean centered and standardized", "identity",
    "log", "log",
    "orderNorm", NA, # TODO, ensure that this is the best behaviour - we need to exclude this first, rather than let it through here.. because else it gets passed through identity_back() inside conversion()
    "divided.by.14", "divided.by.14",
    "square.root", "square_root",
    "back.transformed", "back.transformed",
    "z.score", "identity",
    "(power3)/100", "(power3)/100"
  ) # TODO double-check treatment of z.score
  out <- original_data %>%
    left_join(transformation_tbl) %>%
    select(cleaned_transformation) %>% # TODO WHAT ABOUT MISSING NON-STANDARD TRANSFORMATIONS??
    flatten_chr()

  return(out)
  # TODO, document exclusion of Tukey's / Ordered Quantile transformation and other transformations
}
