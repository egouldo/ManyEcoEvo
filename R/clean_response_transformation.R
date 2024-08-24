#' Clean response transformation variable
#' @description Cleans the response transformation variable to the names of the back-transformation functions
#'
#' @param response_transformation A character vector with the response transformation values
#' @param transformation_tbl A tibble of the response transformation values `transformation_orig` and their cleaned names `cleaned_transformation`
#' 
#'
#' @return A character vector of cleaned response transformation values equal to the required `transformation` values in `conversion()`
#' @export
#' @details
#' The `transformation_tbl` is a tibble of the response transformation values `transformation_orig` and their cleaned
#' names `cleaned_transformation`. The `transformation_orig` values are the original response transformation values
#' used by the analyst. The `cleaned_transformation` values are the cleaned response transformation values that are equal to the required `transformation` values in [conversion()]. 
#' The user can supply an alternate table of transformations depending on what is required for the back-transformation functions.
#' back-transformation
#' @seealso To be called after to [assign_transformation_type()]
#' @examples
#' clean_response_transformation("power2", ManyEcoEvo:::transformation_tbl) 
#' clean_response_transformation("log", ManyEcoEvo:::transformation_tbl)
#' clean_response_transformation("new_transformation", ManyEcoEvo:::transformation_tbl ) # Returns NA if not found
clean_response_transformation <- function(response_transformation, 
                                          transformation_tbl = ManyEcoEvo:::transformation_tbl) {
  original_data <- tibble(transformation_orig = response_transformation)
  
  out <- original_data %>%
    left_join(transformation_tbl, by = join_by(transformation_orig)) %>%
    select(cleaned_transformation) %>% # TODO WHAT ABOUT MISSING NON-STANDARD TRANSFORMATIONS??
    flatten_chr()

  return(out)
  # TODO, document exclusion of Tukey's / Ordered Quantile transformation and other transformations
}
