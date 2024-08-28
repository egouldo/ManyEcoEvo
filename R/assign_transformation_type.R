#' Assign back-transformation type to be applied to analysts' point-estimates
#'
#' @param response_transformation Character vector of length 1L containing the analysis response transformation
#' @param link_fun Character vector of length 1L containing the analysis link function
#'
#' @return A character vector of length 1L containing the back-transformation type to be applied to the analysts' point-estimates. Is either "identity", "double_transformation", or the value of `link_fun` or `response_transformation`, or `NA`, if an appropriate transformation type cannot be assigned.
#' @details 
#' Based on the response transformation and link function, the function assigns the back-transformation type to be applied to the analysts' point-estimates. The function and assigns the identity transformation if the effects were reported on the link-scale and the estimates are already back-transformed the original response variable prior to modelling. When either of these cases is not true for a given analysis, the function returns the value of the `link_fun` or `response_transformation` argument. When an analysis has been reported on the link-scale and the analyst transformed the response variable prior to modelling, the function assigns the `"double-transformation"`  value for that analysis. When the `response_transformation` and `link_fun` arguments are missing, the function assigns the `"identity"` value to the analysis, assuming that `NA` values are equivalent to the identity transformation. 
#' @export
#' @import dplyr
#' @importFrom rlang is_na
#' @family Back-transformation
#' @seealso [prepare_response_variables_yi(), standardise_response()]. To be called prior to [clean_response_transformation()].
assign_transformation_type <- function(response_transformation = character(1L),
                                       link_fun = character(1L)) {
  # # Link-Fun: Set back.transformed to "identity"
  # link_fun <- dplyr::case_when(
  #   link_fun == "back.transformed" ~ "identity",
  #   TRUE ~ link_fun
  # )
  #
  # # Response Variable Transformation: Set back.transformed to NA
  # response_transformation <- dplyr::case_when(
  #   response_transformation == "back.transformed" ~ NA,
  #   TRUE ~ response_transformation
  # )
  transformation_type <- case_when(
    response_transformation %in% "back.transformed" & link_fun %in% "back.transformed" ~ "identity",
    response_transformation %in% "back.transformed" & link_fun %in% "identity" ~ "identity",
    response_transformation %in% "back.transformed" & is_na(link_fun) ~ "identity",
    response_transformation %in% "back.transformed" & !is_na(link_fun) & link_fun %nin% "identity" & link_fun %nin% "back.transformed" ~ paste(link_fun),
    is_na(response_transformation) & link_fun %in% "back.transformed" ~ "identity",
    is_na(response_transformation) & link_fun %in% "identity" ~ "identity",
    is_na(response_transformation) & is_na(link_fun) ~ "identity",
    is_na(response_transformation) & !is_na(link_fun) & link_fun %nin% "back.transformed" & link_fun %nin% "identity" ~ paste(link_fun),
    !is_na(response_transformation) & response_transformation %nin% "back.transformed" & link_fun %in% "back.transformed" ~ paste(response_transformation),
    !is_na(response_transformation) & response_transformation %nin% "back.transformed" & link_fun %in% "identity" ~ paste(response_transformation),
    !is_na(response_transformation) & response_transformation %nin% "back.transformed" & is_na(link_fun) ~ paste(response_transformation),
    !is_na(response_transformation) & response_transformation %nin% "back.transformed" & is_na(link_fun) & link_fun %nin% "back.transformed" & link_fun %nin% "identity" ~ "double_transformation",
    TRUE ~ na_chr
  )

  return(transformation_type)
}
