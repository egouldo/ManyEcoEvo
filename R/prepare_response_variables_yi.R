#' Prepare response variable data for nested ManyEcoEvo `yi` dataset
#'
#' @param data Complete ManyEcoEvo-style dataset containing nested datasets for each different analysis and exclusion set dataset.
#' @param estimate_type A character string of length 1, equal to either "yi", "y25", "y50", "y75", indicating what type of estimates are being prepared.
#' @param param_table A table of parameters \(mean, sd\) for *most* response variables used by analysts. This tibble is pulled from the named object exported by `ManyEcoEvo::`. but can be overwritten with the users's own `param_table` dataset.
#'
#' @return A tibble of nested list-columns
#' @details Operates on nested list-columns of data. The function back-transforms the response variables from the link to the response scale for each dataset in the ManyEcoEvo dataset. The back-transformed data is stored in a list-column called `back_transformed_data`. It is useful for when wanting to conduct a meta-analysis on the response scale, e.g. for the *Eucalyptus* count data. 
#' `estimate_type` is used to specify the type of estimate to be standardised and parsed to [back_transform_response_vars_yi()]
#' @seealso [back_transform_response_vars_yi()]. To be called prior to [generate_yi_subsets()].
#' @import dplyr
#' @importFrom purrr map2 map
#' @importFrom pointblank expect_col_exists
#' @family targets-pipeline functions
#' @family Multi-dataset Wrapper Functions
#' @export
prepare_response_variables_yi <- function(data) {
  stopifnot(is.data.frame(data))
  
  pointblank::expect_col_exists(data, c("data", "diversity_data"))
  
  out <- data %>%
    ungroup() %>%
    dplyr::mutate(
      data = purrr::map(
        .x = data, 
        .f = prepare_response_variables_yi
      ),
      diversity_data = purrr::map2(
        .x = diversity_data,
        .y = data,
        .f = ~ semi_join(.x, .y) %>% 
          distinct()
      )
    )
  return(out)
}

#' Back-transform analyst `yi` estiamtes to the response scale
#'
#' @param data dataframe of out of sample predictions analyst submission data
#' @return A tibble of analyst data with standardised values contained in a list-column called 'back_transformed_data'
#' @details
#' `data` must contain the columns: `id_col`, `augmented_data`, `transformation`, `response_transformation_status`, where `augmented_data` is a list-column of dataframes containing the analyst predictions, `transformation` is the transformation used in the model, and `response_transformation_status` is the status of the response transformation.
#' 
#' @export
#' @import dplyr
#' @importFrom purrr pmap
#' @importFrom rlang is_na na_lgl
#' @importFrom pointblank col_exists
#' @family Analysis-level functions
#' @family Back-transformation
back_transform_response_vars_yi <- function(data) {
  # TODO insert checks that appropriate columns exist
  # TODO apply to data and check that all cases accounted for!
  
  out <- data %>%
    pointblank::col_exists(
      columns =
        c("id_col",
          "augmented_data",
          "transformation",
          "response_transformation_status"
        )
    ) %>% # add check for response transformation
    dplyr::group_by(id_col) %>% 
    dplyr::mutate(
      transformation_type =
        assign_transformation_type(
          response_transformation = response_transformation_status,
          link_fun = transformation
        )
    ) %>% 
    pointblank::col_exists(columns = "transformation_type") %>%
    dplyr::mutate(
      back_transformed_data =
        purrr::pmap(
          .l = list(
            augmented_data,
            transformation_type, # TODO update, gh issue 162
            response_transformation_status,
            transformation
          ), # TODO update, gh issue 162 #NOTE: see #127 / #38 on GH.
          .f = ~ if (all(!rlang::is_na(..1), !rlang::is_na(..2))) {
            convert_predictions(
              augmented_data = ..1,
              transformation_type = ..2,
              response_transformation = ..3,
              link_fun = ..4
            )
          } else {
            rlang::na_lgl
          }
        )
    ) %>% 
    ungroup()
  
  return(out)
}
