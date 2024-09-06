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
#' @importFrom purrr map2
#' @family targets-pipeline functions
#' @family Multi-dataset Wrapper Functions
#' @export
prepare_response_variables_yi <- function(data,
                                          estimate_type = character(1L),
                                          param_table = NULL) {
  stopifnot(is.data.frame(data))
  # TODO run checks on data
  match.arg(estimate_type, choices = c("yi", "y25", "y50", "y75"), several.ok = FALSE)
  
  out <- data %>%
    ungroup() %>%
    dplyr::mutate(
      data = purrr::map2(
        .x = data, .y = dataset,
        .f = ~ back_transform_response_vars_yi(
          dat = .x,
          estimate_type = !!{
            estimate_type
          },
          param_table,
          dataset = .y
        )
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
#' @param estimate_type The type of estimate to be standardised. Character vector of length 1, whose value may be  "yi", "y25", "y50", "y75".
#' @param dataset One of either "blue tit" or "eucalyptus"
#'
#' @return A tibble of analyst data with standardised values contained in a list-column called 'back_transformed_data'
#' @export
#' @import dplyr
#' @importFrom purrr pmap
#' @importFrom rlang is_na na_lgl
#' @importFrom pointblank col_exists
#' @family Analysis-level functions
#' @family Back-transformation
back_transform_response_vars_yi <- function(data,
                                            estimate_type = character(1L),
                                            dataset = character(1L)) {
  # TODO insert checks that appropriate columns exist
  # TODO apply to data and check that all cases accounted for!
  match.arg(estimate_type, choices = c("yi", "y25", "y50", "y75"), several.ok = FALSE)
  
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
