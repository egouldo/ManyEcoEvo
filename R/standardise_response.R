#' Process Response Data for Meta-Analysis
#' 
#' @param data A tibble of analyst data with a list-column called
#' @param ... Ignored
#' @importFrom cli cli_h1 cli_h2 
#' @import dplyr
#' @importFrom tidyr unnest
#' @importFrom purrr pmap map2 map_int map
#' @importFrom rlang is_na
#' @importFrom pointblank col_exists vars
#' @name process_analyst_data
NULL
#> NULL

#' Standardise Response Variable
#' 
#' @param estimate_type The type of estimate to be standardised. Character vector of length 1, whose value may be "Zr", "yi", "y25", "y50", "y75".
#' @param param_table A table of estimated 'population' parameters for each variable in the analysis datasets.
#' @param dataset Character vector of length 1. The name of the dataset being processed, e.g. `blue tit` or `eucalyptus`.
#' @return A tibble of analyst data with standardised values contained in a list-column called 'back_transformed_data'
#' @details
#' # `standardise_response()`
#'
#' When the `estimate_type` is `"Zr"`, [standardise_response()] standardises 
#' effect-sizes with [est_to_zr()], assuming that the `beta_estimate` and 
#' `beta_SE` values have already been back-transformed to the appropriate scale. #TODO check this.
#' 
#' When the `estimate-type` is `"yi"` or otherwise, the function:
#' 1. assigns a `transformation_type` with [assign_transformation_type()], 
#' assumes that 
#' 2. Converts the out-of-sample predictions on the link- or transformed-response 
#' scale back to the original response scale using [convert_predictions()].
#' 3. Standardises predictions on the original response-scale to the Z-scale, with [pred_to_Z()].
#' 
#' Note that for $y_i$ or out of sample predictions that are standardised, 
#' if param_table is `NA` or `NULL` for a given variable, then the response 
#' variable will not be standardised, and NA will be returned for that entry in `back_transformed_data`.
#' @export
#' @describeIn process_analyst_data Standardise response data for meta-analysis
#' @examples
#' # Standardise effect-sizes for eucalyptus dataset
#' 
#' data(ManyEcoEvo)
#' ManyEcoEvo %>%
#'  filter(dataset == "eucalyptus") %>%
#'  pluck("data", 1) %>%
#'  standardise_response(estimate_type = "Zr", 
#'                       param_table =  NULL,
#'                       dataset =  "eucalyptus")
standardise_response <- function(data,
                                 estimate_type = character(1L),
                                 param_table = NULL,
                                 dataset = character(1L),
                                 ...) {
  stopifnot(is.data.frame(data),
            is.character(dataset))
  
  # TODO insert checks that appropriate columns exist
  # TODO apply to data and check that all cases accounted for!
  match.arg(estimate_type, 
            choices = c("Zr", "yi", "y25", "y50", "y75"), 
            several.ok = FALSE)
  
  cli::cli_h1(c("Computing meta-analysis inputs", 
                "for {.arg estimate_type} = ", 
                "{.val {estimate_type}}"))
  
  if (estimate_type == "Zr") {
    
    # ------ Convert Effect Sizes to Zr -------
    
    cli::cli_h2(paste0("Computing standardised effect sizes ", 
                       "{.code Zr}", 
                       " and variance ", "{.code VZr}"))
    
    data <- data %>%
      dplyr::mutate(
        Zr_VZr = purrr::pmap(
          .l = list(
            beta_estimate = beta_estimate,
            beta_SE = beta_SE,
            adjusted_df = adjusted_df
          ),
          .f = est_to_zr
        )) %>%
      tidyr::unnest(cols = c(Zr_VZr))
  } else { 
    
    # ------ Convert predictions to Z -------
    
    cli::cli_h2(paste0("Standardising out-of-sample predictions"))
    
    data <- data %>%
      pointblank::col_exists(
        columns =
          c(
            "id_col",
            "back_transformed_data",
            "response_variable_name"
          )
      ) %>% # add check for  response transformation
      dplyr::group_by(id_col) %>%
      dplyr::mutate(params = 
                      purrr::map(
                        .x = response_variable_name,
                        .y = param_table,
                        .f = ~ dplyr::filter(.y, variable == .x)
                      )) %>%
      dplyr::mutate(nrow_params = 
                      purrr::map_int(params, nrow)) %>%
      dplyr::mutate(params = 
                      purrr::map2(params,
                                  nrow_params,
                                  .f = ~ if (.y > 0) {
                                    .x
                                  } else {
                                    NA
                                  }
                      )) %>%
      dplyr::select(-nrow_params) %>% 
      dplyr::mutate(
        back_transformed_data = # TODO rename standardised_data and fix up downstream (and upstream wrappers, when not standardised) dependencies
          purrr::pmap(
            list(
              back_transformed_data,
              params
            ),
            .f = ~ if (all(!rlang::is_na(..1), !rlang::is_na(..2))) {
              pred_to_Z(
                back_transformed_data = ..1, 
                params = ..2)
            } else {
              NA
            }
          )
      ) %>% 
      ungroup()
  }
  # TODO for any analyses implicitly excluded, return a message to the user
  return(data)
}

#' Process response data for meta-analysis
#' 
#' @description
#' This function generates the response data for meta-analysis without standardising the effect sizes / out-of-sample predictions.
#' @describeIn process_analyst_data Process response data for meta-analysis but do not standardise effect-sizes
#' @details
#' # `process_response()`
#' 
#' Formats tibbles in the list-column `back_transformed_data` to ensure that the 
#' correct columns are present for meta-analysis, matching the outputs of
#'  [standardise_response()]. For blue tit data `data$back_transformed_data$fit` 
#'  and for eucalyptus data, `data$back_transformed_data$estimate` is renamed `Z`.
#'  `se.fit` is renamed `VZ`.
#' @import dplyr
#' @importFrom purrr map
process_response <- function(data, ...){
  
  Z_names_lookup <- c(Z = "estimate", #blue tit
                      Z = "fit", #eucalyptus
                      VZ = "se.fit",
                      lower = "ci.low",
                      upper = "ci.hi") # both datasets
  
  data %>%  
    mutate(back_transformed_data = 
             map(back_transformed_data, 
                 rename, 
                 any_of(Z_names_lookup)), 
           params = NA)
}

#' Log-transform out-of-sample predictions data for meta-analysis
#' 
#' @param sim a numeric vector of length 1L with the number of simulations that should be passed to [log_transform()]
#' @details
#' # [log_transform_response()]
#' 
#' maps [log_transform_yi()] onto back-transformed data stored in list-columns within [data]
#' @examples
#' data(ManyEcoEvo_yi)
#' ManyEcoEvo_yi %>%
#' filter(dataset == "eucalyptus") %>%
#'   pluck("data", 1) %>%
#'   back_transform_response_vars_yi() %>%
#'   log_transform_response()
#' @export
#' @import dplyr
#' @importFrom cli cli_h1 cli_h2
#' @importFrom pointblank col_exists
#' @importFrom purrr map
#' @importFrom rlang is_na
#' @describeIn process_analyst_data Standardise response data for meta-analysis
log_transform_response <- function(data, sim = 10000L, ...) {
  # TODO insert checks that appropriate columns exist
  # TODO apply to data and check that all cases accounted for!
  stopifnot(is.data.frame(data))
  
  cli::cli_h1(c("Computing meta-analysis inputs:"))
  cli::cli_h2(c("Log-transforming response-variable"))
  
  out <- data %>%
    pointblank::col_exists(
      columns = c(
        "id_col",
        "back_transformed_data"
      )
    ) %>% 
    dplyr::mutate(back_transformed_data = 
             purrr::map(
               .x = back_transformed_data,
               .f = ~ if (!rlang::is_na(.x)) {
                 log_transform_yi(.x, sim = sim)
               } else {
                 NA
               }
             ), 
           .by = "id_col")
  
  return(out)
}
