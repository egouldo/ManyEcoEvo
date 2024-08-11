#' Standardise Response Variable
#'
#' @param dat A tibble of analyst data with a list-column called 
#' @param estimate_type The type of estimate to be standardised. Character vector of length 1, whose value may be "Zr", "yi", "y25", "y50", "y75".
#' @param param_table A table of estimated 'population' parameters for each variable in the analysis datasets.
#' @param dataset One of either "blue tit" or "eucalyptus"
#'
#' @return A tibble of analyst data with standardised values contained in a list-column called 'back_transformed_data'
#' @details
#' 
#' When the `estimate_type` is `"Zr"`, [standardise_response()] standardises effect-sizes with [est_to_zr()], assuming that the `beta_estimate` and `beta_SE` values have already been back-transformed to the appropriate scale. #TODO check this.
#' 
#' When the `estimate-type` is `"yi"` or otherwise, the function:
#' 1. assigns a `transformation_type` with [assign_transformation_type()]
#' 2. Converts the out-of-sample predictions on the link- or transformed-response scale back to the original response scale using [convert_predictions()].
#' 3. Standardises predictions on the original response-scale to the Z-scale, with [pred_to_Z()].
#' 
#' Note that for $y_i$ or out of sample predictions that are standardised, if param_table is `NA` or `NULL` for a given variable, then the response variable will not be standardised, and NA will be returned for that entry in `back_transformed_data`.
#'
#' @export
#' @family analyst-data
standardise_response <- function(dat,
                                 estimate_type = character(1L),
                                 param_table = NULL,
                                 dataset = character(1L)) {
  # TODO insert checks that appropriate columns exist
  # TODO apply to data and check that all cases accounted for!
  match.arg(estimate_type, choices = c("Zr", "yi", "y25", "y50", "y75"), several.ok = FALSE)
  match.arg(dataset, choices = c("eucalyptus", "blue tit"), several.ok = FALSE)
  cli::cli_h1(glue::glue("Computing meta-analysis inputs", "for estimate type ", "{estimate_type}"))
  
  if (estimate_type == "Zr") {
    # Convert Effect Sizes to Zr -------
    cli::cli_h2(paste0("Computing standardised effect sizes ", "{.code Zr}", " and variance ", "{.code VZr}"))
    dat <- dat %>%
      # unnest(back_transformed_estimate) %>%
      dplyr::mutate(Zr_VZr = purrr::pmap(
        .l = list(
          beta_estimate = beta_estimate,
          beta_SE = beta_SE,
          adjusted_df = adjusted_df
        ),
        .f = est_to_zr
      )) %>%
      tidyr::unnest(cols = c(Zr_VZr))
  } else { # estimate_type != Zr, i.e. == "y*"
    cli::cli_h2(paste0("Transforming out of sample predictions from link to response scale"))
    dat <- dat %>%
      pointblank::col_exists(
        columns =
          pointblank::vars(
            "TeamIdentifier",
            "submission_id",
            "analysis_id",
            "split_id",
            "augmented_data",
            "transformation",
            "response_transformation_status"
          )
      ) %>% # add check for  response transformation
      dplyr::group_by(
        TeamIdentifier,
        submission_id,
        analysis_id,
        split_id
      ) %>%
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
        transformation_type =
          assign_transformation_type(
            response_transformation = response_transformation_status,
            link_fun = transformation
          )
      ) %>%
      dplyr::mutate(
        back_transformed_data =
          purrr::pmap(
            .l = list(
              augmented_data,
              transformation_type, # TODO update, gh issue 162
              response_transformation_status,
              transformation
            ), # TODO update, gh issue 162 #NOTE: see #127 / #38 on GH.
            .f = ~ if (!rlang::is_na(..1) | !rlang::is_na(..2)) {
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
      )
    
    cli::cli_h2(paste0("Standardising out-of-sample predictions"))
    
    dat <- dat %>%
      dplyr::mutate(
        back_transformed_data = # TODO rename standardised_data and fix up downstream dependencies
          purrr::pmap(
            list(
              back_transformed_data,
              params,
              dataset
            ),
            .f = ~ if (!rlang::is_na(..1) | !rlang::is_na(..2)) {
              pred_to_Z(..1, params = ..2, dataset = ..3)
            } else {
              NA
            }
          )
      )
  }
  
  # TODO for any analyses implicitly excluded, return a message to the user
  return(dat)
}
