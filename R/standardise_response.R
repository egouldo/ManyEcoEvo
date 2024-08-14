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
#' 1. assigns a `transformation_type` with [assign_transformation_type()], assumes that 
#' 2. Converts the out-of-sample predictions on the link- or transformed-response scale back to the original response scale using [convert_predictions()].
#' 3. Standardises predictions on the original response-scale to the Z-scale, with [pred_to_Z()].
#' 
#' Note that for $y_i$ or out of sample predictions that are standardised, if param_table is `NA` or `NULL` for a given variable, then the response variable will not be standardised, and NA will be returned for that entry in `back_transformed_data`.
#'
#' @export
#' @family analyst-data
#' @seealso [est_to_zr(), assign_transformation_type()]
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
    # ------ Convert Effect Sizes to Zr -------
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
  } else { 
    # ------ Convert predictions to Z -------
    cli::cli_h2(paste0("Standardising out-of-sample predictions"))
    
    dat <- dat %>%
      pointblank::col_exists(
        columns =
          pointblank::vars(
            "id_col",
            "augmented_data",
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
                params = ..2, 
                dataset = dataset)
            } else {
              NA
            }
          )
      ) %>% 
      ungroup()
  }
  
  # TODO for any analyses implicitly excluded, return a message to the user
  return(dat)
}

#' Process response data for meta-analysis
#' 
#' @description
#' This function generates the response data for meta-analysis without standardising the effect sizes / out-of-sample predictions.
#' @describeIn process_analyst_data Process response data for meta-analysis but do not standardise effect-sizes
process_response <- function(dat,
                             estimate_type = NULL,
                             param_table = NULL,
                             dataset = NULL){ #TODO what to do about args in pmap_prepare_response? allow ... args in fns(x,y,z, ...)?
  
  Z_names_lookup <- c(Z = "estimate", #blue tit
                      Z = "fit", #eucalyptus
                      VZ = "se.fit") # both datasets
  
  dat %>%  
    mutate(back_transformed_data = 
             map(back_transformed_data, 
                 rename, 
                 any_of(Z_names_lookup)), 
           params = NA)
   #TODO replace dummy function with actual function
}
