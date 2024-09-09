#' Convert Predictions
#' @description Converts out-of-sample predictions on the link scale back to the response scale
#' @param augmented_data A tibble of out of analyst's sample prediction data for 3 scenarios
#' @return A tibble of out-of-sample predictions on the response variable scale of the response variable used by the analyst
#' @family Analysis-level functions
#' @export
#' @import dplyr
#' @importFrom purrr discard
#' @importFrom rlang is_na
#' @importFrom cli cli_alert_warning cli_abort
#' @importFrom data.table setnames
#' @importFrom pointblank has_columns
#' @importFrom tibble tibble deframe
#' @importFrom purrr discard
#' @importFrom tidyselect any_of
#' @family Back-transformation
#' @seealso [rename_prediction_cols()]
convert_predictions <- function(augmented_data,
                                transformation_type,
                                response_transformation,
                                link_fun) {
  # tar_load(all_prediction_data)
  # convert_predictions(augmented_data = all_prediction_data$augmented_data[[262]],
  # transformation_type = all_prediction_data$transformation_type[[262]])
  # 
  if (rlang::is_na(list(augmented_data))) {
    cli::cli_alert_warning("Missing Value for {.arg augmented_data}, returning {.val NA}")
    out <- rlang::na_cpl
  } else if (rlang::is_na(transformation_type)) {
    cli::cli_alert_warning("Missing Value for {.arg transformation_type}, returning {.val NA}")
    out <- rlang::na_cpl
  } else if (!pointblank::has_columns(augmented_data, "se.fit")) {
    # Before Apply Transformation, check valid inputs
    cli::cli_abort(message = "Data.frame {.arg augmented_data} must contain column {.val se.fit}")
  } else if ("data.frame" %nin% class(augmented_data)) {
    cli::cli_abort(message = "{.arg augmented_data} is not a {.cls data.frame}")
  } else {
    # Define Helper Funs
    vconvert <- Vectorize(
      FUN = conversion,
      vectorize.args = c("beta", "se"),
      USE.NAMES = TRUE,
      SIMPLIFY = "matrix"
    )

    vconvert_double_transformation <- Vectorize(
      FUN = conversion_2,
      vectorize.args = c("beta", "se"),
      USE.NAMES = TRUE,
      SIMPLIFY = TRUE
    )

    # Define input variables to conversion fns

    key_var <- augmented_data %>%
      dplyr::select(dplyr::contains(c("scenario", "SurveyID")))

    beta_vals <- if (names(key_var) == "scenario") {
      # Extracting yi estimates to supply to beta arg in vconvert
      if (!pointblank::has_columns(augmented_data, "estimate")) {
        cli::cli_abort(message = "Blue tit prediction data must contain column {.code estimate}")
      }
      augmented_data$estimate
    } else {
      if (!pointblank::has_columns(augmented_data, "fit")) {
        cli::cli_abort(message = "Eucalyptus prediction data must contain column {.code fit}")
      }
      augmented_data$fit
    }

    # Apply Conversion w Helper Funs & Input Variables

    converted <- if (transformation_type %nin% "double_transformation") {
      vconvert(
        beta = beta_vals,
        se = augmented_data$se.fit,
        transformation = transformation_type,
        sim = 10000
      ) %>%
        t()
    } else { # Back-transform response AND link-function

      if (any(rlang::is_na(response_transformation), rlang::is_na(link_fun))) {
        cli::cli_alert_warning("Missing Value for {.arg response_transformation}, returning {.val NA}")
        out <- rlang::na_cpl
      }

      vconvert_double_transformation(
        beta = beta_vals,
        se = augmented_data$se.fit,
        response_transformation = response_transformation,
        link_fun = link_fun,
        sim = 10000
      ) %>%
        t()
    }

    # reshape conversion outputs

    out <- converted %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(across(.cols = everything(), .fns = as.double)) %>%
      rename_prediction_cols(key_var, augmented_data) %>%
      dplyr::bind_cols(key_var, .) %>%
      dplyr::ungroup()
  }

  return(out)
}

#' Rename Prediction Columns
#' @description Renames the prediction columns in the output of convert_predictions
#' @param .data A tibble of out of sample predictions on the response variable scale
#' @param key_var A tibble of the key variables used in the conversion
#' @param .old_data A tibble of the original data used in the conversion
#' @return A tibble of out of sample predictions on the response variable scale with the correct column names
#' @family Analysis-level functions
#' @family Back-transformation
#' @importFrom data.table setnames
#' @importFrom purrr discard
#' @export
rename_prediction_cols <- function(.data, key_var, .old_data) {
  if (names(key_var) == "scenario") {
    .data %>%
      data.table::setnames(
        old = colnames(.data), # TODO: is this order correct? .old_data is being used for the new variable..
        new = colnames(.old_data) %>% purrr::discard(~ .x == "scenario")
      )
  } else if (names(key_var) == "SurveyID") {
    .data %>%
      data.table::setnames(
        old = colnames(.data), # TODO: is this order correct? .old_data is being used for the new variable..
        new = colnames(.old_data) %>% purrr::discard(~ .x == "SurveyID")
      )
  } else {
    NA
  }
  
  return(.data)
}
