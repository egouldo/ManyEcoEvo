#' Validating analyst-submitted predictions
#' @description
#' Validates the structure of the analyst-submitted predictions, ensuring that the required columns are present and in the correct format.
#' @param input Either a filepath or a dataframe, corresponding to the `type` argument specification
#' @param type character string of length 1, equal to either "filepath" or "df". Defaults to "filepath"
#' @return A pointblank agent, class `ptblank_agent`, yet to be interrogated
#' @family Out-of-Sample Prediction Validation
#' @name validate
#' @importFrom pointblank create_agent col_exists col_is_integer col_is_numeric col_is_character col_vals_in_set action_levels warn_on_fail vars
#' @importFrom fs file_exists
#' @importFrom readr read_csv
#' @importFrom stringr str_split
#' @importFrom purrr pluck
#' @seealso [pointblank::create_agent()]
NULL
#> NULL

#' Validate out of sample predictions for blue tit dataset with pointblank package
#' @export
#' @describeIn validate Validate Blue tit predictions data
validate_predictions_df_blue_tit <- function(input, type = "filepath") {
  match.arg(arg = type, choices = c("filepath", "df"), several.ok = FALSE)

  if (type == "filepath") {
    if (fs::file_exists(input)) {
      dat <- read_csv(input) # TODO, need conditional readin fn based on extension?
      tbl_name <- str_split(input, pattern = "~") %>% pluck(1, 2)
    } else {
      (NA)
    }
  } else if (type == "df") {
    dat <- input
    tbl_name <- as.character(substitute(input))
  }

  predictions_agent_blue_tit <-
    pointblank::create_agent(
      tbl = dat, tbl_name = tbl_name,
      label = "Predictions File Data Structure QA - Blue tit dataset"
    ) %>%
    col_exists(
      columns =
        vars(scenario, estimate, se.fit),
      actions = warn_on_fail(),
      label = "Check required columns exist"
    ) %>%
    col_exists(
      columns = vars(ci.low, ci.hi),
      actions = action_levels(notify_at = 1),
      label = "Check CI cols exist (optional)"
    ) %>%
    col_is_integer(
      columns = vars(scenario),
      actions = warn_on_fail(),
      label = "Check column `scenario` is integer"
    ) %>%
    col_is_numeric(
      columns = vars(estimate, se.fit, ci.low, ci.hi),
      actions = warn_on_fail(),
      label = "Check remaining columns are numeric"
    ) %>%
    col_vals_in_set(
      columns = vars(scenario),
      set = c(1:3),
      label = "Check `scenario` is 1, 2 or 3",
      actions = action_levels(notify_at = 1)
    )

  return(predictions_agent_blue_tit)
}

#' Validate out of sample predictions for eucalytpus dataset with pointblank package
#'
#' @describeIn validate Validate *Eucalyptus* predictions data
#' @family Out-of-Sample Prediction Validation
validate_predictions_df_euc <- function(input, type = "filepath") {
  match.arg(arg = type, choices = c("filepath", "df"), several.ok = FALSE)

  if (type == "filepath") {
    if (fs::file_exists(input)) {
      dat <- read_csv(input) # TODO, need conditional readin fn based on extension?
      tbl_name <- str_split(input, pattern = "~") %>% pluck(1, 2)
    } else {
      (NA)
    }
  } else if (type == "df") {
    dat <- input
    tbl_name <- as.character(substitute(input))
  }

  predictions_agent_euc <-
    pointblank::create_agent(
      tbl = dat, tbl_name = tbl_name,
      label = "Predictions File Data Structure QA  - Eucalyptus dataset"
    ) %>%
    col_exists(
      columns =
        vars(SurveyID, fit, se.fit),
      actions = warn_on_fail(),
      label = "Check required columns exist"
    ) %>%
    col_exists(
      columns = vars(ci.low, ci.hi),
      actions = action_levels(notify_at = 1),
      label = "Check CI cols exist (optional)"
    ) %>%
    col_is_character(
      column = vars(SurveyID),
      label = "Check SurveyID is character"
    ) %>%
    col_is_numeric(
      columns = vars(fit, se.fit, ci.low, ci.hi),
      actions = warn_on_fail(),
      label = "Check remaining columns are numeric"
    ) %>%
    col_vals_in_set(
      columns = vars(SurveyID),
      set = c("Q1", "Q2", "Q3"),
      label = "Check Survey ID is Q1, Q2 or Q3",
      actions = action_levels(notify_at = 1)
    )

  return(predictions_agent_euc)
}

#' Validate predictions conditioned on dataset
#'
#' @param data_set the dataset being analysed, either "blue tit" or "eucalyptus"
#' @export
#' @describeIn validate Wrapper-function for [validate_predictions_df_euc()] and [validate_predictions_df_blue_tit()]
#' @family Out-of-Sample Prediction Validation
validate_predictions <- function(data_set, input, type = "filepath") { # TODO change `data` to `filepath` for semantic accuracy
  # I have written a separate function to control the conditional application of the validation
  # because using if else doesn't return the full data from the agent interrogation
  match.arg(arg = type, choices = c("filepath", "df"), several.ok = FALSE)
  match.arg(arg = data_set, choices = c("eucalyptus", "blue tit"), several.ok = FALSE)

  if (data_set == "eucalyptus") {
    out <- validate_predictions_df_euc(input, type)
  } else {
    (
      out <- validate_predictions_df_blue_tit(input, type)
    )
  }

  return(out)
}
