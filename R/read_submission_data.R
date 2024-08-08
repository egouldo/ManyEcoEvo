#' Read out-of-sample-prediction analyst submission data
#'
#' @param filepath
#'
#' @return A tibble of out of sample prediction data
#' @export
read_submission_data <- function(filepath) {
  if (!file.exists(filepath)) {
    out <- NA
    cli::cli_alert_warning("File {filepath} does not exist, returning NA")
  }
  # TODO replace with stricter heuristic once files added
  # if(!file.exists(filepath)){
  #   cli::cli_abort("File {filepath} does not exist")
  # }

  if (str_detect(filepath, ".csv")) {
    cli::cli_alert("freading file {filepath}")
    out <- data.table::fread(filepath) %>%
      as_tibble()
  } else if (str_detect(filepath, ".xlsx")) {
    out <- readxl::read_excel(filepath) %>% as_tibble()
  } else if (str_detect(filepath, "R_2tarci531JBsPED")) {
    out <- readr::read_csv(filepath) %>% as_tibble()
  } else if (stringr::str_detect(filepath, ".zip")) {
    out <- NA
  } else {
    out <- NA
  }

  return(out)
}
