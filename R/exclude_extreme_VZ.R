#' Exclude extreme values of VZ from a dataframe of standardised predictions
#'
#' @param df A dataframe containing processed out-of-sample prediction values
#' @param VZ_cutoff A numeric vector of length 1, values equal to or greater than this value of VZ will be filtered out of `df`.
#' @param VZ_colname A character vector corresponding to the column name in `df` containing the VZ values to filter on.
#' @return A dataframe with observations removed where the value is less than that of `VZ_cutoff`.
#' @export
#' @import dplyr
#' @importFrom pointblank col_exists
#' @importFrom cli cli_alert
#' @importFrom tidyselect all_of
exclude_extreme_VZ <- function(df = data.frame(), VZ_colname, VZ_cutoff = numeric(1L)) {
  
  # ----- Argument Checking -----
  pointblank::col_exists(df, VZ_colname)
  stopifnot(
    length(VZ_cutoff) == 1, 
    is.numeric(VZ_cutoff),
    is.data.frame(df)
  )
  
  # ----- filter out extreme values -----
  cli::cli_h2("Excluding extreme values of VZ")
  df_out <- df %>%
    dplyr::filter(if_any(.cols = all_of(VZ_colname), 
                         .fns = ~ .x < !!{{VZ_cutoff}}))
  
  # ---- Format output ----
  if ( all(!is.null(cur_data()), !is.null(cur_group_id()))) {
    dataset <- cur_data() %>% slice(cur_group_id()) %>% pluck("dataset", 1)
    estimate_type <- cur_data() %>% slice(cur_group_id()) %>% pluck("estimate_type", 1)
  } else {
    dataset <- "unknown"
    estimate_type <- "unknown"
  }
  
  cli::cli_alert(paste0(
    nrow(df) - nrow(df_out),
    " extreme values of {.arg {VZ_colname}} removed at threshold of ",
    "{.val {VZ_cutoff}} for {.arg dataset} {.val {dataset}}, ",
    "{.arg estimate_type} = {.val {estimate_type}}."
  ))
  
  return(df_out)
}
