#' Exclude extreme values of VZ from a dataframe of standardised predictions
#'
#' @param df A dataframe containing the columns `Z` and `VZ`
#' @param VZ_cutoff A numeric vector of length 1, values equal to or greater than this value of VZ will be filtered out of `df`.
#'
#' @return A dataframe with observations removed where the value is less than that of `VZ\_cutoff`.
#' @export
exclude_extreme_VZ <- function(df = data.frame(), VZ_cutoff = numeric(1L)) {
  pointblank::col_exists(df, "VZ")

  df_out <- df %>%
    dplyr::filter(VZ < !!{{ VZ_cutoff }})

  cli::cli_alert(paste0(
    nrow(df) - nrow(df_out),
    " extreme values of VZ removed at threshold of ",
    "{.val {VZ_cutoff}}."
  ))

  return(df_out)
}
