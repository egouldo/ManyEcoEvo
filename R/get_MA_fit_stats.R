#' Extract meta-analytic statistics like \eqn{I^2}, etc.
#' @param MA_mod a fitted model of class rma.mv
#' @return A tidy tibble with \eqn{\sigma^2} and \eqn{I^2} estimates for `MA_mod`
#' @export
#' @importFrom orchaRd i2_ml
#' @importFrom purrr pluck set_names
#' @importFrom tibble as_tibble_row
#' @import dplyr
get_MA_fit_stats <- function(MA_mod) {
  
  stopifnot("MA_mod must be an object of class `rma.mv`" = "rma.mv" %in% class(MA_mod))
  
  res <- 
    bind_cols(
      pluck(MA_mod, "sigma2") %>%
        set_names("sigma2_1", "sigma2_2") %>%
        as_tibble_row(),
      orchaRd::i2_ml(MA_mod) %>%
        as_tibble_row()
    )
  
  return(res)
}
