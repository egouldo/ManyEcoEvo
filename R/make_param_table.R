#' Make parameter table
#'
#' Calculates the mean and standard deviation of
#' all numeric variables in a dataframe or tibble.
#'
#' @export
#' @param analysis_data A dataframe or tibble.
#' @param na.rm Logical. Should missing values be removed?
#' @return A tibble.
#' @examples
#' make_param_table(ManyEcoEvo::blue_tit_data)
#' make_param_table(ManyEcoEvo::euc_data)
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @details The parameter table is used in the computation of Z-values during the standardisation of out-of-sample predictions with `pred_to_Zr()`. `make_param_table()` returns a tidy, long-form tibble with the variable names of `analysis_data` stored in column`variable`, the corresponding `parameter` ("mean" or "sd"), and the `value` of that `parameter`.
#'
#' @details # Note
#' Currently variable names of `analysis_data` must not contain `.` because this character is used to split the `variable` from the `parameter` during `pivot_longer()`
#' @seealso The table from this function is used to standardize out-of-sample predictions \eqn{y_i} in [pred_to_Z()]
make_param_table <- function(analysis_data, na.rm = TRUE) {
  out <- analysis_data %>%
    ungroup() %>%
    summarise(across(
      .cols = where(is.numeric),
      .fns = list(mean = mean, sd = sd),
      na.rm = na.rm,
      .names = "{.col}--{.fn}"
    )) %>%
    pivot_longer(
      cols = everything(),
      names_sep = "--",
      names_to = c("variable", "parameter"),
      values_to = "value"
    )
  return(out)
}
