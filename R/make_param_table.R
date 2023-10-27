#' Make paramater table
#' 
#' `make_param_table()` calculates the mean and standard deviation of 
#' all numeric variables in a dataframe or tibble.
#' 
#' @details The parameter table is used in the computation of Z-values during the standardisation of out-of-sample predictions with `pred_to_Zr()`. `make_param_table()` returns a tidy, long-form tibble with the variable names of `analysis_data` stored in column`variable`, the corresponding `parameter` ("mean" or "sd"), and the `value` of that `parameter`. 
#' 
#' @note Currently variable names of `analysis_data` must not contain `.` because this character is used to split the `variable` from the `parameter` during `pivot_longer()`
#' @export
#' @param analysis_data A dataframe or tibble.
#' @return A tibble.
#' @examples 
#' make_param_table(ManyAnalysts::blue_tit_data)
#' make_param_table(ManyAnalysts::euc_data)
make_param_table <- function(analysis_data){
  # calc SD + mean for ALL continuous variables
  # The table will then be used in the standardisation of 
  # prediction estimate, pred_to_Z.
  # 
  # 
  out <- analysis_data %>% 
    ungroup %>% 
    summarise(across(.cols = where(is.numeric), 
                     .fns = list(mean = mean, sd = sd),
                     na.rm = TRUE,
                     .names = "{.col}--{.fn}")) %>% 
    pivot_longer(cols = everything(),
                 names_sep = "--",
                 names_to = c("variable", "parameter"), 
                 values_to = "value")
  return(out)
} #TODO calculate additional constructed variables coded by THP
