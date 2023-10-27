#' Apply VZ exclusion to a data-frame containing list-columns of yi subsets
#'
#' @param df A dataframe of yi data subsets generated from `generate_yi_subsets\(\)`. 
#' @param VZ_cutoff A numeric vector of length 1, values equal to or greater than this value of VZ will be filtered out of the dataframes stored in `df`'s list-column `data`.
#'
#' @return A dataframe of yi subsets, whose extreme values of VZ have been removed.
#' @export
#' @family Multi-dataset Wrapper Functions
apply_VZ_exclusions <- function(df = data.frame(), VZ_cutoff = numeric(1L)){
  pointblank::col_exists(df,columns =  c("data", "diversity_data"))
  
  df_out <- df %>% 
    mutate(data = map(data, exclude_extreme_VZ, !!{{VZ_cutoff}}), #TODO check whether we should run on effects_analysis instead of data
           diversity_data = map2(.x = diversity_data, 
                                 .y = data,
                                 .f = ~ semi_join(.x, .y, by = "id_col")))
  return(df_out)
}

