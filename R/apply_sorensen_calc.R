#' Applies the sorensen diversity index calculation to variable diversity dataset
#'
#' @param .data A variable diversity dataset with columns for each unique variable used in all analyses. Values are `NA` if the analysis did not use that variable, and take the value of the variable as a character string if the analysis did use that variable. Each unique analysis is a row.
#'
#' @return A tibble containing the variables `id_col`, `mean_diversity_index` and `num_variables`
#' @export
apply_sorensen_calc <- function(.data){
  out <- .data
  
  out %>% 
    select(-dataset) %>% #TODO THEN REMOVE DUPES
    filter(id_col %nin% 
             {out %>% 
                 count(id_col) %>% 
                 filter(n>1) %>% 
                 pull(id_col)}) %>% 
    calculate_sorensen_diversity_index(., "id_col")
}