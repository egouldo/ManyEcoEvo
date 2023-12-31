#' Calculate mean sorensen diversity index values for an analyst dataset
#'
#' @param .data A variable diversity dataset with columns for each unique variable used in all analyses. Values are `NA` if the analysis did not use that variable, and take the value of the variable as a character string if the analysis did use that variable. Each unique analysis is a row.
#' @param .id character string of the analysis identifier column
#'
#' @return A tibble containing the variables `id_col`, `mean_diversity_index` and `num_variables`
#' @export
calculate_sorensen_diversity_index <- function(.data, .id = character()){
  out <- 
    .data %>% 
    column_to_rownames(paste0(.id)) %>% 
    mutate(across(everything(),
                  .fns = ~ case_when(!is.na(.x) ~ 1,
                                     TRUE ~ 0)
    )
    )
  
  num_variables <- rowSums(out, na.rm=TRUE)
  
  out <- out %>% 
    betapart::beta.pair(index.family = "sorensen") %>% 
    pluck("beta.sor") %>% 
    as.matrix() %>% 
    as_tibble(rownames = NA) %>% 
    rownames_to_column("id_col") %>% 
    bind_cols(num_variables = num_variables) %>% 
    filter(num_variables > 0) %>% 
    rowwise() %>% 
    mutate(mean_diversity_index = #TODO why do we take the mean again (remember: these are pairwise comparisons, so we need to reduce down to single value per analysis)
             mean(c_across(cols = -c(num_variables, 
                                     id_col)), na.rm = TRUE)) %>% 
    select(id_col, 
           mean_diversity_index,
           num_variables)
  
  return(out)
}
