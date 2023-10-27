#' Generate Outlier Subsets for ManyAnalysts datasets
#' @description Removes top outlier for `yi` datasets and top 2 and bottom 2 outliers for `Zr` datasets
#'
#' @param ManyEcoEvo a ManyAnalysts dataframe containing formatted raw `data`, formatted `diversity_data`, the `estimate_type`, and `dataset`
#'
#' @return A ManyAnalysts dataframe with added column `exclusion_set` with new subsets of `data` and `diversity_data`
#' @export
#' @family Multi-dataset Wrapper Functions
#' @family targets-pipeline functions
generate_outlier_subsets <- function(ManyEcoEvo){
  # NOTE: should be run *after* computing Zr with compute_MA_inputs() 
  # because the function expects the column 'Zr' to exist in
  # TODO: will nolonger work on Zr dataset, because this doesn't contain an estimate_type col?
  # TODO: Don't run with the reduced publishability subset.... some of these already only have 10 data points!!
  # apply conditional behaviour to trigger both
  # 
  if(str_detect(ManyEcoEvo$estimate_type, "Zr") %>% any(na.rm = TRUE)){
    ManyEcoEvo_Zr <-  ManyEcoEvo %>% 
      filter(estimate_type == "Zr") %>% 
      bind_rows(., {ManyEcoEvo %>% 
          filter(estimate_type == "Zr") %>% 
          mutate(effects_analysis = map(effects_analysis, 
                                        ~ slice_max(.x, Zr, n = -2) %>% 
                                          slice_min(Zr, n = -2))) %>% 
          mutate(exclusion_set = paste0(exclusion_set, "-rm_outliers"),
                 diversity_data = 
                   map2(.x = diversity_data, 
                        .y = data, #TODO should this be effects analysis?? Yes, but no shared variables...
                        .f = ~ semi_join(.x, .y) %>% distinct))}) #TODO duplicates in diversity_data....??
  }
  
  if (str_detect(ManyEcoEvo$estimate_type, "y") %>% any(na.rm = TRUE)) {
    ManyEcoEvo_yi <- ManyEcoEvo %>% filter(str_detect(estimate_type, "y")) %>% 
      bind_rows(.,
                {
                  ManyEcoEvo %>% 
                    filter(str_detect(estimate_type, "y")) %>% 
                    mutate(data = map(data, #TODO check list-column is still called this!
                                      ~ slice_max(.x, Z, n = -1))) %>% #TODO check that downstream functions call on data and not effects analysis!!!
                    mutate(exclusion_set = paste0(exclusion_set, "-rm_outliers"),
                           diversity_data = 
                             map2(.x = diversity_data, 
                                  .y = data, #TODO should this be effects analysis???
                                  .f = ~ semi_join(.x, .y) %>% distinct))
                }
                
      )
  }
  
  out <- if(exists(x = "ManyEcoEvo_Zr") & exists(x = "ManyEcoEvo_yi")){
    bind_rows(ManyEcoEvo_Zr, ManyEcoEvo_yi)
  } else if(exists(x = "ManyEcoEvo_Zr") & !exists(x = "ManyEcoEvo_yi")){
    ManyEcoEvo_Zr
  } else if(!exists(x = "ManyEcoEvo_Zr") & exists(x = "ManyEcoEvo_yi")){
    ManyEcoEvo_yi
  } else{NULL}
  
  return(out)
}