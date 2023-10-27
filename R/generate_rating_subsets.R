#' Generate subsets of ManyEcoEvo Data based on Peer Review Ratings
#' @description Generates two subsets of data based on for both complete and partial exclusion datasets for both `yi` and `Zr` estimates.
#' 
#'
#' @param ManyEcoEvo a ManyEcoEvo dataframe containing formatted raw `data`, formatted `diversity_data`, the `estimate_type`, and `dataset`
#'
#' @return A ManyEcoEvo dataframe with added column `exclusion_set` with new subsets of `data` and `diversity_data`
#' @export
#' @family Multi-dataset Wrapper Functions
#' @family targets-pipeline functions
generate_rating_subsets <- function(ManyEcoEvo){
  # NOTE: should be run *after* computing Zr with compute_MA_inputs() 
  
  out  <-   ManyEcoEvo %>% 
    filter(exclusion_set == "complete" | exclusion_set == "partial") %>% 
    mutate(data = 
             map(data, 
                 .f = ~ .x %>%  unnest(review_data) %>% 
                   select(Zr, VZr, 
                          id_col, 
                          PublishableAsIs, 
                          ReviewerId, 
                          TeamIdentifier, 
                          RateAnalysis,
                          mixed_model) %>% 
                   mutate(PublishableAsIs = 
                            forcats::as_factor(PublishableAsIs) %>% 
                            forcats::fct_relevel(c("deeply flawed and unpublishable", 
                                                   "publishable with major revision", 
                                                   "publishable with minor revision", 
                                                   "publishable as is" ))))) %>% 
    mutate(rm_flawed = 
             map(data, 
                 .f = ~ .x %>%  group_by(PublishableAsIs, id_col) %>% 
                   count() %>% 
                   filter(str_detect(PublishableAsIs, pattern = "flawed")) %>% 
                   pull(id_col)),
           rm_flawed_major = 
             map(data, 
                 .f = ~ .x %>%  group_by(PublishableAsIs, id_col) %>% 
                   count() %>% 
                   filter(str_detect(PublishableAsIs, pattern = "flawed|major")) %>% 
                   pull(id_col))) %>% 
    mutate(data_flawed = map2(.x = data, 
                                          .y = rm_flawed,
                                          .f = ~ filter(.x, id_col %nin% .y)),
           data_flawed_major = map2(.x = data, 
                                                .y = rm_flawed_major,
                                                .f = ~ filter(.x, id_col %nin% .y))) %>% 
    group_by(dataset, exclusion_set, estimate_type) %>% 
    select(-data) %>% 
    pivot_longer(cols = c(data_flawed, data_flawed_major),
                 names_to = "publishable_subset",
                 values_to = "data") %>% 
    select(-starts_with("rm_")) %>% 
    mutate(data = map(data,
                      .f = ~ group_by(.x, id_col) %>% 
                        nest(review_data = c(ReviewerId, RateAnalysis, PublishableAsIs)) %>% 
                        ungroup)) %>% 
    mutate(diversity_data = 
             map2(.x = diversity_data, 
                  .y = data, 
                  .f = ~ semi_join(.x, .y) %>% distinct))
  
  # DON"T FORGET WE NEED TO RE DO THE DIVERSITY DATA!! TO DEAL WITH REMOVED DATA POINTS!
  
  # THEN BIND ROWS WITH PREVIOUS DATASETS
  out <- bind_rows(
    ManyEcoEvo %>% 
      mutate(publishable_subset = "All"),
    out
  )
  
  return(out)
}
