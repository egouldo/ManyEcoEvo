#' Generate Outlier Subsets for ManyEcoEvo datasets
#' @description Removes top outlier for `yi` datasets and top 2 and bottom 2 outliers for `Zr` datasets
#'
#' @param data a ManyEcoEvo dataframe containing formatted raw `data`, formatted `diversity_data`, the `estimate_type`, and `dataset`
#'
#' @return A ManyEcoEvo dataframe with added column `exclusion_set` with new subsets of `data` and `diversity_data`
#' @export
#' @family Multi-dataset Wrapper Functions
#' @family targets-pipeline functions
generate_outlier_subsets <- function(data, n_min = numeric(1L), n_max = numeric(1L)) {
  # NOTE: should be run *after* computing Zr with compute_MA_inputs()
  # because the function expects the column 'Zr' to exist in
  # TODO: will nolonger work on Zr dataset, because this doesn't contain an estimate_type col?
  # TODO: Don't run with the reduced publishability subset.... some of these already only have 10 data points!!
  # apply conditional behaviour to trigger both
  # TODO: do not run for collinearity_removed datasets
  if (str_detect(data$estimate_type, "Zr") %>% any(na.rm = TRUE)) {
    data_Zr <- data %>%
      filter(estimate_type == "Zr") %>%
      bind_rows(., {
        data %>%
          filter(estimate_type == "Zr", collinearity_subset != "collinearity_removed") %>%
          mutate(effects_analysis = 
                   map(
                     effects_analysis,
                     ~ slice_max(.x, Zr, n = n_max) %>%
                       slice_min(Zr, n = n_min)
                   )) %>%
          mutate(
            exclusion_set = paste0(exclusion_set, "-rm_outliers"),
            diversity_data =
              map2(
                .x = diversity_data,
                .y = effects_analysis,
                .f = ~ semi_join(.x, .y, by = join_by(id_col == study_id)) %>% 
                  distinct()
              ),
            diversity_indices =
              map2(
                .x = diversity_indices,
                .y = effects_analysis,
                .f = ~ semi_join(.x, .y, by = join_by(id_col == study_id)) %>% 
                  distinct()
              )
          )
      }) # TODO duplicates in diversity_data....??
  }
  
  if (str_detect(data$estimate_type, "y") %>% 
      any(na.rm = TRUE)) {
    data_yi <- data %>%
      filter(str_detect(estimate_type, "y")) %>%
      bind_rows(., {
        data %>%
          filter(str_detect(estimate_type, "y")) %>%
          mutate(data = map(
            data, # TODO check list-column is still called this!
            ~ slice_max(.x, Z, n = n_max)
          )) %>% # TODO check that downstream functions call on data and not effects analysis!!!
          mutate(
            exclusion_set = paste0(exclusion_set, "-rm_outliers"),
            diversity_data =
              map2(
                .x = diversity_data,
                .y = effects_analysis, 
                .f = ~ semi_join(.x, .y, by = join_by(id_col == study_id)) %>% 
                  distinct()
              ),
            diversity_indices =
              map2(
                .x = diversity_indices,
                .y = effects_analysis,
                .f = ~ semi_join(.x, .y, by = join_by(id_col == study_id)) %>% 
                  distinct()
              )
          )
      })
  }
  
  out <- if (exists(x = "data_Zr") & exists(x = "data_yi")) {
    bind_rows(data_Zr, data_yi)
  } else if (exists(x = "data_Zr") & !exists(x = "data_yi")) {
    data_Zr
  } else if (!exists(x = "data_Zr") & exists(x = "data_yi")) {
    data_yi
  } else {
    NULL
  }
  
  return(out)
}
