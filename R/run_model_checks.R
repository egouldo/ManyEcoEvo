#' Perform model checking on series of fitted models for different datasets, exclusion sets and estimate types
#' @description Checks for convergence, singularity and heteroscedasticy
#'
#' @param data A series of fitted models stored in list-columns for different `model_name`s, `exclusion_set`s, `dataset`s and `estimate_type`s
#' @param model_name character string of model name to be extracted from the nested data frame. String must be contained in `data$model_name`
#'
#' @return A tibble whose columns contain the outcomes of different model checks
#' @export
run_model_checks <- function(data, model_name = character(0L)) {
  stopifnot(is.data.frame(data),
            is.character(model_name))
  
  pointblank::col_exists(data,
    columns = c(model_name, "exclusion_set", "dataset", "estimate_type")
  )
  match.arg(model_name, choices = unique(data$model_name))

  # TODO, write possible() wrappers for all performance check funs
  poss_check_heteroscedasticity <- purrr::possibly(performance::check_heteroscedasticity, otherwise = NA)

  list(
    {
      data %>%
        filter(model_name == !!{
          model_name
        }) %>%
        unite(col = "id", exclusion_set, dataset, estimate_type) %>%
        pull(model, name = id) %>%
        map_if(
          .x = .,
          .p = ~ any(class(.x) %in% "model_fit"),
          .f = ~ pluck(.x, "fit") %>% # for class _glm, req data is in .$fit
            pluck("converged"),
          .else = ~ performance::check_convergence(.x)
        ) %>%
        enframe(name = "dataset", value = "converged") %>%
        mutate(converged = flatten_lgl(converged))
    },
    {
      data %>%
        filter(model_name == !!{
          model_name
        }) %>%
        unite(col = "id", exclusion_set, dataset, estimate_type) %>%
        pull(model, name = "id") %>%
        map_if(
          .x = .,
          .p = ~ any(class(.x) %in% "model_fit"),
          .f = ~ pluck(.x, "fit") %>%
            performance::check_singularity(),
          .else = ~ performance::check_singularity(.x)
        ) %>%
        enframe(name = "dataset", value = "is_singular") %>%
        mutate(is_singular = flatten_lgl(is_singular))
    },
    { # Heteroscedasctisticy
      data %>%
        filter(model_name == !!{
          model_name
        }) %>%
        unite(col = "id", exclusion_set, dataset, estimate_type) %>%
        pull(model, name = id) %>%
        map_if(
          .x = .,
          .p = ~ any(class(.x) %in% "model_fit"),
          .f = ~ pluck(.x, "fit") %>%
            poss_check_heteroscedasticity() %>%
            as.numeric(),
          .else = ~ poss_check_heteroscedasticity(.x) %>% as.numeric()
        ) %>%
        enframe(name = "dataset", value = "heteroscedasticity") %>%
        mutate(heteroscedasticity = case_when(
          heteroscedasticity >= 0.05 ~ FALSE,
          TRUE ~ TRUE
        ))
    }
  ) %>%
    reduce(left_join, .dir = "forward")
}
