#' Fit univariate glm of deviation scores on sorensen diversity index
#'
#' @param data A `data.frame` containing Box-Cox transformed absolute deviation from the meta-analytic mean scores and the mean Sorensen's scores for each analysis
#'
#' @return A fitted model object of class `glm` and `parsnip`
#' @export
#' @family Model fitting and meta-analysis
#' @importFrom parsnip linear_reg fit
#' @importFrom recipes recipe update_role
#' @importFrom workflows workflow add_model add_recipe extract_fit_parsnip
#' @import dplyr
#' @importFrom cli cli_h2 cli_alert_info
#' @importFrom pointblank expect_col_exists
#' @importFrom purrr simplify
#' @importFrom rlang try_fetch
fit_sorensen_glm <- function(data) {
  cli::cli_h2(c("Fitting glm for box-cox transformed outcome with sorensen diversity index as predictor"))
  # Only run cur_group() when called within a dplyr grouping context
  rlang::try_fetch(
    {
      group_info <- dplyr::cur_group()
      if (length(group_info) > 0) {
        cli::cli_alert_info(group_info %>% purrr::simplify())
      }
    },
    error = function(cnd) {
      # Silently ignore if not in a dplyr grouping context
    }
  )
  
  pointblank::expect_col_exists(data, 
                                columns = c(starts_with("box_cox_abs_"),
                                            "mean_diversity_index"))
  
  data <- data %>%
    dplyr::select(dplyr::starts_with("box_cox_abs_"), 
                  mean_diversity_index)
  
  glm_recipe <-
    recipes::recipe(~.,
                    data = data
    ) %>%
    recipes::update_role(starts_with("box_cox_abs_"), new_role = "outcome")
  
  glm_mod <- parsnip::linear_reg(engine = "glm")
  
  fitted_mod <-
    workflows::workflow() %>%
    workflows::add_model(glm_mod) %>%
    workflows::add_recipe(glm_recipe) %>%
    parsnip::fit(data = data) %>%
    workflows::extract_fit_parsnip()
  
  return(fitted_mod)
}

#' Possibly [fit_sorensen_glm()]
#' @description A version of [fit_sorensen_glm()] that returns `NA` if an error is encountered
#' @keywords internal
#' @importFrom purrr possibly
poss_fit_sorensen_glm <- purrr::possibly(fit_sorensen_glm,
                                         otherwise = NA,
                                         quiet = FALSE
) 
