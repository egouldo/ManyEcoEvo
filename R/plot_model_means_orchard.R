#' Plot orchard-plot style model means
#' @description Plot the means of a model with a predictor variable
#' @param dat A tibble with the data to plot
#' @param variable A character string of the predictor variable to plot
#' @param predictor_means A tibble with the means of the model
#' @param new_order A character vector of the new order of the `variable`
#' @param title A character string of the plot title
#' @return A ggplot object
#' @export
#' @import ggplot2
#' @importFrom ggbeeswarm geom_quasirandom
#' @import dplyr
#' @importFrom see theme_modern
#' @importFrom forcats fct_relevel
#' @family Plotting functions
plot_model_means_orchard <- function(dat,
                                     variable,
                                     predictor_means,
                                     new_order,
                                     title) {
  dat <- dat %>%
    rename(weights = `(weights)`) %>%
    mutate(
      "{{variable}}" := #
        fct_relevel(
          .f = {{ variable }},
          new_order
        ),
      weights = as.numeric(weights)
    )

  ggplot() +
    ggbeeswarm::geom_quasirandom(
      data = dat,
      mapping = ggplot2::aes(
        y = box_cox_abs_deviation_score_estimate,
        x = {{ variable }},
        size = weights,
        colour = {{ variable }}
      ),
      alpha = 0.7
    ) +
    geom_pointrange(
      dat = predictor_means,
      aes(x = {{ variable }}, y = Mean, ymin = CI_low, ymax = CI_high, color = {{ variable }}),
      size = 1,
      alpha = 1
    ) +
    see::theme_modern() +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle(label = title)
}
