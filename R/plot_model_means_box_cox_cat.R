#' @title plot_model_means_box_cox_cat
#' @description Plot model means for box-cox transformed deviation scores as a function of categorical ratings
#' @param dat Data for plotting
#' @param variable Categorical predictor variable to plot
#' @param predictor_means A tibble containing the means and confidence intervals of the predictor variable
#' @param new_order A character vector of the new order of the levels of the categorical predictor
#' @param title A character vector of the plot title
#' @param lambda A length 1 numeric vector of the lambda value used in the box-cox transformation
#' @param back_transform A logical indicating whether to back-transform the box-cox transformed data
#' @return A ggplot object
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom see geom_jitter2 scale_fill_material_d theme_modern
#' @importFrom EnvStats stat_n_text
#' @importFrom forcats fct_relevel
#' @importFrom sae bxcx
#' @family Plotting functions
plot_model_means_box_cox_cat <- function(dat,
                                         variable,
                                         predictor_means,
                                         new_order,
                                         title,
                                         lambda,
                                         back_transform = FALSE) {
  dat <- mutate(
    dat,
    "{{variable}}" := #
      fct_relevel(
        .f = {{ variable }},
        new_order
      )
  )

  if (back_transform == TRUE) {
    dat <- dat %>%
      mutate(
        box_cox_abs_deviation_score_estimate =
          sae::bxcx(unique(lambda),
            x = box_cox_abs_deviation_score_estimate, InverseQ = TRUE
          )
      )

    predictor_means <- predictor_means %>%
      as_tibble() %>%
      mutate(lambda = lambda %>% unique()) %>%
      mutate(across(
        .cols = -PublishableAsIs,
        ~ sae::bxcx(unique(lambda), x = .x, InverseQ = TRUE)
      ))
  }

  p <- ggplot(dat, aes(
    x = {{ variable }},
    y = box_cox_abs_deviation_score_estimate
  )) +
    # Add base dat
    geom_violin(aes(fill = {{ variable }}),
      trim = TRUE,
      # scale = "count", #TODO consider toggle on/off?
      colour = "white"
    ) +
    see::geom_jitter2(width = 0.05, alpha = 0.5) +
    # Add pointrange and line from means
    geom_line(dat = predictor_means, aes(y = Mean, group = 1), linewidth = 1) +
    geom_pointrange(
      dat = predictor_means,
      aes(y = Mean, ymin = CI_low, ymax = CI_high),
      size = 1,
      color = "grey",
      alpha = 0.5
    ) +
    # Improve colors
    see::scale_fill_material_d(
      discrete = TRUE,
      name = "",
      palette = "ice",
      labels = pull(dat, {{ variable }}) %>%
        levels() %>%
        capwords(),
      reverse = TRUE
    ) +
    EnvStats::stat_n_text() +
    see::theme_modern() +
    theme(axis.text.x = element_text(angle = 90)) #+
  # ggtitle(label = title)

  if (back_transform == TRUE) {
    p <- p +
      labs(
        x = "Categorical Peer Review Rating",
        y = "Absolute Deviation from\n Meta-Anaytic Mean Zr"
      )
  } else {
    p <- p + labs(
      x = "Categorical Peer Review Rating",
      y = "Deviation from\nMeta-Analytic Mean Effect Size"
    )
  }

  return(p)
}
