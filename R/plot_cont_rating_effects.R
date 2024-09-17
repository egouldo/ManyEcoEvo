#' Plot Marginal Effects for Numeric Rating Model
#'
#' @param df Dataframe with column 'abs_deviation_score_estimate', 'lambda' and `predictor`, `response` columns
#' @param response A character vector naming the response variable column in `df`
#' @param predictor A character vector naming the predictor variable column in `df`
#' @param group An optional character vector naming the random effect / grouping variable column in `df`
#' @param plot A logical indicating whether the plot should be rendered interactively or not, defaults to `TRUE`
#' @param back_transform A logical indicating whether the response variable should be back-transformed from the box-cox transformed scale to absolute deviation scores
#'
#' @return A list, with the first element containing the fitted statistical model and the second element containing the plot
#' @export
#' @family Plotting functions
#' @examples
#' # ManyEcoEvo_results$effects_analysis[[1]] %>% #TODO use package data object instead of targets object
#' # unnest(review_data) %>%
#' #   plot_cont_rating_effects(response = "box_cox_abs_deviation_score_estimate",
#' #                            predictor = "RateAnalysis",
#' #                            group = "ReviewerId",
#' #                            back_transform = TRUE) %>%
#' #   pluck(2) +
#' #   ggforce::facet_zoom(xlim = c(0,100), ylim = c(0,0.55)) +
#' #   ggpubr::theme_pubclean() +
#' #   ggplot2::xlab("Rating") +
#' #   ggplot2::ylab("Deviation In Effect Size from Analytic Mean")
plot_cont_rating_effects <- function(df = data.frame(), response = character(), predictor = character(), group = NULL, plot = TRUE, back_transform = FALSE) {
  if (is.null(group)) {
    f <- rlang::new_formula(
      rlang::ensym(response),
      rlang::ensym(predictor)
    )
    mod <- lm(f, data = df)
  } else {
    f <- as.formula(paste(as.name(response), "~ ", as.name(predictor), "+ (1 | ", as.name(group), ")"))
    mod <- lme4::lmer(formula = f, data = df)
  } # Because ggeffects looks for the model variable in the gloabl environment...

  predictions <- ggeffects::ggpredict(mod)
  predictions_df <- predictions %>%
    pluck(as.character(rlang::ensym(predictor))) %>%
    as_tibble()

  if (back_transform == TRUE) {
    df <- df %>% # use abs deviation values
      mutate(box_cox_abs_deviation_score_estimate = abs_deviation_score_estimate)

    predictions_df <- predictions_df %>%
      mutate(lambda = df$lambda %>% unique()) %>%
      mutate(across(
        .cols = c(-group, -x),
        ~ sae::bxcx(unique(df$lambda), x = .x, InverseQ = TRUE)
      ))
  }

  p <- ggplot(
    data = predictions_df,
    mapping = aes(x = x, y = predicted)
  ) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
    geom_point(data = df, aes_(y = as.name(response), x = as.name(predictor)))

  if (plot == TRUE) {
    plot(p)
  }

  return(list(mod, p))
}
