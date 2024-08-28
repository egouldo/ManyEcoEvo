#' Forestplot with ggplot2
#'
#' @param meta_model Fitted meta-analysis model of class `rma`
#' @param estimate_type Whether the estimate is standardized effect size of standardised out of sample prediction. Must be one of "Zr", "y50", "y25", "y75".
#' @param dataset Character string equal to the name of the dataset. One of "blue tit" or "eucalyptus"
#'
#' @return An object of class `ggplot2`
#' @export
#' @family Plotting Functions
#' @importFrom ggplot2 ggplot aes element_line element_text theme guides coord_flip labs geom_pointrange
#' @importFrom ggforestplot theme_forest
#' @importFrom parameters parameters
#' @importFrom tibble as_tibble
#' @import dplyr
#' @importFrom forcats fct_reorder
#' @importFrom stringr str_detect
#' @importFrom cli cli_h2
gg_forest <- function(meta_model, estimate_type, dataset = character(1L)) {
  match.arg(dataset, choices = c("blue tit", "eucalyptus"), several.ok = FALSE)
  
  stopifnot("rma" %in% class(meta_model))

  match.arg(estimate_type,
    choices = c("Zr", "y50", "y25", "y75"),
    several.ok = FALSE
  )
  
  # ---- Extract Plot Data & Axis Labels ----
  
  cli::cli_h2(c(
    "Creating gg-forest-plot of {.arg estimate_type} estimates for ",
    "{.arg dataset} = {.val {dataset}}"
  ))
  
  plot_data <- meta_model %>%
    parameters::parameters() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      point_shape = ifelse(stringr::str_detect(Parameter, "Overall"), "diamond", "circle"),
      Parameter = forcats::fct_reorder(Parameter, Coefficient) %>%
        forcats::fct_reorder(., point_shape, .desc = TRUE)
    )

  x_axis_label <- case_when(
    estimate_type == "Zr" ~ expression("Standardised Effect Size, Z"[r]),
    estimate_type == "ymed" ~ expression("Standardised Out-of-sample Prediction, y"[50]),
    estimate_type == "y25" ~ expression("Standardised Out-of-sample Prediction, y"[25]),
    estimate_type == "y50" ~ expression("Standardised Out-of-sample Prediction, y"[50]),
    TRUE ~ expression("Standardised Out-of-sample Prediction, y"[75])
  )

  # euc_svg_url <- "https://images.phylopic.org/images/a42656fa-b92a-4c69-8f13-55e4cb4b6bc1/vector.svg"
  # bt_svg_url <- "https://images.phylopic.org/images/dfdfb59e-8126-44e1-a7a9-1bf698113e1c/vector.svg"
  # 
  # img_url <- ifelse(dataset == "eucalyptus", euc_svg_url, bt_svg_url)

  p <- plot_data %>%
    ggplot(aes(
      y = Coefficient,
      x = Parameter,
      ymin = CI_low,
      ymax = CI_high,
      shape = point_shape
    )) +
    geom_pointrange() +
    ggforestplot::theme_forest() +
    theme(
      axis.line = element_line(size = 0.10, colour = "black"),
      text = element_text(family = "Helvetica"),
      axis.text.y = element_text(face = "italic", size = 8, colour = "grey40")
    ) +
    guides(shape = "none") +
    coord_flip() +
    labs(
      y = x_axis_label,
      x = "Analysis"
    )

  # p <-
  #   p %>%
  #   ggimage::ggbackground(., background = , img_url,
  #                         alpha = 0.1,
  #                         color = "steelblue")

  return(p)
}
