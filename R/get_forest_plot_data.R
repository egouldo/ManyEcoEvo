#' @title Get Forest Plot Data from a Metafor Model
#'
#' @param model A metafor model object of class `rma.mv` or `rma.uni`
#' @return A tibble containing the data required to plot a forest plot
#' @export
#' @family Forest Plot Functions
#' @examples
#' get_forest_plot_data(model)
#' @import broom.mixed
#' @import dplyr
#' @import forcats
#' @import stringr
get_forest_plot_data <- function(model){
  model %>% 
    broom.mixed::tidy(conf.int = TRUE, include_studies = TRUE) %>% 
    dplyr::mutate(
      point_shape = 
        ifelse(stringr::str_detect(term, "overall"), 
               "diamond", 
               "circle"),
      term = 
        forcats::fct_reorder(term, 
                             estimate) %>% 
        forcats::fct_reorder(., 
                             point_shape,
                             .desc = TRUE),
      parameter_type = case_when(str_detect(term, "overall") ~ "mean",
                                 TRUE ~ "study"),
      meta_analytic_mean =  pull(., estimate, type) %>% 
        keep_at(at = "summary")) %>% 
    select(-type, Parameter = term, everything())
}

#' @title Plot a Forest Plot
#' @description Plot a forest plot using the data from `get_forest_plot_data`
#' @param data A tibble containing the data required to plot a forest plot
#' @param intercept Logical. Should a horizontal line be added at 0?
#' @param MA_mean Logical. Should a dashed line be added at the meta-analytic mean?
#' @return A ggplot object
#' @export
#' @import ggplot2
#' @import ggforestplot
#' @import NatParksPalettes
#' @family Forest Plot Functions
#' @examples
#' model <- ManyEcoEvo_results %>% pluck("MA_mod", 1) 
#' plot_data <- get_forest_plot_data(model)
#' plot_forest(plot_data)
#' plot_forest(plot_data, intercept = FALSE)
#' plot_forest(plot_data, MA_mean = FALSE)
#' plot_forest(plot_data, intercept = FALSE, MA_mean = FALSE)
plot_forest <- function(data, intercept = TRUE, MA_mean = TRUE){
  if (MA_mean == FALSE) {
    data <- filter(data, Parameter != "overall")
  }
  
  p <- ggplot(data, aes(y = estimate, 
                        x =  Parameter, 
                        ymin = conf.low, 
                        ymax = conf.high,
                        shape = point_shape,
                        colour = parameter_type)) +
    geom_pointrange(fatten = 2) +
    ggforestplot::theme_forest() +
    theme(axis.line = element_line(linewidth = 0.10, colour = "black"),
          axis.line.y = element_blank(),
          text = element_text(family = "Helvetica")#,
          # axis.text.y = element_blank()
    ) +
    guides(shape = guide_legend(title = NULL), 
           colour = guide_legend(title = NULL)) +
    coord_flip() +
    ylab(bquote(Standardised~Effect~Size~Z[r])) +
    xlab(element_blank()) +
    # scale_y_continuous(breaks = c(-4,-3,-2,-1,0,1),
    # minor_breaks = seq(from = -4.5, to = 1.5, by = 0.5)) +
    NatParksPalettes::scale_color_natparks_d("Glacier")
  
  if (intercept == TRUE) {
    p <- p + geom_hline(yintercept = 0)
  }
  if (MA_mean == TRUE) {
    p <- p + geom_hline(aes(yintercept = meta_analytic_mean), 
                        data = data,
                        colour = "#01353D", 
                        linetype = "dashed")
  }
  
  return(p)
}
