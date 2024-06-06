#' Extract collinearity removal results


library(ManyEcoEvo)
library(tidyverse)
library(ggplot2)
library(ggforestplot)
library(NatParksPalettes)
library(metafor)

plot_forest <- function(data, intercept = TRUE, MA_mean = TRUE){
  if (MA_mean == FALSE){
    data <- filter(data, Parameter != "overall")
  }
  
  p <- ggplot(data, aes(y = estimate, 
                        x =  term, 
                        ymin = conf.low, 
                        ymax = conf.high,
                        shape = parameter_type,
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
  
  if(intercept == TRUE){
    p <- p + geom_hline(yintercept = 0)
  }
  if(MA_mean == TRUE){
    p <- p + geom_hline(aes(yintercept = meta_analytic_mean), 
                        data = data,
                        colour = "#01353D", 
                        linetype = "dashed")
  }
  
  return(p)
}
filter_params <- rlang::exprs(exclusion_set == "complete", 
                              publishable_subset == "All", 
                              expertise_subset == "All", 
                              # collinearity_subset == "All",
                              model_name == "MA_mod",
                              dataset == "blue tit")
summary_output_params <- rlang::exprs(tidy_mod_summary, MA_fit_stats, mod_fit_stats)

map2(.x = list(ManyEcoEvo_viz %>% filter(!!!filter_params)), 
     .y = summary_output_params, 
     .f = ~ select(.x, collinearity_subset, all_of(.y)) %>% unnest(.y) %>% knitr::kable())


ManyEcoEvo_viz %>% filter(!!!filter_params) %>% 
  mutate(plot_data = map(model, 
                         .f = ~ broom::tidy(.x, 
                                            conf.int = TRUE, 
                                            include_studies = TRUE)%>% 
                           dplyr::mutate(point_shape = 
                                           ifelse(stringr::str_detect(term, "overall"), 
                                                  "diamond", 
                                                  "circle"),
                                         Parameter = 
                                           forcats::fct_reorder(term, 
                                                                estimate) %>% 
                                           forcats::fct_reorder(., 
                                                                point_shape,
                                                                .desc = TRUE))
  ),
  meta_analytic_mean = map_dbl(plot_data, 
                               ~ filter(.x, Parameter == "overall") %>% 
                                 pull(estimate))) %>% 
  group_by(collinearity_subset) %>% 
  group_split() %>% 
  map( ~ select(.x, plot_data, meta_analytic_mean) %>% 
         unnest(cols = c("plot_data")) %>% 
         mutate(parameter_type = case_when(str_detect(Parameter, "overall") ~ "mean",
                                           TRUE ~ "study")) %>% 
         arrange(desc(type)) %>% 
         mutate(type = forcats::as_factor(type)) %>% 
         group_by(type) %>% 
         arrange(desc(estimate),.by_group = TRUE) %>% 
         mutate(term = forcats::as_factor(term),
                point_shape = case_when(str_detect(type, "summary") ~ "mean",
                                        TRUE ~ "study")) %>% 
         plot_forest(intercept = TRUE, MA_mean = TRUE) +
         theme(axis.text.x = element_text(size = 15), 
               axis.title.x = element_text(size = 15),
               axis.text.y = element_blank()
         )) 


  