#' Marginal Effects Plot of Diversity Index Model
#'
#' @param mod A fitted model of class 'lm'
#' @param df A dataframe with the columns, lambda, abs_deviation_score_estimate, box_cox_abs_deviation_score_estimate, mean_diversity_index
#' @param back_transform logical, to back-transform box-cox transformed to absolute deviation scores or not
#'
#' @return A ggplot with the original data,  predicted values and confidence values from the fitted model
#' @export
#'
#' @examples
#' #targets::tar_load(ManyEcoEvo_results) #TODO change this to package data
#' #library(tidyverse)
#' #plot_effects_diversity(mod = ManyEcoEvo_results$sorensen_glm[[5]], 
#' #df = ManyEcoEvo_results$effects_analysis[[5]] %>% #dat
#' #select(mean_diversity_index, study_id, 
#' #          starts_with("box_cox"), 
#' #                   starts_with("abs_dev"),
#' #                            lambda), 
#' #                            back_transform = TRUE) 
plot_effects_diversity <- function(mod, df, back_transform = FALSE) {
  #TODO extract df from fitted model and supply lambda separately!??
  predictions_df <- mod %>% 
    ggeffects::ggpredict(terms = "mean_diversity_index [all]") %>% 
    as_tibble()
  
  if(back_transform == TRUE){
    #TODO add check that predictor var has correct col name, and lambda exists
    predictions_df <- predictions_df %>% 
      mutate(lambda = df$lambda %>% unique()) %>%  
      mutate(across(.cols = c(-group, -x),
                    ~ sae::bxcx(unique(df$lambda),x = .x, InverseQ = TRUE)))
    
    p <- ggplot(data = predictions_df, 
                mapping = aes(x = x, y = predicted)) +
      geom_line() +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
      geom_point(data = df, aes(y = abs_deviation_score_estimate, x = mean_diversity_index)) +
      ggplot2::xlab("Mean Sorensen's Index") +
      ggplot2::ylab("Deviation In Effect Size from Analytic Mean")
  }else {
    #TODO add check that columns are labelled appropriately i.e. same as what is supplied to aes()
    p <- ggplot(data = predictions_df, 
                mapping = aes(x = x, y = predicted)) +
      geom_line() +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
      geom_point(data = df, aes(y = box_cox_abs_deviation_score_estimate, x = mean_diversity_index)) +
      ggplot2::xlab("Mean Sorensen's Index") +
      ggplot2::ylab("Deviation In Effect Size from Analytic Mean")
  }
  
  return(p)
  
}