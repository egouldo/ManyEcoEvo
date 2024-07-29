#' Fit Multivariate Metaregression using metafoR
#'
#' @param estimate Numeric vector
#' @param variance Numeric vector
#' @param estimate_type Character vector of either "Zr", "y50", "y25", "y75".
#' @param data Dataframe containing estimates and variances with case id column \code{study_id}.
#'
#' @return Object of class \code{rma.mv}
#' @import metafor
#' @export
#'
#' @examples
#'   #TODO -- is this the best way of setting up this fun?? (i.e. to take numeric vectors)?
#' # Example Usage:
#' # library(tidyverse);library(targets);library(metafor) # NOT RUN, TODO: remove after create pkg
#' # source("R/functions.R") #NOT RUN, TODO: remove after create pkg
#' tar_read(round_2_survey_meta_analysis) %>%
#'   filter(dataset == "eucalyptus") %>%
#'   filter(!is.na(Zr),
#'          !is.na(VZr),
#'          !is.infinite(Zr),
#'          !is.infinite(VZr)) %>%
#' fit_metafor_mv(estimate = .$Zr, variance = .$VZr, estimate_type = "Zr", data = .)
fit_metafor_mv <- function(estimate, variance, estimate_type = character(1L), data){
  cli::cli_h2(glue::glue("Fitting metaregression"))
  match.arg(estimate_type, choices = c("Zr", "y50", "y25", "y75", "yi"), several.ok = FALSE) 
  
  if(estimate_type != "Zr"){ # you need to put SE^2 for y
    variance <- variance^2
  }
  
  mod <- metafor::rma.mv(yi = estimate, # of type "Zr" or "ymed", "y25", "y75"
                  V = variance, 
                  random = list(~1|TeamIdentifier/study_id),
                  data = data,
                  sparse = TRUE,
                  # verbose = TRUE,
                  control = list(optimizer="nloptr", maxeval=1000),
                  slab = data$study_id)
  return(mod)
}


#' Fit reduced metaregression model
#'
#' @param estimate Numeric vector
#' @param variance Numeric vector
#' @param estimate_type Character vector of either "Zr", "y50", "y25", "y75".
#' @param data Dataframe containing estimates and variances with case id column \code{study_id}.
#'
#' @return Object of class \code{rma.mv}
#' @import metafor
#' @import dplyr
#' @importFrom glue glue
#' @importFrom cli cli_h2
#' @export
#'
#' @examples
#'   #TODO -- is this the best way of setting up this fun?? (i.e. to take numeric vectors)?
#' # Example Usage:
#' # library(tidyverse);library(targets);library(metafor) # NOT RUN, TODO: remove after create pkg
#' # source("R/functions.R") #NOT RUN, TODO: remove after create pkg
#' tar_read(round_2_survey_meta_analysis) %>%
#'   filter(dataset == "eucalyptus") %>%
#'   filter(!is.na(Zr),
#'          !is.na(VZr),
#'          !is.infinite(Zr),
#'          !is.infinite(VZr)) %>%
#' fit_metafor_mv(estimate = .$Zr, variance = .$VZr, estimate_type = "Zr", data = .)
fit_metafor_mv_reduced <- function(estimate, variance, estimate_type = character(1L), data){
  cli::cli_h2(glue::glue("Fitting multivariate metaregression"))
  match.arg(estimate_type, choices = c("Zr", "y50", "y25", "y75"), several.ok = FALSE) 
  
  data <- data %>% 
    ungroup() %>% 
    mutate(obs_id = 1:n()) # metafor idiosyncrasy 
  
  if(estimate_type != "Zr"){ # you need to put SE^2 for y
    variance <- variance^2
  }
  
  metafor::rma.mv(yi = estimate, # of type "Zr" or "ymed", "y25", "y75"
                  V = variance, 
                  random=list(~1|TeamIdentifier/obs_id),
                  data = data,
                  sparse = TRUE,
                  sigma2 =  c(0, NA))
  
}

poss_fit_metafor_mv <- purrr::possibly(fit_metafor_mv,
                                       otherwise = NA,
                                       quiet = FALSE)
