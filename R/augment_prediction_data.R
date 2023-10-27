#' Augment analyst out-of-sample prediction data according to the outcome of pointblank interrogation
#' @description `augment_prediction_data()` will relabel and remove extraneous columns from out-of-sample prediction data. 
#' @details Where there are missing variables in `.data` or `checks` is missing, `NA` will be returned for the augmented prediction data instead.
#' @param .data A data.frame of out of-sample-predictions analyst submission data
#' @param checks A data.frame of pointblank interrogation output for `.data`
#' @param dataset A character string equal to either "blue tit" or "eucalyptus"
#'
#' @return A dataframe. Individual analyst submission containing augmented out-of-sample prediction data ready for meta-analysis.
#' @export
#'
#' @examples
#' # for testing and dev purposes:
#' # safe_augment <- purrr:::safely(augment_prediction_data)
augment_prediction_data <- function(.data, checks, dataset){
  if(is.logical(.data)){
    return(NA)
  } else{
    stopifnot(is.data.frame(.data))
    checks <- flatten_df(checks)
    stopifnot(is.data.frame(checks))
    
    # ---- define helper funs ----
    relabel <- function(checks, submission_data) {
      if(!is.data.frame(checks)){
        cli::cli_abort("Argument `checks` is not a data.frame")
      }
      if(!is.data.frame(submission_data)){
        cli::cli_abort("Argument `submission_data` is not a data.frame")
      }
      
      test_check_dat <- 
        checks %>% 
        select(new_name, column_rename, column_exists) %>% 
        ungroup %>% 
        filter(column_exists == "mislabel")
      
      action <- 
        checks %>% ungroup %>% 
        distinct(other_action) %>% flatten_chr()
      
      out <- if(nrow(test_check_dat) > 0 & is.na(action)){
        replacement_names <- test_check_dat %>% pull(column_rename) %>% 
          set_names(test_check_dat %>% pull(new_name))
        
        submission_data %>% 
          rename(!!!replacement_names)
      } else(submission_data)
      
      return(out)
    }
    
    remove_extraneous_cols <- function(checks, submission_data){
      if(!is.data.frame(checks)){
        cli::cli_abort("Argument `checks` is not a data.frame")
      }
      if(!is.data.frame(submission_data)){
        cli::cli_abort("Argument `submission_data` is not a data.frame")
      }
      
      test_check_dat <- 
        checks %>% 
        select(new_name, column_rename, column_exists) %>% 
        ungroup %>% 
        filter(column_exists == "missing")
      
      action <- 
        checks %>% ungroup %>% 
        distinct(other_action) %>% flatten_chr()
      
      
      # nrow(test_check_dat) should be 0
      # action should be NA
      # if no columns are missing, then we select the vars we want
      if(nrow(test_check_dat) == 0 & is.na(action)){
        select_cols <- checks %>% 
          ungroup %>% 
          select(new_name) %>% 
          drop_na() %>% 
          pull(new_name)
        
        submission_data %>% 
          select(!!!select_cols)
      } else(submission_data) # should we just return NA if they don't have all the columns??
      # how do we deal with the missing columns... do we create them
    } 
    
    exclude_from_binding <- function(checks, submission_data){
      if(!is.data.frame(checks)){
        cli::cli_abort("Argument `checks` is not a data.frame")
      }
      if(!is.data.frame(submission_data)){
        cli::cli_abort("Argument `submission_data` is not a data.frame")
      }
      # exclude from the binding any file with 'missing vars'
      # and any file with other_action not NA
      test_check_dat <- 
        checks %>% 
        ungroup %>% 
        select(other_action, contact_analyst) %>% 
        filter(if_any(everything(), ~ !is.na(.)))
      
      # If any obs in either col is not NA, then exclude DF
      # To exclude, return NA, otherwise we return the submission_data
      if(nrow(test_check_dat) == 0){
        return(submission_data)
      } else(NA)
    }
    
    reformat_SurveyID <- function(submission_data){
      if(!is.data.frame(submission_data)){
        cli::cli_abort("Argument `submission_data` is not a data.frame")
      }
      
      # if "Q" is missing, concatenate "Q" to SurveyID number
      submission_data %>% 
        mutate(formatted = str_detect(SurveyID, "Q"), 
               SurveyID = ifelse(formatted == FALSE, paste0("Q", SurveyID), SurveyID)) %>% 
        select(-formatted)
    }
    
    reformat_bluetit <- function(submission_data){
      if(!is.data.frame(submission_data)){
        cli::cli_abort("Argument `submission_data` is not a data.frame")
      }
      
      submission_data %>% 
        group_by(scenario) %>% 
        mutate(scenario = ifelse(is.character(scenario), parse_number(scenario), scenario),
               across(where(~ is.character(.x) && str_detect(.x, ",")), 
                      ~ parse_number(.x, locale = locale(decimal_mark = ","))))
    }
    
    new_names <- 
      tibble(dataset = "eucalyptus", 
             check_id = c(1:5),
             new_name = c("SurveyID", "fit", "se.fit", "ci.low", "ci.hi")) %>% 
      bind_rows(
        tibble(
          dataset = "blue tit",
          check_id = c(1:5),
          new_name = c("scenario", "estimate", "se.fit", "ci.low", "ci.hi")
        )
      )
    
    # ---- apply helper funs and post process ----
    augmented_df <-
      .data %>%
      relabel(checks = checks, submission_data = .) %>% 
      remove_extraneous_cols(checks = checks, submission_data = .) %>% 
      filter(., complete.cases(.)) %>% 
      exclude_from_binding(checks = checks, submission_data = .)
    
    if(rlang::is_na(augmented_df)){ 
      cli::cli_alert("Prediction file excluded") #TODO ID which file/response_id/analysis_id is being excluded in message - change to warning?
    } else if(rlang::is_na(dataset)){
      augmented_df <- NA
      cli::cli_alert("{.arg dataset} is {.val {dataset}}.")
    } else if(dataset == "eucalyptus"){
      augmented_df <- reformat_SurveyID(augmented_df)
    } else{
      augmented_df <- reformat_bluetit(augmented_df)
    }
    
    
    out <- augmented_df
    
    return(out)
  }
  
}
