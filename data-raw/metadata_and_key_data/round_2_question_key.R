# round_2_question_key.csv
library(tidyverse)
library(here)
source(here::here("R/functions.R"))

make_round_2_survey_long(here::here("data-raw/qualtrics_output/SV_6rlLp8U0XsP7m9T_round_2_survey.csv")) %>% 
  rename(response_id = ResponseId) %>% 
  process_survey_questions(surveyID = "SV_6rlLp8U0XsP7m9T", #TODO rename function (include round_2 specifically?)
                           survey_long = .,
                           survey_key_path = 
                           here::here("data-raw/metadata_and_key_data/",  
                                      "round_2_qualtrics_question_key.csv")) %>% 
  write_csv(here::here("data-raw/metadata_and_key_data/",  
                       "round_2_question_key.csv"))
