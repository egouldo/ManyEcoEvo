# round_1_question_key.csv
library(tidyverse)
library(here)
source(here::here("R/functions.R"))



# define covariates key to label individual covariate responses
covariates_key <-
  tribble(~variable_name,
          "VariablesBTS1_1", #TODO any reason why there are missing values in the numeric sequence 1:X??
          "VariablesBTS1_4",
          "VariablesBTS1_5",
          "VariablesBTS1_6",
          "VariablesBTS1_7",
          "VariablesBTS1_8",
          "VariablesBTS1_9",
          "VariablesBTS1_10",
          "VariablesBTS1_11",
          "VariablesBTS1_12",
          "VariablesBTS1_13",
          "VariablesBTS1_14",
          "VariablesBTS1_15",
          "VariablesBTS1_16",
          "VariablesBTS1_17",
          "VariablesBTS1_18",
          "VariablesBTS1_19",
          "VariablesBTS1_20",
          "VariablesBTS1_21",
          "VariablesBTS1_22",
          "VariablesBTS1_23",
          "VariablesBTS1_24",
          "VariablesBTS1_25",
          "VariablesBTS1_26",
          "VariablesBTS1_27",
          "VariablesBTS1_28",
          "VariablesBTS1_29",
          "VariablesBTS1_30",
          "VariablesBTS1_31",
          "VariablesBTS1_32",
          "VariablesBTS1_33",
          "VariablesBTS1_34",
          "VariablesBTS1_35",
          "VariablesBTS1_36",
          "VariablesBTS1_37",
          "VariablesBTS1_38",
          "VariablesBTS1_39",
          "VariablesBTS1_40",
          "VariablesEucS1_1",
          "VariablesEucS1_4",
          "VariablesEucS1_5",
          "VariablesEucS1_6",
          "VariablesEucS1_7",
          "VariablesEucS1_8",
          "VariablesEucS1_9",
          "VariablesEucS1_10",
          "VariablesEucS1_11",
          "VariablesEucS1_12",
          "VariablesEucS1_13",
          "VariablesEucS1_14",
          "VariablesEucS1_15",
          "VariablesEucS1_16",
          "VariablesEucS1_17",
          "VariablesEucS1_18",
          "VariablesEucS1_19",
          "VariablesEucS1_20",
          "VariablesEucS1_21",
          "VariablesEucS1_22",
          "VariablesEucS1_23",
          "VariablesEucS1_24",
          "VariablesEucS1_25",
          "VariablesEucS1_26",
          "VariablesEucS1_27",
          "VariablesEucS1_28",
          "VariablesEucS1_29",
          "VariablesEucS1_30",
          "VariablesEucS1_31",
          "VariablesEucS1_32",
          "VariablesEucS1_33",
          "VariablesEucS1_34",
          "VariablesEucS1_35",
          "VariablesEucS1_36",
          "VariablesEucS1_37",
          "VariablesEucS1_38",
          "VariablesEucS1_39",
          "VariablesEucS1_40") %>% 
  rowwise() %>% 
  mutate(qname = case_when(str_detect(string = variable_name,
                                      pattern = "VariablesEuc") ~ "Q37",
                           TRUE ~ "Q38") %>% 
           paste(., str_split(variable_name, "_") %>% 
                   flatten_chr() %>% pluck(2), sep = "_"))

q_text <- process_survey_questions_r1("SV_8FU0Y2y0TaFLQXP") %>% 
  filter(qname %in% c("Q37", "Q38")) %>% 
  pluck("question") %>% set_names(c("BT", "Euc")) 


round_1_question_key <- process_survey_questions_r1("SV_8FU0Y2y0TaFLQXP")  %>% 
  filter(qname %nin% c("Q37", "Q38")) %>% 
  bind_rows(covariates_key) %>% 
  mutate(question = 
           case_when(str_detect(pattern = "VariablesEuc", 
                                string = variable_name) ~ pluck(q_text, "Euc"),
                     str_detect(pattern = "VariablesBT", 
                                string = variable_name) ~ pluck(q_text, "BT"),
                     TRUE ~ question))

write_csv(round_1_question_key, 
          here::here("./data-raw/metadata_and_key_data/round_1_question_key.csv"))

