# Load Analyst Datasets from OSF - creates exported datasets: `euc_data` and `blue_tit_data`

library(tidyverse)
library(osfr) # Must have personal access token stored
library(here)

#Download the Euc data from the OSF:
osf_repo <- osfr::osf_retrieve_node("https://osf.io/mn5aj/") # to authenticate, follow these instructions: https://cran.r-project.org/web/packages/osfr/vignettes/auth.html

evo_eco_data_node <- osfr::osf_retrieve_node("34fzc")

# Download Eucalyptus Data

osf_repo %>% 
  osfr::osf_ls_nodes() %>% 
  dplyr::filter(name == "Ecology and Conservation Data") %>% 
  dplyr::pull("id") %>% 
  osfr::osf_retrieve_node() %>% 
  osfr::osf_ls_files() %>% 
  dplyr::filter(name == "Euc_data.csv") %>% 
  dplyr::pull("id") %>% 
  osfr::osf_retrieve_file() %>% 
  osfr::osf_download(path = here::here("data-raw/analysis_datasets/"), 
               progress = TRUE,
               conflicts = "overwrite")

# Download Blue Tit Data
osf_repo %>% 
  osfr::osf_ls_nodes() %>% 
  dplyr::filter(name == "Evolutionary Ecology Data") %>% 
  dplyr::pull("id") %>% 
  osfr::osf_retrieve_node() %>% 
  osfr::osf_ls_files() %>% 
  dplyr::filter(name == "blue_tit_data_updated_2020-04-18.csv") %>% 
  dplyr::pull("id") %>% 
  osfr::osf_retrieve_file() %>% 
  osfr::osf_download(path = here::here("data-raw/analysis_datasets/"), 
               progress = TRUE,
               conflicts = "overwrite")

# Load Analyst Datasets

euc_data <- readr::read_csv(here::here("data-raw/analysis_datasets/Euc_data.csv")) %>% 
  dplyr::mutate(SurveyID = as.factor(SurveyID),
                Date = as.Date(Date, "%d/%m/%Y"))

blue_tit_data <- readr::read_csv(here::here("data-raw/analysis_datasets/blue_tit_data_updated_2020-04-18.csv"))

# Load Master Data File (obtain standardised names of analyst variables)

# master_data<-read_excel("C:/Users/hanna/OneDrive/Documents/ManyAnalysts/data-raw/Combined_Master_2022.06.29.xlsx")
# standardised_names<-unique(master_data$response_name_standardized)

# Add analyst-constructed variables to analysis data
euc_data <- euc_data %>% 
  mutate(euc_sdlgs_all = euc_sdlgs0_50cm + 
           `euc_sdlgs50cm-2m` + 
           `euc_sdlgs>2m`,
         euc_sdlgs0_2m = euc_sdlgs0_50cm +
           `euc_sdlgs50cm-2m`,
         `euc_sdlgs>50cm` = `euc_sdlgs50cm-2m` + `euc_sdlgs>2m`,
         `small*0.25+medium*1.25+large*2.5` = 0.25 * (euc_sdlgs0_50cm) + 
           1.25 * (`euc_sdlgs50cm-2m`) + 
           2.5 * (`euc_sdlgs>2m`)) %>% 
  rename(euc_sdlgs50cm_2m = `euc_sdlgs50cm-2m`)

euc_data <- euc_data %>% 
  left_join({euc_data %>% 
      mutate(euc_sdlgs_allbinary = ifelse(euc_sdlgs_all>0,1,0)) %>% 
      group_by(Season,Property) %>% 
      summarise(proportion_plots_seedlings=(sum(euc_sdlgs_allbinary)/n())) %>% 
      group_by(Property) %>% 
      summarise(average.proportion.of.plots.containing.at.least.one.euc.seedling.of.any.size = mean(proportion_plots_seedlings)) %>% 
      ungroup()})


# Blue Tit Data
blue_tit_data <- 
  blue_tit_data %>% 
  mutate(`day 14 weight` = 
           day_14_weight,
         `day_14_weight/(day_14_tarsus_length^2)` = 
           day_14_weight/(day_14_tarsus_length^2),
         `day_14_weight/day_14_tarsus_length` = 
           day_14_weight/(day_14_tarsus_length),
         `day_14_weight*day_14_tarsus_length` =  
           day_14_weight*day_14_tarsus_length)


usethis::use_data(euc_data, overwrite = TRUE)
usethis::use_data(blue_tit_data, overwrite = TRUE)
