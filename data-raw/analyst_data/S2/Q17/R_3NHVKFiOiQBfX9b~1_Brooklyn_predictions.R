#LOAD DATA=====
setwd("~/00DeakinUni/R/Fraser") #set working directory to data file
library(tidyverse)
library(MASS)
library(pscl)

Euc_data <- read.csv("Euc_data.csv")#Load DATA

#Sum grass covers and filter out NA-s:
euc <-  Euc_data %>%
  mutate(gc = ExoticAnnualGrass_cover+ExoticPerennialGrass_cover+NativePerennialGrass_cover) %>%  #gc = grass cover, #Join covers of all (3) types of grasses:
  filter(is.na(gc)==F) #filter out NA-s 


#Run 3 Models (nb1,nb2,nb3)======
#Compare NB and zeroinfl model, Winter 2006:
summary(nb1 <- glm.nb(euc_sdlgs0_50cm ~ gc, data = subset(euc, Season=="Winter 2006")))#https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
