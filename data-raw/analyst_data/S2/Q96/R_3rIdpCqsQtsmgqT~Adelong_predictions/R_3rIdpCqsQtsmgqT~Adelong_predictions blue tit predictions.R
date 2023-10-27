#code to extract predictions from my models
#last edited December 10, 2020 by A. R. Martinig


#clear memory
rm(list=ls(all=T))

options(dplyr.width = Inf, tibble.print_min = 200, digits=5, scipen=999)
#dplyr.width will show all columns
#tibble.print_min sets how many rows are printed
#digits is number of digits in output
#scipen=999 removes scientific notation

#loading required packages
lapply(c("dplyr", "tidyverse", "lme4", "ggplot2", "cowplot", "tidyr"), require, character.only = TRUE)


################################################
#############    original data    ##############
################################################

blue<-read.csv("/Users/april-martinig/Documents/Files/Manuscripts/Parker et al. 2021//Blue Tit Analysis/blue_tit_data_NA.csv", header=T) %>%  #loading original dataset after replacing all blanks with NAs
#creating a new variable called "related" (variable that measures proportion of nestlings that come from original nest vs. new nest)
	group_by(rear_nest_breed_ID) %>%
		mutate(home=ifelse(home_or_away==1, 1, 0),
		total=n(),
		total_home=sum(home),
		related=total_home/total) %>%
	ungroup() %>%
#changing variable names and setting them as numeric or factor	
	mutate(year = (hatch_year-2001),
		wt=as.numeric(day_14_weight), 
		tarsus=as.numeric(day_14_tarsus_length),  
		hatch_LD=as.numeric(hatch_nest_LD), 
		hatch_CS=as.numeric(hatch_nest_CS), 
		hatch_OH=as.numeric(hatch_nest_OH), 
		d0_hatch_brood_size=as.numeric(d0_hatch_nest_brood_size),  
		d14_hatch_brood_size= as.numeric(d14_hatch_nest_brood_size), 
		rear_LD=as.numeric(rear_nest_LD), 	
		rear_CS=as.numeric(rear_nest_CS), 
		rear_OH=as.numeric(rear_nest_OH), 
		d0_rear_brood_size= as.numeric(rear_d0_rear_nest_brood_size), 
		rear_Cs_out=as.numeric(rear_Cs_out), 
		rear_Cs_in=as.numeric(rear_Cs_in), 
		net_manipulation=as.numeric(net_rearing_manipulation), 
		rear_Cs=as.numeric(rear_Cs_at_start_of_rearing), 
		d14_rear_brood_size=as.numeric(d14_rear_nest_brood_size), 
		day14=as.numeric(Date_of_day14),
		home_or_away=as.factor(home_or_away),
		sex=as.factor(chick_sex_molec),
		observer_id=as.factor(day14_measurer)) %>%
#selecting which variables to keep (also renaming any remaining variables) based on model that will be run later
	select(chick_ring_number, year, 
		hatch_id=hatch_nest_breed_ID, 
		hatch_area=hatch_Area, hatch_LD, hatch_CS, hatch_OH, 
		d0_hatch_brood_size, d14_hatch_brood_size, 
		rear_id=rear_nest_breed_ID, rear_area, 
		rear_trt=rear_nest_trt, rear_Cs, d14_rear_brood_size, 
		tarsus, wt, observer_id,	 related) %>% 
#filtering out nestlings with NAs for the variables of interest
	filter (!is.na(hatch_CS), !is.na(hatch_OH), 
		!is.na(d0_hatch_brood_size), 
		!is.na(d14_rear_brood_size), !is.na(rear_trt), 
		!is.na(rear_Cs), !is.na(hatch_area), !is.na(year), 
		!is.na(observer_id), !is.na(wt), !is.na(tarsus), 
		!is.na(related), 
#exclude the outlier chicks (results don't change with or without them, but because I don't know if they are mistakes or not, removing to be safe)		
		!chick_ring_number %in% c("P810753", "P808581"))
		
	
#externally setting these variables as factors as this allows you to rename them at the same time		
blue$rear_trt <- factor(blue$rear_trt, levels = c(5, 6, 7), labels = c("Increase", "Decrease", "Control"))
blue$rear_trt <- relevel(blue$rear_trt, ref="Control")	

summary(blue)
head(blue)
names(blue)

#count unique observations
(blue) %>% as_tibble() %>% count(chick_ring_number) #3208 inds
(blue) %>% as_tibble() %>% count(hatch_id) #418 hatch ids
(blue) %>% as_tibble() %>% count(rear_id) #392 rear ids


#################################################
#############    predictor data    ##############
#################################################

blue_pred<-read.csv("/Users/april-martinig/Documents/Files/Manuscripts/Parker et al. 2021/Blue Tit Analysis/blue_tit_percentiles_for_supplement_wide.csv", header=T) %>%  #loading predictions dataset 
#creating a new variable called "related" (variable that measures proportion of nestlings that come from original nest vs. new nest)
	group_by(rear_nest_breed_ID) %>%
		mutate(home=ifelse(home_or_away==1, 1, 0),
		total=n(),
		total_home=sum(home),
		related=total_home/total) %>%
	ungroup() %>%
#changing variable names and setting them as numeric or factor	
	mutate(year = (hatch_year-2001),
		wt=as.numeric(day_14_weight), 
		tarsus=as.numeric(day_14_tarsus_length),  
		hatch_LD=as.numeric(hatch_nest_LD), 
		hatch_CS=as.numeric(hatch_nest_CS), 
		hatch_OH=as.numeric(hatch_nest_OH), 
		d0_hatch_brood_size=as.numeric(d0_hatch_nest_brood_size),  
		d14_hatch_brood_size= as.numeric(d14_hatch_nest_brood_size), 
		rear_LD=as.numeric(rear_nest_LD), 	
		rear_CS=as.numeric(rear_nest_CS), 
		rear_OH=as.numeric(rear_nest_OH), 
		d0_rear_brood_size= as.numeric(rear_d0_rear_nest_brood_size), 
		rear_Cs_out=as.numeric(rear_Cs_out), 
		rear_Cs_in=as.numeric(rear_Cs_in), 
		net_manipulation=as.numeric(net_rearing_manipulation), 
		rear_Cs=as.numeric(rear_Cs_at_start_of_rearing), 
		d14_rear_brood_size=as.numeric(d14_rear_nest_brood_size), 
		day14=as.numeric(Date_of_day14),
		home_or_away=as.factor(home_or_away),
		sex=as.factor(chick_sex_molec),
		observer_id=as.factor(day14_measurer)) %>%
#selecting which variables to keep (also renaming any remaining variables) based on model that will be run later
	select(scenario, year, hatch_id=hatch_nest_breed_ID, 
		hatch_area=hatch_Area, hatch_LD, hatch_CS, hatch_OH, 
		d0_hatch_brood_size, d14_hatch_brood_size, 
		rear_id=rear_nest_breed_ID, rear_area, 
		rear_trt=rear_nest_trt, rear_Cs, d14_rear_brood_size, 
		tarsus, wt, observer_id, related) %>% 
	group_by(year, hatch_area, rear_trt) %>% #standardizing w/in year, area, and treatment
	mutate(
		hatch_CS =((hatch_CS-mean(blue$hatch_CS))/(1*(sd(blue$hatch_CS)))),
		hatch_OH =((hatch_OH-mean(blue$hatch_OH))/(1*(sd(blue$hatch_OH)))), 
		d0_hatch_brood_size =((d0_hatch_brood_size-mean(blue$d0_hatch_brood_size))/(1*(sd(blue$d0_hatch_brood_size)))),
		d14_rear_brood_size =((d14_rear_brood_size-mean(blue$d14_rear_brood_size))/(1*(sd(blue$d14_rear_brood_size)))),
		rear_Cs =((rear_Cs-mean(blue$rear_Cs))/(1*(sd(blue$rear_Cs)))),
		wt =((wt-mean(blue$wt))/(1*(sd(blue$wt)))),
		tarsus =((tarsus-mean(blue$tarsus))/(1*(sd(blue$tarsus)))),
		related =((related-mean(blue$related))/(1*(sd(blue$related))))
		) %>% 
#fixing any cases where standardization resulted in NAs because you cannot divide by 0 (this happens when all values within grouping are the same) - replace with mean
	replace(is.na(.), 0) %>% 
#drop any unused levels
	droplevels()

#setting these variables as factors as this allows you to rename them at the same time		
blue_pred$rear_trt <- factor(blue_pred$rear_trt, levels = c(5, 6, 7), labels = c("Increase", "Decrease", "Control"))
blue_pred$rear_trt <- relevel(blue_pred$rear_trt, ref="Control")	

#I have to do this afterwards because I need to use the unscaled mean and sd from the blue data to properly scale the blue_pred data
blue<-blue%>%
	group_by(year, hatch_area, rear_trt) %>% #standardizing w/in year, area, and treatment for my original dataset
	mutate(
		hatch_CS =((hatch_CS-mean(hatch_CS))/(1*(sd(hatch_CS)))),
		hatch_OH =((hatch_OH-mean(hatch_OH))/(1*(sd(hatch_OH)))), 
		d0_hatch_brood_size =((d0_hatch_brood_size-mean(d0_hatch_brood_size))/(1*(sd(d0_hatch_brood_size)))),
		d14_rear_brood_size =((d14_rear_brood_size-mean(d14_rear_brood_size))/(1*(sd(d14_rear_brood_size)))),
		rear_Cs =((rear_Cs-mean(rear_Cs))/(1*(sd(rear_Cs)))),
		wt =((wt-mean(wt))/(1*(sd(wt)))),
		tarsus =((tarsus-mean(tarsus))/(1*(sd(tarsus)))),
		related =((related-mean(related))/(1*(sd(related))))
		) %>%
	#fixing any cases where standardization resulted in NAs because you cannot divide by 0 (this happens when all values within grouping are the same) - replace with mean
	replace(is.na(.), 0) %>% 
#drop any unused levels
	droplevels()	 	



################################################
#############    matching data?    #############
################################################

#are all column names in the training dataset in the prediction dataset?
all(names(blue %>% dplyr::select(-chick_ring_number))  %in% names(blue_pred %>% dplyr::select(-scenario))) # Yes
      
## Are all column names in prediction data in the training data?       
all(names(blue_pred %>% dplyr::select(-scenario)) %in% names(blue)) # Yes

       
#########################################
#########    original models    #########
#########################################

weight<-lme4::lmer(wt ~ hatch_OH + hatch_CS + d0_hatch_brood_size + rear_Cs + d14_rear_brood_size + rear_trt +  related + hatch_area + year + observer_id + (1|hatch_id), data=blue)

length<-lme4::lmer(tarsus ~ hatch_OH + hatch_CS + d0_hatch_brood_size + rear_Cs + d14_rear_brood_size + rear_trt +  related + hatch_area + year + observer_id + (1|hatch_id), data=blue)

#note I report the bayesian posterior mode estimates for the fixed effects and credible intervals in my original analysis


################################################
##########    vector of predictors    ##########
################################################

predict(weight, 
        type = "link", 
        newdata = blue_pred)
#        1        2        3 
# 0.13042 -0.81789 -0.53165 
 
predict(length, 
        type = "link", 
        newdata = blue_pred)
#      1        2        3 
# 0.13848 -0.38544  0.61455 

        
####################################################
#############    weight SE and CIs    ##############
####################################################

set.seed(1234)

merBoot <- lme4::bootMer(weight, FUN = function(x) predict(x, newdata = blue_pred, type = "link"), nsim = 1000)

sumBoot <- function(merBoot) {
  out <- data.frame(
    fit = apply(merBoot$t, 2, function(x)
      as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
    lwr = apply(merBoot$t, 2, function(x) 
      as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
    upr = apply(merBoot$t, 2, function(x) 
      as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
  )
  return(out)
}

sumBoot(merBoot)  # but doesn't give SE

#to get SE:
std.err <- apply(merBoot$t, 2, sd)

predictions <- sumBoot(merBoot) %>% 
	cbind(std.err) %>% 
  	mutate(scenario = c(1:3)) %>% 
  	rename(ci.low = lwr, ci.hi = upr, estimate = fit) %>%
#back-transform the data, mean(wt)=27.87, mean(sd)= 26.83
  	mutate(estimate = (estimate*26.83 + 27.87),
  		ci.low = (ci.low*26.83 + 27.87),
  		ci.hi = (ci.hi*26.83 + 27.87),
  		std.err = (std.err*26.83 + 27.87)) 
predictions

#	estimate  ci.low  ci.hi std.err scenario
#   32.706 17.7904 46.367  35.447        1
#   28.099 10.1312 45.023  37.364        2
#   23.331  6.7177 39.484  36.907        3


write_csv(predictions, "/Users/april-martinig/Desktop/blue_weight_predictions.csv")



####################################################
#############    length SE and CIs    ##############
####################################################

merBoot2 <- lme4::bootMer(length, FUN = function(x) predict(x, newdata = blue_pred, type = "link"), nsim = 1000)

sumBoot2 <- function(merBoot2) {
  out <- data.frame(
    fit = apply(merBoot2$t, 2, function(x)
      as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
    lwr = apply(merBoot2$t, 2, function(x) 
      as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
    upr = apply(merBoot2$t, 2, function(x) 
      as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
  )
  return(out)
}

sumBoot(merBoot2)  # but doesn't give SE

#to get SE:
std.err2 <- apply(merBoot2$t, 2, sd)

predictions2 <- sumBoot(merBoot2) %>% 
	cbind(std.err2) %>% 
  	mutate(scenario = c(1:3)) %>% 
  	rename(ci.low = lwr, ci.hi = upr, estimate = fit) %>%
  	#back-transform the data, mean(tarsus)= 32.515, mean(tarsus)= 7.2233
  	mutate(estimate = (estimate*7.2233 + 32.515),
  		ci.low = (ci.low*7.2233 + 32.515),
  		ci.hi = (ci.hi*7.2233 + 32.515),
  		std.err2 = (std.err2*7.2233 + 32.515))
predictions2

#   estimate ci.low  ci.hi std.err2 scenario
#   32.405 27.348 37.266   35.046        1
#   31.465 25.872 37.439   35.419        2
#   36.478 30.989 42.294   35.503        3

write_csv(predictions2, "/Users/april-martinig/Desktop/blue_length_predictions.csv")


##############################################
#############    extract dfs    ##############
##############################################

## load lmerTest package
library(lmerTest)
library(parameters)

weight2<-lmerTest::lmer(wt ~ hatch_OH + hatch_CS + d0_hatch_brood_size + rear_Cs + d14_rear_brood_size + rear_trt +  related + hatch_area + year + observer_id + (1|hatch_id), data=blue)

length2<-lmerTest::lmer(tarsus ~ hatch_OH + hatch_CS + d0_hatch_brood_size + rear_Cs + d14_rear_brood_size + rear_trt +  related + hatch_area + year + observer_id + (1|hatch_id), data=blue)

# Extract Adjusted Degrees of freedom using summary()
summary(weight2)$coef[,3]
summary(length2)$coef[,3]

#what to do with your estimate, SE, and df?
#pick one main effect of interest to submit in survey
#go with rear_Cs (rear_Cs_at_start_of_rearing) because it has a bigger effect