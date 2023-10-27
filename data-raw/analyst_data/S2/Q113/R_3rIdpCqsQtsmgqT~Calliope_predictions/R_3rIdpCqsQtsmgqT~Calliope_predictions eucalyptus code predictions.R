#code to extract predictions from my models
#last edited December 15, 2020 by A. R. Martinig

#clear memory
rm(list=ls(all=T))

options(dplyr.width = Inf, tibble.print_min = 200, digits=5, scipen=999)
#dplyr.width will show all columns
#tibble.print_min sets how many rows are printed
#digits is number of digits in output
#scipen=999 removes scientific notation

#loading required packages
lapply(c("dplyr", "tidyverse", "lme4", "lubridate", "glmmTMB"), require, character.only = TRUE)

################################################
#############    original data    ##############
################################################

euc<-read.csv("/Users/april-martinig/Documents/Files/Manuscripts/Parker et al. 2021/Eucalyptus Analysis/Euc_data.csv", header=T) %>% #loading original dataset without any modifications
	#changing variable names and setting them as numeric or factor
	mutate(
		id=SurveyID, 
		date=dmy(Date), 
		year=year(date),
		year=as.factor(year-2006),
		month=month(date),
		season=month,
		season=ifelse(month %in% c(4, 5), "Autumn", 
			ifelse(month == 7, "Winter",
			ifelse(month %in% c(10, 11, 12), "Spring", season))),
		prop_id=Property, 
		quad=Quadrat.no, 
		ea_grass=ExoticAnnualGrass_cover, 
		ep_grass=ExoticPerennialGrass_cover, 
		np_grass=NativePerennialGrass_cover, 
		np_other_grass=NativePerennialGraminoid_cover, 
		euc0=euc_sdlgs0_50cm, 
		euc50=euc_sdlgs50cm.2m, 
		euc2=euc_sdlgs.2m, 
		precip=annual_precipitation) %>%
#selecting which variables to keep
	select(id, year, month, season, prop_id, quad, ea_grass, ep_grass,  np_grass, np_other_grass, euc0, euc50, euc2, precip) %>%
	filter (!is.na(precip), 
	!id %in% c(78, 69),
#exclude the outlier observations because I don't know if they are mistakes or not, removing to be safe - and also necessary for model convergence		
		!is.na(ea_grass)) 

summary(euc)
head(euc)
names(euc)

#count unique observations
(euc) %>% as_tibble() %>% count(prop_id) #18 properties
(euc) %>% as_tibble() %>% count(quad) #11 quadrants
nrow(euc) #346 observations



euc_pred<-read.csv("/Users/april-martinig/Documents/Files/Manuscripts/Parker et al. 2021/Eucalyptus Analysis/euc_specification_data_wide.csv", header=T) %>% #loading predictor dataset
	#changing variable names and setting them as numeric or factor
	mutate(
		id=SurveyID, 
		date=dmy(Date), 
		year=year(date),
		year=as.factor(year-2006),
		month=month(date),
		season=month,
		season=ifelse(month %in% c(4, 5), "Autumn", 
			ifelse(month == 7, "Winter",
			ifelse(month %in% c(10, 11, 12), "Spring", season))),
		prop_id=Property, 
		quad=Quadrat.no, 
		ea_grass=ExoticAnnualGrass_cover, 
		ep_grass=ExoticPerennialGrass_cover, 
		np_grass=NativePerennialGrass_cover, 
		np_other_grass=NativePerennialGraminoid_cover, 
		euc0=euc_sdlgs0_50cm, 
		euc50=euc_sdlgs50cm.2m, 
		euc2=euc_sdlgs.2m, 
		precip=annual_precipitation) %>%
#selecting which variables to keep
	select(id, year, month, season, prop_id, quad, ea_grass, ep_grass,  np_grass, np_other_grass, euc0, euc50, euc2, precip) %>%
	filter (!is.na(precip), 
	!id %in% c(78, 69),
#exclude the outlier observations because I don't know if they are mistakes or not, removing to be safe - and also necessary for model convergence		
		!is.na(ea_grass)) %>%
#standardizing w/in year, area, and treatment
	group_by(year, prop_id) %>% 
	mutate(
		ea_grass =((ea_grass-mean(euc$ea_grass))/(1*(sd(euc$ea_grass)))),
		ep_grass =((ep_grass-mean(euc$ep_grass))/(1*(sd(ep_grass)))), 
		np_grass =((np_grass-mean(euc$np_grass))/(1*(sd(euc$np_grass)))),
		np_other_grass =((np_other_grass-mean(euc$np_other_grass))/(1*(sd(np_other_grass)))),
		precip =((precip-mean(euc$precip))/(1*(sd(euc$precip))))
		) %>% #standardization "fixes" zero-inflation, but now must account for poisson distribution
#fixing any cases where standardization resulted in NAs because you cannot divide by 0 (this happens when all values within grouping are the same) - replace with mean
	replace(is.na(.), 0) %>% 
#drop any unused levels
	droplevels()	



euc<-euc%>%
#standardizing w/in year, area, and treatment
	group_by(year, prop_id) %>% 
	mutate(
		ea_grass =((ea_grass-mean(ea_grass))/(1*(sd(ea_grass)))),
		ep_grass =((ep_grass-mean(ep_grass))/(1*(sd(ep_grass)))), 
		np_grass =((np_grass-mean(np_grass))/(1*(sd(np_grass)))),
		np_other_grass =((np_other_grass-mean(np_other_grass))/(1*(sd(np_other_grass)))),
		precip =((precip-mean(precip))/(1*(sd(precip))))
		) %>% #standardization "fixes" zero-inflation, but now must account for poisson distribution
#fixing any cases where standardization resulted in NAs because you cannot divide by 0 (this happens when all values within grouping are the same) - replace with mean
	replace(is.na(.), 0) %>% 
#drop any unused levels
	droplevels()	 	



################################################
#############    matching data?    #############
################################################

# Are all column names in euc_data in new_euc data?
 all(names(euc) %in% names(euc_pred)) # Yes

# Are all column names in euc_data in new_euc data?
 all(names(euc) %in% names(euc_pred)) # Yes


#########################################
#########    original models    #########
#########################################

seed0<-glmmTMB(euc0 ~ ea_grass + ep_grass + np_grass + np_other_grass + month + precip + (1|prop_id), data = euc, family= nbinom2) #nbimom2 lets var increases quadratically

seed50<-glmmTMB(euc50 ~ ea_grass + ep_grass + np_grass + np_other_grass + month + precip + (1|prop_id), data = euc, family= nbinom2) #nbimom2 lets var increases quadratically


################################################
##########    vector of predictors    ##########
################################################

predict(seed0, 
        type = "link", 
        newdata = euc_pred)
# -0.78016 -2.12560 -1.13276 

predict(seed50, 
        type = "link", 
        newdata = euc_pred)
# -1.46853 -0.32842  0.70434
 

####################################################
#############    weight SE and CIs    ##############
####################################################

set.seed(1234)

merBoot <- lme4::bootMer(seed0, FUN = function(x) 
  predict(x, type = "link", 
          newdata = as.data.frame(euc_pred)), 
  nsim = 1000,  type = 'parametric',   parallel = 'multicore', ncpus = 4,  verbose = TRUE)

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
	mutate(scenario = paste("Q", c(1:3), sep = "")) %>% 
  	rename(ci.low = lwr, ci.hi = upr, estimate = fit)
predictions

#  estimate  ci.low   ci.hi std.err scenario
#  -2.0605 -4.4185 0.31951  1.1834       Q1
#  -1.4967 -3.6584 0.79880  1.1171       Q2
#  -1.0970 -3.5117 1.27856  1.2371       Q3

#how to back transform the data?

write_csv(predictions, "/Users/april-martinig/Desktop/euc_seed0_predictions.csv")



####################################################
#############    length SE and CIs    ##############
####################################################

set.seed(1234)

merBoot2 <- lme4::bootMer(seed50, FUN = function(x) 
  predict(x, type = "link", 
          newdata = as.data.frame(euc_pred)), 
  nsim = 1000,  type = 'parametric',   parallel = 'multicore', ncpus = 4,  verbose = TRUE)

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
	mutate(scenario = paste("Q", c(1:3), sep = "")) %>% 
  	rename(ci.low = lwr, ci.hi = upr, estimate = fit)
predictions2

#  estimate  ci.low  ci.hi std.err2 scenario
# -1.11052 -4.0444 2.1610   1.6057       Q1
# -0.84349 -3.3347 2.2279   1.5012       Q2
# -0.96648 -3.6806 2.4287   1.5783       Q3

#how to back transform the data?

write_csv(predictions2, "/Users/april-martinig/Desktop/euc_seed50_predictions.csv")


##############################################
#############    extract dfs    ##############
##############################################

## load lmerTest package
library(lmerTest)
library(parameters)

seed02<-lmerTest::lmer(euc0 ~ ea_grass + ep_grass + np_grass + np_other_grass + month + precip + (1|prop_id), data = euc)

seed502<-lmerTest::lmer(euc50 ~ ea_grass + ep_grass + np_grass + np_other_grass + month + precip + (1|prop_id), data = euc)

# Extract Adjusted Degrees of freedom using summary()
summary(seed02)$coef[,3]
summary(seed502)$coef[,3]

#what to do with your estimate, SE, and df?
#pick one main effect of interest to submit in survey
#go with ep_grass because it has a bigger effect