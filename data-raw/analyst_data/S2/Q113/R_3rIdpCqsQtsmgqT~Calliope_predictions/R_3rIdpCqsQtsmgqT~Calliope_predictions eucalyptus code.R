#code to generate results and graphs for eucalyptus dataset after data exploration (see exploration code)
#Research question: "How does grass cover influence Eucalyptus spp. seedling recruitment?"
#last edited May 19, 2020 by A. R. Martinig


#clear memory
rm(list=ls(all=T))

options(dplyr.width = Inf, tibble.print_min = 200, digits=5, scipen=999)
#dplyr.width will show all columns
#tibble.print_min sets how many rows are printed
#digits is number of digits in output
#scipen=999 removes scientific notation

#loading required packages
lapply(c("dplyr", "tidyverse", "lme4", "lubridate", "glmmTMB"), require, character.only = TRUE)

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
		!is.na(ea_grass)) %>%
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

summary(euc)
head(euc)
names(euc)

#count unique observations
(euc) %>% as_tibble() %>% count(prop_id) #18 properties
(euc) %>% as_tibble() %>% count(quad) #11 quadrants
nrow(euc) #346 observations

##################################################
#############    seedlings <50 cm	 #############
#############	  and 50-200 cm		 #############
##################################################

mean(euc$euc0)
var(euc$euc0)

mean(euc$euc50)
var(euc$euc50)

#in all cases, the mean < var - i.e., overdispersed
#count data is normally modelled with a poisson distribution, but because it is overdispersed, a negative binomial distribution is necessary

#NOTE:
#(1|quad) variance is zero, so dropped from model

seed0<-glmmTMB(euc0 ~ ea_grass + ep_grass + np_grass + np_other_grass + month + precip + (1|prop_id), data = euc, family= nbinom2) #nbimom2 lets var increases quadratically
summary(seed0)
confint(seed0)

seed50<-glmmTMB(euc50 ~ ea_grass + ep_grass + np_grass + np_other_grass + month + precip + (1|prop_id), data = euc, family= nbinom2) #nbimom2 lets var increases quadratically
summary(seed50)
confint(seed50)