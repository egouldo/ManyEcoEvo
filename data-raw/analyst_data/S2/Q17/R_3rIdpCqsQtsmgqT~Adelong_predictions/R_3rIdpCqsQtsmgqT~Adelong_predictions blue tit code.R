#code to generate results and graphs for blue tit dataset after data exploration (see exploration code)
#Research question: "To what extent is the growth of nestling blue tits (Cyanistes caeruleus) influenced by competition with siblings?"
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


blue<-read.csv("/Users/april-martinig/Documents/Files/Manuscripts/Parker et al. 2021/Blue Tit Analysis/blue_tit_data_NA.csv", header=T) %>%  #loading original dataset after replacing all blanks with NAs
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
		!chick_ring_number %in% c("P810753", "P808581")) %>%
#standardizing w/in year, area, and treatment
	group_by(year, hatch_area, rear_trt) %>% 
	mutate(
		hatch_CS2 =((hatch_CS-mean(hatch_CS))/(1*(sd(hatch_CS)))),
		hatch_OH2 =((hatch_OH-mean(hatch_OH))/(1*(sd(hatch_OH)))), 
		d0_hatch_brood_size2 =((d0_hatch_brood_size-mean(d0_hatch_brood_size))/(1*(sd(d0_hatch_brood_size)))),
		d14_rear_brood_size2 =((d14_rear_brood_size-mean(d14_rear_brood_size))/(1*(sd(d14_rear_brood_size)))),
		rear_Cs2 =((rear_Cs-mean(rear_Cs))/(1*(sd(rear_Cs)))),
		wt2 =((wt-mean(wt))/(1*(sd(wt)))),
		tarsus2 =((tarsus-mean(tarsus))/(1*(sd(tarsus)))),
		related2 =((related-mean(related))/(1*(sd(related))))
		) %>% 
#fixing any cases where standardization resulted in NAs because you cannot divide by 0 (this happens when all values within grouping are the same) - replace with mean
	replace(is.na(.), 0) %>% 
#drop any unused levels
	droplevels()	 	

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



#########################################
#############    weight    ##############
#########################################

weight<-lmer(wt ~ hatch_OH + hatch_CS + d0_hatch_brood_size + rear_Cs + d14_rear_brood_size + rear_trt +  related + hatch_area + year + observer_id + (1|hatch_id), data=blue)
summary(weight) 
 
#verifying model assumptions 
plot(weight) #visual inspection shows no trends
#visually and quantitatively assessing variances of residuals
hist(resid(weight)) 
blue$m1.wo.resid<- residuals(weight) #extracts the residuals and places them in a new column in our original data table
blue$abs.m1.wo.resid <-abs(blue$m1.wo.resid) #creates a new column with the absolute value of the residuals
blue$m1.wo.resid2 <- blue$abs.m1.wo.resid^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.Model.F <- lm(m1.wo.resid2 ~ hatch_id, data=blue) #ANOVA of the squared residuals
anova(Levene.Model.F) #displays the results

#extracting model estimates and credible intervals
sm.weight <-arm::sim(weight, n.sim=1000)
smfixef.weight = sm.weight@fixef
smfixef.weight = coda::as.mcmc(smfixef.weight)
MCMCglmm::posterior.mode(smfixef.weight) #mode of the distribution
coda::HPDinterval(smfixef.weight) #credible intervals

#among-brood variance
blitter<-sm.weight@ranef$hatch_id[,,1]
bvar<-as.vector(apply(blitter, 1, var)) #between brood variance posterior distribution
bvar<-coda::as.mcmc(bvar)
MCMCglmm::posterior.mode(bvar) #mode of the distribution
coda::HPDinterval(bvar) #credible intervals

#extract SEs
s <- summary(smfixef.weight)$statistics
s[grep("hatch_OH|hatch_CS|d0_hatch_brood_size|rear_Cs|d14_rear_brood_size|rear_trt|related|hatch_area|year|observer_id", rownames(s)), grep("SE", colnames(s))]
#"naive standard error of the mean [ignores] autocorrelation of the chain ... [whereas] time-series standard error [are] based on an estimate of the spectral density at 0"


#########################################
#############    tarsus    ##############
#########################################

length<-lme4::lmer(tarsus ~ hatch_OH + hatch_CS + d0_hatch_brood_size + rear_Cs + d14_rear_brood_size + rear_trt +  related + hatch_area + year + observer_id + (1|hatch_id), data=blue)
summary(length)

#verifying model assumptions 
plot(length)#visual inspection shows no trends
#visually and quantitatively assessing variances of residuals
hist(resid(length))
test$m1.wo.resid<- residuals(length) #extracts the residuals and places them in a new column in our original data table
test$abs.m1.wo.resid <-abs(test$m1.wo.resid) #creates a new column with the absolute value of the residuals
test$m1.wo.resid2 <- test$abs.m1.wo.resid^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.Model.F <- lm(m1.wo.resid2 ~ rear_id, data=test) #ANOVA of the squared residuals
anova(Levene.Model.F) #displays the results

#extracting model estimates and credible intervals
sm.tarsus <-arm::sim(length, n.sim=1000)
smfixef.tarsus = sm.tarsus@fixef
smfixef.tarsus =coda::as.mcmc(smfixef.tarsus)
MCMCglmm::posterior.mode(smfixef.tarsus)  #mode of the distribution
coda::HPDinterval(smfixef.tarsus)  #credible intervals

#among-brood variance
blitter<-sm.tarsus@ranef$hatch_id[,,1]
bvar<-as.vector(apply(blitter, 1, var)) #between brood variance posterior distribution
bvar<-coda::as.mcmc(bvar)
MCMCglmm::posterior.mode(bvar) #mode of the distribution
coda::HPDinterval(bvar) #credible intervals

#extract SEs
t <- summary(smfixef.tarsus)$statistics
t[grep("hatch_OH|hatch_CS|d0_hatch_brood_size|rear_Cs|d14_rear_brood_size|rear_trt|related|hatch_area|year|observer_id", rownames(t)), grep("SE", colnames(t))]