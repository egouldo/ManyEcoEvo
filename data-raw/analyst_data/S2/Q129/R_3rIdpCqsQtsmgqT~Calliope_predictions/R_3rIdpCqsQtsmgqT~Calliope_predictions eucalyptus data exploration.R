#code to go through the data exploration steps in the eucalyptus dataset as outlined by Zuur et al. 2010 recommendations
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
library(dplyr) 
library(tidyverse)
library(lattice)
library(lubridate)

euc<-read.csv("/Users/april-martinig/Documents/Files/Manuscripts/Parker et al. 2021/Eucalyptus Analysis/Euc_data.csv", header=T) %>% #loading original dataset without any modifications
	#changing variable names and setting them as numeric or factor
	mutate(
		id=SurveyID, 
		date=dmy(Date), 
		year=as.factor(year(date)),
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
#selecting which variables to keep based on model that might be run later
	select(id, year, month, season, prop_id, quad, ea_grass, ep_grass,  np_grass, np_other_grass, euc0, euc50, euc2, precip)

#externally setting these variables as factors as this allows you to rename them at the same time	
euc$month <- factor(euc$month, levels = c(4, 5, 7, 10, 11, 12), labels = c("April", "May", "July", "October", "November", "December"))


summary(euc)
head(euc)
names(euc)

#count unique observations
(euc) %>% as_tibble() %>% count(prop_id) #18 properties
(euc) %>% as_tibble() %>% count(quad) #11 quadrants
nrow(euc) #351 observations



######################################
#####  Getting down to business  #####
######################################

#starting model could include all of these variables: year, month, season, prop_id, quad, ea_grass, ep_grass,  np_grass, np_other_grass, euc0, euc50, euc2, precip



#####################################
######  checking for outliers  ######
#####################################

#create a new dataset for subset of variables
#limited to a small number so the plots aren't unreasonably large
Z <- cbind(euc$year, euc$month, euc$season, euc$euc0, euc$euc50, euc$euc2, euc$precip)

#name the column headers
colnames(Z) <- c("year", "month", "season", "euc0", "euc50", "euc_2", "precip")

#plot output to see relationship
dotplot(as.matrix(Z), groups = FALSE,
        strip = strip.custom(bg = 'white',
        par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")

#investigate further the points outside the clouds - particularly for euc0, euc50, euc2:
boxplot(euc$euc0) #outliers - investigate
boxplot(euc$euc50) #outliers - investigate
boxplot(euc$euc2) #outliers - investigate

plot(euc$euc0)
euc%>%filter(euc0>20) #id 44 has a count of 78 - keep an eye on this one
euc%>%filter(prop_id %in% c("Kellock")) #inconsistent within quadrant over time and inconsistent with nearby quadrants

plot(euc$euc50) #has other observations nearby, likely a real observation
plot(euc$euc2) #has other observations nearby, likely a real observation


#create another dataset for another subset of variables
Y <- cbind(euc$ea_grass, euc$ep_grass,  euc$np_grass, euc$np_other_grass)

#name the column headers
colnames(Y) <- c("ea_grass", "ep_grass", "np_grass", "np_other_grass")

#plot output to see relationship
dotplot(as.matrix(Y), groups = FALSE,
        strip = strip.custom(bg = 'white',
        par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")

#investigate further the points outside the clouds:
boxplot(euc$np_other_grass) #outliers - investigate
boxplot(euc$ep_grass) #outliers - investigate
boxplot(euc$np_grass) #outliers - investigate


plot(euc$np_other_grass)
euc%>%filter(np_other_grass>10) #id 69 has a cover of 20% - keep an eye on this one
euc%>%filter(prop_id %in% c("McCracken")) #inconsistent within quadrant over time and inconsistent with nearby quadrants

plot(euc$bare)
euc%>%filter(bare>10) #has other observations nearby, likely a real observation

plot(euc$ep_grass)
euc%>%filter(ep_grass>50) #id 69 has a cover of 20% - keep an eye on this one
euc%>%filter(prop_id %in% c("Taylor")) #consistent with nearby quadrant

plot(euc$np_grass)
euc%>%filter(np_grass>50) #has other observations nearby, likely a real observation


######################################
#summary
#two observations are possible outliers
#keep an eye on Kellock property (id 78) and McCracken property (id 69)
######################################

 

#########################################
#######  homogeneity of variance  #######
#########################################
#these plots allow you to see the spread of variance within/among-groups

#should individuals be grouped regardless of the year, season, or month?                             

#euc0
A<-bwplot(euc0 ~ year, data = euc,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(1, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))
               
B<-bwplot(euc0 ~ month, data = euc,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(1, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))               

C<-bwplot(euc0 ~ season, data = euc,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(1, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))  

cowplot::plot_grid(A, B, C, labels=c("auto"), ncol = 3, nrow =1, align = "hv", label_x=0.89, label_y=1)

#shows that years/seasons should be considered in the same model because their variances are not different 
#months have different variances  
    
    
#euc50    
A<-bwplot(euc50 ~  year, data = euc,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(1, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))
               
B<-bwplot(euc50 ~ month, data = euc,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(1, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))               

C<-bwplot(euc50 ~ season, data = euc,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(1, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))  

cowplot::plot_grid(A, B, C, labels=c("auto"), ncol = 3, nrow =1, align = "hv", label_x=0.89, label_y=1)

#shows that years/seasons should be considered in the same model because their variances are not different
#months have different variances
                     
#euc2                     
A<-bwplot(euc2 ~  year, data = euc,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(1, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))
               
B<-bwplot(euc2 ~ month, data = euc,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(1, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))               

C<-bwplot(euc2 ~ season, data = euc,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(1, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))  

cowplot::plot_grid(A, B, C, labels=c("auto"), ncol = 3, nrow =1, align = "hv", label_x=0.89, label_y=1)
                   
#shows that years/seasons/months should NOT be considered in the same model because their variances are different                
              
              
#####################################
#summary
#year/season can be grouped for euc0 and euc50
#month must be included as an explanatory variable for all response variables
#year/season/month cannot be grouped for euc2
#####################################
 

#####################################
############  normality  ############
##################################### 

#looking at overall normality 
#see model code for residual normality for models

hist(euc$euc0,
     xlab = "Seedlings (0-50cm)", breaks = 30,
     main = "", ylab = "Frequency")
#0-inflated
nrow(euc%>%filter(euc0 > 0)) #40 non-zero values

histogram( ~ euc0 | year, type = "count",
    xlab = "Seedlings (0-50cm)",
    ylab = "Frequency",
    nint=30,layout=c(1,2),
    strip.left = strip.custom(bg = 'white'),
    strip = F,
    col.line = "black", col = "white",
    scales = list(x = list(relation = "same"),
                  y = list(relation = "same"),
                  draw = TRUE),
    subset = year =="2006" | year == "2007",
    data = euc)
#problem in both years

histogram( ~ euc0 | season, type = "count",
    xlab = "Seedlings (0-50cm)",
    ylab = "Frequency",
    nint=30,layout=c(1,3),
    strip.left = strip.custom(bg = 'white'),
    strip = F,
    col.line = "black", col = "white",
    scales = list(x = list(relation = "same"),
                  y = list(relation = "same"),
                  draw = TRUE),
    subset = season =="Spring" | season == "Winter" | season == "Autumn",
    data = euc)
#problem in all seasons

histogram( ~ euc0 | month, type = "count",
    xlab = "Seedlings (0-50cm)",
    ylab = "Frequency",
    nint=30,layout=c(2,3),
    strip.left = strip.custom(bg = 'white'),
    strip = F,
    col.line = "black", col = "white",
    scales = list(x = list(relation = "same"),
                  y = list(relation = "same"),
                  draw = TRUE),
    subset = month =="April" | month == "May" | month == "July" | month =="October" | month == "November" | month == "December",
    data = euc)
#problem in all months


hist(euc$euc50,
     xlab = "Seedlings (0-50cm)", breaks = 30,
     main = "", ylab = "Frequency")
#0-inflated
nrow(euc%>%filter(euc50 > 0)) #74 non-zero values

histogram( ~ euc50 | year, type = "count",
    xlab = "Seedlings (0-50cm)",
    ylab = "Frequency",
    nint=30,layout=c(1,2),
    strip.left = strip.custom(bg = 'white'),
    strip = F,
    col.line = "black", col = "white",
    scales = list(x = list(relation = "same"),
                  y = list(relation = "same"),
                  draw = TRUE),
    subset = year =="2006" | year == "2007",
    data = euc)
#problem in both years, bigger problem in 2006

histogram( ~ euc50 | season, type = "count",
    xlab = "Seedlings (0-50cm)",
    ylab = "Frequency",
    nint=30,layout=c(1,3),
    strip.left = strip.custom(bg = 'white'),
    strip = F,
    col.line = "black", col = "white",
    scales = list(x = list(relation = "same"),
                  y = list(relation = "same"),
                  draw = TRUE),
    subset = season =="Spring" | season == "Winter" | season == "Autumn",
    data = euc)
#problem in all seasons

histogram( ~ euc50 | month, type = "count",
    xlab = "Seedlings (0-50cm)",
    ylab = "Frequency",
    nint=30,layout=c(2,3),
    strip.left = strip.custom(bg = 'white'),
    strip = F,
    col.line = "black", col = "white",
    scales = list(x = list(relation = "same"),
                  y = list(relation = "same"),
                  draw = TRUE),
    subset = month =="April" | month == "May" | month == "July" | month =="October" | month == "November" | month == "December",
    data = euc)
#problem in all months



hist(euc$euc2,
     xlab = "Seedlings (0-50cm)", breaks = 30,
     main = "", ylab = "Frequency")
#0-inflated
nrow(euc%>%filter(euc2 > 0)) #9 non-zero values
#cannot include because there are too few non-zero observations

histogram( ~ euc2 | year, type = "count",
    xlab = "Seedlings (0-50cm)",
    ylab = "Frequency",
    nint=30,layout=c(1,2),
    strip.left = strip.custom(bg = 'white'),
    strip = F,
    col.line = "black", col = "white",
    scales = list(x = list(relation = "same"),
                  y = list(relation = "same"),
                  draw = TRUE),
    subset = year =="2006" | year == "2007",
    data = euc)
#problem in both years, bigger problem in 2006

histogram( ~ euc2 | season, type = "count",
    xlab = "Seedlings (0-50cm)",
    ylab = "Frequency",
    nint=30,layout=c(1,3),
    strip.left = strip.custom(bg = 'white'),
    strip = F,
    col.line = "black", col = "white",
    scales = list(x = list(relation = "same"),
                  y = list(relation = "same"),
                  draw = TRUE),
    subset = season =="Spring" | season == "Winter" | season == "Autumn",
    data = euc)
#problem in all seasons

histogram( ~ euc2 | month, type = "count",
    xlab = "Seedlings (0-50cm)",
    ylab = "Frequency",
    nint=30,layout=c(2,3),
    strip.left = strip.custom(bg = 'white'),
    strip = F,
    col.line = "black", col = "white",
    scales = list(x = list(relation = "same"),
                  y = list(relation = "same"),
                  draw = TRUE),
    subset = month =="April" | month == "May" | month == "July" | month =="October" | month == "November" | month == "December",
    data = euc)
#problem in all months
    
########################################
#summary
#data are not normally distrubuted
#must use a model the accounts for zero-inflation 
#cannot include euc2 because of small non-zero sample size 
########################################



########################################
############  collinearity  ############
########################################   

#testing for collinearity between all variables that may be biologically relevant to question

euc_no_NA<-euc%>%filter(!is.na(precip), !is.na(ea_grass))
summary(euc_no_NA)

attach(euc_no_NA);tt=cbind(year, ea_grass, ep_grass,  np_grass, np_other_grass, euc0, euc50, euc2, precip)
cor(tt)     
#no correlations, all are <0.45   

#note that month, year, and season are all confounded with each other
#dropping season
#consider dropping year because month will "capture" the year

########################################
#summary
#no correlations to worry about for the numerical variables
#season, year, and month are confounded
########################################


########################################
############  interactions  ############
######################################## 

#should interactions be considered? 
#looking for lines that are in different directions across plots
#empty cells also indicate interactions should NOT be considered

coplot(euc0 ~ ea_grass | precip * year, data=euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc0 ~ ep_grass | precip * year, data=euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc0 ~ np_grass | precip * year, data= euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc0 ~ np_other_grass | precip * year, data= euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })   
#worth including a precip*year interaction based on the above


coplot(euc0 ~ ea_grass | precip * month, data=euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc0 ~ ep_grass | precip * month, data=euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc0 ~ np_grass | precip * month, data= euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc0 ~ np_other_grass | precip * month, data= euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })          
#cannot include a precip*month interaction because of holes in the data collection         
         
              

coplot(euc50 ~ ea_grass | precip * year, data=euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc50 ~ ep_grass | precip * year, data=euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc50 ~ np_grass | precip * year, data= euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc50 ~ np_other_grass | precip * year, data= euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })      
#worth including a precip*year interaction based on the above


coplot(euc50 ~ ea_grass | precip * month, data=euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc50 ~ ep_grass | precip * month, data=euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc50 ~ np_grass | precip * month, data= euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc50 ~ np_other_grass | precip * month, data= euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })     
#cannot include a precip*month interaction because of holes in the data collection 




coplot(euc2 ~ ea_grass | precip * year, data=euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc2 ~ ep_grass | precip * year, data=euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc2 ~ np_grass | precip * year, data= euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc2 ~ np_other_grass | precip * year, data= euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })   
#worth including a precip*year interaction based on the above


coplot(euc2 ~ ea_grass | precip * month, data=euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc2 ~ ep_grass | precip * month, data=euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc2 ~ np_grass | precip * month, data= euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(euc2 ~ np_other_grass | precip * month, data= euc,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })           
#cannot include a precip*month interaction because of holes in the data collection 


########################################
#summary      
#can include one interaction if sample size permits:
#precip*year
########################################



########################################
############  independence  ############
######################################## 

#repeated measures within properties and quadrants: 
#need to include prop_id and/or quad as random effects




########################################
#remaining variables for consideration in model: 

#response variables
euc0 #number of seedlings <50 cm tall #40 non-zero values
euc50 #number of seedlings between 50-200 cm tall #74 non-zero values

#explanatory variables
#variables that need to be included to account for environmental or seasonal differences, but are not related to question:
year*precip
month

#remaining variables related to grass cover:
ea_grass #exotic annual grass
ep_grass #exotic perennial grass
np_grass #native perennial grass
np_other_grass #natice perennial graminoids (grass-like spp)

#random effects:
prop_id #property identity
quad #quadrant identity

########################################
#"effective" sample size issues mean that I have ~4-7 degrees of freedom to play with, depending on the response variable... will drop the interaction and year to be conservative

#final variables for model: ea_grass + ep_grass + np_grass + np_other_grass + month + precip + (1|prop_id) + (1|quad)