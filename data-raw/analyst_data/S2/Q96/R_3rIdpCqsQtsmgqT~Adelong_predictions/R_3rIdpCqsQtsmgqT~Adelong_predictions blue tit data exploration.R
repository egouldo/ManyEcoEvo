#code to go through the data exploration steps in the blue tit dataset as outlined by Zuur et al. 2010 recommendations
#Research question: "To what extent is the growth of nestling blue tits (Cyanistes caeruleus) influenced by competition with siblings?"
#last edited April 23, 2020 by A. R. Martinig

#clear memory
rm(list=ls(all=T))

options(dplyr.width = Inf, tibble.print_min = 200, digits=5, scipen=999) 
#dplyr.width will show all columns
#tibble.print_min sets how many rows are printed
#digits is number of digits in output
#scipen=999 removes scientific notation

#loading required packages
lapply(c("dplyr", "tidyverse", "lattice"), require, character.only = TRUE)

blue<-read.csv("/Users/april-martinig/Desktop/Same data, different analysts/Blue Tit Analysis/blue_tit_data.csv", header=T) %>% #loading original dataset without any modifications
	#changing variable names and setting them as numeric or factor	
	mutate(
		wt=as.numeric(day_14_weight), 
		tarsus=as.numeric(day_14_tarsus_length), 
		year=as.factor(hatch_year), 
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
		day14=as.numeric(Date_of_day14)) %>%
#selecting which variables to keep (also renaming any remaining variables) based on model that might be run later
	select(chick_ring_number, year, hatch_id=hatch_nest_breed_ID, 
		hatch_area=hatch_Area, hatch_LD, 
		hatch_CS, hatch_OH, d0_hatch_brood_size, 
		d14_hatch_brood_size, rear_id=rear_nest_breed_ID, 
		rear_area, rear_trt=rear_nest_trt, 
		home_or_away, rear_LD, rear_CS, rear_OH, 
		d0_rear_brood_size, rear_Cs_out, rear_Cs_in, 
		net_manipulation, rear_Cs, 
		d14_rear_brood_size, day14, tarsus, wt, 
		observer_id=day14_measurer, sex=chick_sex_molec)

#externally setting these variables as factors as this allows you to rename them at the same time	
blue$sex <- factor(blue$sex, levels = c("1", "2"), labels = c("Female", "Male"))
blue$home_or_away <- factor(blue$home_or_away, levels = c(1, 2), labels = c("Home", "Away"))
blue$rear_trt <- factor(blue$rear_trt, levels = c(5, 6, 7), labels = c("Increase", "Decrease", "No_trt"))
blue$observer_id <- factor(blue$observer_id, levels = c(1, 2, 3, 4), labels = c("Person1", "Person2", "Person3", "Person4"))

summary(blue)
head(blue)
names(blue)

#count unique observations
(blue) %>% as_tibble() %>% count(chick_ring_number) #3720 inds
(blue) %>% as_tibble() %>% count(hatch_id) #497 hatch ids
(blue) %>% as_tibble() %>% count(rear_id) #452 rear ids



######################################
#####  Getting down to business  #####
######################################

#starting model could include all of these variables: year, hatch_area, hatch_LD, hatch_CS, hatch_OH, d0_hatch_brood_size, d14_hatch_brood_size, rear_area, rear_trt, home_or_away, rear_LD, rear_CS, rear_OH, d0_rear_brood_size, rear_Cs_out, rear_Cs_in, net_manipulation, rear_Cs, d14_rear_brood_size, day14, observer_id, sex



#####################################
######  checking for outliers  ######
#####################################

#create a new dataset for subset of variables
#limited to a small number so the plots aren't unreasonably large
Z <- cbind(blue$wt, blue$tarsus,  blue$year, blue$rear_trt,  blue$rear_Cs_out, blue$rear_Cs_in, blue$net_manipulation, blue$rear_Cs, blue$day14)

#name the column headers
colnames(Z) <- c("day_14_weight", "day_14_tarsus_length", "hatch_year", "rear_nest_trt", "rear_Cs_out", "rear_Cs_in", "net_rearing_manipulation", "rear_Cs_at_start_of_rearing", "Date_of_day14")

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

#investigate further the points outside the clouds - particularly for wt and tarsus:
boxplot(blue$wt) #outliers - investigate
boxplot(blue$tarsus) #outliers - investigate

plot(blue$day14, blue$wt)
blue%>%filter(wt<5.5) #chick P810753 has a weight of 4.5 - keep an eye on this one
plot(blue$day14, blue$tarsus)
blue%>%filter(tarsus<13) #chick P808581 has a length of 12.4 - keep an eye on this one

#create another dataset for another subset of variables
Y <- cbind(blue$hatch_LD, blue$hatch_CS, blue$hatch_OH, blue$d0_hatch_brood_size, blue$d14_hatch_brood_size, blue$rear_LD, blue$rear_CS, blue$rear_OH, blue$d0_rear_brood_size,  blue$d14_rear_brood_size)

#name the column headers
colnames(Y) <- c("hatch_nest_LD", "hatch_nest_CS", "hatch_nest_OH", "d0_hatch_nest_brood_size", "d14_hatch_nest_brood_size", "rear_nest_LD", "rear_nest_CS", "rear_nest_OH", "rear_d0_rear_nest_brood_size", "d14_rear_nest_brood_size")

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
boxplot(blue$hatch_LD)
boxplot(blue$hatch_CS)
boxplot(blue$hatch_OH)
boxplot(blue$d0_hatch_brood_size)
boxplot(blue$d14_hatch_brood_size)
boxplot(blue$rear_LD)
boxplot(blue$rear_CS)
boxplot(blue$rear_OH)
boxplot(blue$d0_rear_brood_size)
boxplot(blue$d14_rear_brood_size)
#no outliers

######################################
#summary
#two chicks are possible outliers
#keep an eye on P810753 and P808581
######################################

 

#########################################
#######  homogeneity of variance  #######
#########################################

#these plots allow you to see the spread of variance within/among-groups

#should sexes be grouped or separated?

bwplot(wt ~ home_or_away | sex, data = blue,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(2, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))
                     
bwplot(tarsus ~ home_or_away | sex, data = blue,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(2, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))      
                     
bwplot(wt ~ rear_trt | sex, data = blue,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(2, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))
                     
bwplot(tarsus ~ rear_trt | sex, data = blue,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(2, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))                     
#shows that females and males should be considered in the same model because their variances are not different 
                     
              
#should individuals be grouped regardless of there home or away status?                             
bwplot(wt ~ rear_trt | home_or_away, data = blue,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(2, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))
                     
bwplot(tarsus ~ rear_trt | home_or_away, data = blue,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(2, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))                          
#same story for the home vs. away categorization 

#should treatments be grouped?
bwplot(wt ~ home_or_away | rear_trt, data = blue,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(3, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))
                     
bwplot(tarsus ~ home_or_away | rear_trt, data = blue,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(3, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))                          
#rear_trt has different variances, so this will need to be accounted for

#can observer identity be ignored?
bwplot(wt ~ observer_id, data = blue,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(1, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))
                     
bwplot(tarsus ~ observer_id, data = blue,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(1, 1),
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))     
#keep an eye on observer_id 

#####################################
#summary
#sexes can be grouped
#home_or_away can be grouped
#rear_trt CANNOT be grouped
#keep an eye on observer_id
#####################################
 
#updated model (remove sex and home_or_away): year, hatch_area, hatch_LD, hatch_CS, hatch_OH, d0_hatch_brood_size, d14_hatch_brood_size, rear_area, rear_trt, rear_LD, rear_CS, rear_OH, d0_rear_brood_size, rear_Cs_out, rear_Cs_in, net_manipulation, rear_Cs, d14_rear_brood_size, day14, observer_id 
 
 

#####################################
############  normality  ############
##################################### 

#looking at overall normality 
#see model code for residual normality for models

hist(blue$wt,
     xlab = "Weight (g)", breaks = 30,
     main = "", ylab = "Frequency")
#normal

histogram( ~ wt | year, type = "count",
    xlab = "Weight (g)",
    ylab = "Frequency",
    nint=30,layout=c(1,3),
    strip.left = strip.custom(bg = 'white'),
    strip = F,
    col.line = "black", col = "white",
    scales = list(x = list(relation = "same"),
                  y = list(relation = "same"),
                  draw = TRUE),
    subset = year =="2001" | year == "2002" | year == "2003",
    data = blue)

hist(blue$tarsus,
     xlab = "Tarsus length (mm)", breaks = 30,
     main = "", ylab = "Frequency")
#normal

histogram( ~ tarsus | year, type = "count",
    xlab = "Tarsus (mm)",
    ylab = "Frequency",
    nint=30,layout=c(1,3),
    strip.left = strip.custom(bg = 'white'),
    strip = F,
    col.line = "black", col = "white",
    scales = list(x = list(relation = "same"),
                  y = list(relation = "same"),
                  draw = TRUE),
    subset = year =="2001" | year == "2002" | year == "2003",
    data = blue)    
    
########################################
#summary
#both look good    
########################################



########################################
###########  zero inflation  ###########
######################################## 
summary(blue) #no zeros far and wide



########################################
############  collinearity  ############
########################################   

#testing for collinearity between all variables that may be biologically relevant to question

attach(blue);tt=cbind(year, hatch_LD, hatch_CS, hatch_OH, d0_hatch_brood_size, d14_hatch_brood_size, rear_trt, rear_LD, rear_CS, rear_OH, d0_rear_brood_size, rear_Cs_out, rear_Cs_in, net_manipulation, rear_Cs, d14_rear_brood_size, day14, observer_id)
cor(tt)     

######################################## 
#keep all hatch variables
########################################    
#hatch_LD and rear_LD 0.77
#hatch_CS and rear_CS 0.72
#hatch_OH and rear_OH 0.9999
#hatch_OH and day14 0.99
#d0_hatch_brood_size and d0_rear_brood_size 0.77
#rear_OH and day14 0.99

######################################## 
#take out rear_Cs_in
######################################## 
#rear_trt and rear_Cs_in -0.87
#net_manipulation and rear_Cs_in 0.65

######################################## 
#keep earliest measure of hatching brood size and latest measure of rearing brood size
######################################## 
#d0_hatch_brood_size and d14_hatch_brood_size 0.61
#net_manipulation and d14_rear_brood_size 0.67

#can either area variable be used?
attach(blue);tt=cbind(hatch_area, rear_area)
cor(tt)   
#yes - highly correlated with each other 0.84
#use hatch_area for consistentcy with above
   
      
########################################
#summary
#updated model after removing correlated variables (see above): year, hatch_area, hatch_LD, hatch_CS, hatch_OH, d0_hatch_brood_size, rear_trt, d0_rear_brood_size, rear_Cs_out, rear_Cs, d14_rear_brood_size, observer_id  
########################################


########################################
############  interactions  ############
######################################## 

#should interactions be considered? 
#looking for lines that are in different directions across plots
#empty cells also indicate interactions should NOT be considered

coplot(wt ~ tarsus | rear_trt * year, data=blue,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(wt ~ tarsus | rear_trt * net_manipulation, data=blue,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(wt ~ tarsus | rear_trt * rear_Cs, data=blue,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(wt ~ tarsus | rear_trt * rear_Cs_out, data=blue,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })   

coplot(wt ~ tarsus | rear_trt * hatch_LD, data=blue,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(wt ~ tarsus | rear_trt * hatch_CS, data=blue,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })
         
coplot(wt ~ tarsus | rear_trt * hatch_OH, data=blue,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(wt ~ tarsus | rear_trt * d0_hatch_brood_size, data=blue,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(wt ~ tarsus | rear_trt * d14_hatch_brood_size, data=blue,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(wt ~ tarsus | rear_trt * d14_rear_brood_size, data=blue,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(wt ~ tarsus | year * observer_id, data=blue,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

########################################
#summary      
#not worth including any interactions
########################################



########################################
############  independence  ############
######################################## 

#repeated measures within nests: need to include brood_id (either hatch_id or rear_id) as random effect at minimum



########################################
#remaining variables for consideration in model: 

#variables that need to be included to account for environmental or observer differences, but are not related to question:
year
hatch_area
observer_id  

#remaining variables related to hatching:

hatch_LD #lay date
hatch_CS #clutch size
hatch_OH #hatch date
d0_hatch_brood_size #number of eggs that hatch

#keeping lay date and hatch date is redundant - both were standardized for, picking hatch date because it should control for a greater portion of variation between nests

#remaining variables related to rearing:
rear_trt #treatment variable is a must (see above)

d0_rear_brood_size #number of eggs that hatch
d14_rear_brood_size #number of nestlings left after 14d
rear_Cs_out #number of eggs removed
rear_Cs #clutch size after manipulations

#not keeping d0_rear_brood_size - variable is redundant with d0_hatch_brood_size because they are matched between manipulations
#not keeping rear_Cs_out - variable is redundant with rear_Cs (which gives the final clutch size)

########################################
#final variables for model: year + hatch_area + observer_id + hatch_CS + hatch_OH + d0_hatch_brood_size + rear_trt + d14_rear_brood_size + rear_Cs
