#
#Code from the meet up on 2/23/2024 

#This script goes over how to combine data sets and how to do multiple 
#type of regression or correlation analysis


#other notes
#stacked data frames 
#meta_data (see densiometer for example meta-data)

#' [packages you may need]
#' 

library(ggplot2)
library(ggthemes)
library(tidyverse)
library(agricolae)
library(plyr)
library(lmodel2)
library(rstatix)

#' [import dataset]
#import, merge and clean data 

lux<- lux_CostaRica #read in lux
View(lux)

Dense<- Dense #read in densiometer 
View(Dense)

#subset before merging (we did this just for the meeting 
#but this is how you can subset your data if youd like )

lux.sub <- subset(lux, trail=="Danta")
View(lux.sub)

dens.sub <- subset(Dense, trail=="Danta")
dens.sub$percent.open <-  (dens.sub$square.num *1.04) #we need to calculate the percent open canopy 
View(dens.sub)                                        #from the number of open dots counted 
                                                      #this line asigns a new column to the data set we already have 


#Here we find the average of the two measuerments we took by using ddply -->library (plyr)

avg.lux <- ddply(lux.sub, .(distance), summarize, MeanLux= mean(lux, na.rm=T)) #this makes a new data set 
#ddply(dataset, .(columns want to isolate), sumarize, colname=command(#'s to crunch, na.rm=T))
View(avg.lux)

avg.dense <- ddply(dens.sub, .(distance), summarize, Meanpercent= mean(percent.open, na.rm=T))
View(avg.dense)

#we can join these two data sets using a shared value in each dataset --> distance
merged <- merge(avg.dense, avg.lux, by ="distance", all = TRUE )
#merge(df1, df2, by="joining variable", all=T ??)
View(merged)

#merged <- merged[-26,] #REMEMBER ME we just did this to get rid of outlier, IDK how to hide data yet 

#' [Graph regression]
#' 
#' 

#for your lichen's rember axsis may or may not matter based on what model you run
reg.lux <-ggplot(merged, aes(x = Meanpercent, y = MeanLux)) + 
  geom_point(size=3)+ #this will change the size of your dots 
  geom_smooth(method="lm",se=FALSE, color="black")+ #type of regression
  theme_tufte(base_size=20) + #theme to allow you to get rid of nasty base graphs 
  geom_rangeframe() + #will limit your axsis lines to the range of your data set, if it F's up just fix it in ppt for presenting
  scale_y_continuous() + #name axis here 
  scale_x_continuous()+ #name axsis here
  theme(axis.ticks.length=unit(-0.25, "cm"), #idk what this does other than do something with ticks 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))
reg.lux



#' [Regressions]
#' 

#' @model_1 --> predictive (x axis will predict your y axis values)

#run least squares regression
Regr.out <- lm(Meanpercent~MeanLux, merged) #lm(dependent(y)~independent(x),data)
summary(Regr.out)
anova(Regr.out)#analysis of F-ratio 

#Test assumptions
model1 <- lm(Meanpercent~MeanLux, merged)
summary(model1)
res <- residuals(model1)
shapiro.test(res) # if your p-value is .05 or less you cannot technically use 
                  #this model without transforming it, this just tests if your data is normally distributed 

#visual validation of normal distribution 
qqnorm(res)
qqline(res)
#if normal your data should look linear here, could also use the hist() fucntion 



#' @model_2 --> this is to quantify a relationship 

#run model 2, -->library(lmodel2)
Model2.out <- lmodel2(Meanpercent~MeanLux, data=merged) #arragment wont matter of x and y variables
Model2.out

#test assumuptions of bivariate normality
mshapiro_test(merged[,2:3]) #needs two columns for this command 
#if the p value is .05 or less it violates assumptions of normality and we cant technically run


#pearsons correlation anlysis 
#idk what this does, just a non-parametric way to see relationship, give a google for more info
b <- cor.test (merged$Meanpercent, merged$MeanLux, method="spearman")
b
#funny looking p --> is the rho value similar to like an R2 value in a linear regression


