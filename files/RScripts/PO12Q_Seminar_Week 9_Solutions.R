################################################
# PO12Q, WORKSHEET, WEEK 9
################################################

# Clear Memory
rm(list =ls())

# Set Working Directory
setwd('')

# Load packages
library(tidyverse)

# Load data

crime <- read.csv('crime.csv', header=TRUE, stringsAsFactors=TRUE)
names(crime)

##################################
### WORKING WITH REGRESSION
##################################


# Data Analysis
################################## 

# 1

# a
levels(crime$split)
crime.a<- subset.data.frame(crime, split == "A (Experience of the police)") 

# b
model.antisoc.rural.men <- lm(antisocx ~ rural2, subset(crime.a, crime.a$sex == "Male"))
summary(model.antisoc.rural.men)
# Yes the model does suggest men perceive more crime in urban areas with a significant positive coefficient for urban areas compared to rural areas. 

# c
model.antisoc.rural.women <- lm(antisocx ~ rural2, subset(crime.a, crime.a$sex == "Female"))
summary(model.antisoc.rural.women)
# The coefficient for rural areas is still highly significant and positive, slightly larger than the one for men. 




# 2
# a 
crime$wburgl[crime$wburgl=="Not Applicable"]<-NA
crime$wburgl[crime$wburgl=="Don't Know"]<-NA
crime$wburgl <- droplevels(crime$wburgl)

# b
table(crime$wburgl)
# Higher numbers will relate to less worried
model.burgle.age <- lm(as.numeric(wburgl) ~ as.numeric(agegrp7), crime)
summary(model.burgle.age)
# The coefficent for age is not significantly different from zero and therefore we cannot reject the null hypothesis that being worried about being burgled is not affected by age. 

# c
# Create a variable for whether the respondents are over 65.
retiree <- c("65-74","75+")
crime$retired <- ifelse(is.element(crime$agegrp7, retiree),"Retired", "Under 65")

# Use a linear model to test whether older people are more worried about burglary. Could also use a t-test
model.burgle.retired <- lm(as.numeric(wburgl) ~ retired, crime)
summary(model.burgle.retired)
# The coefficent for retirement is not significantly different from zero and therefore we cannot reject the null hypothesis.

# d
model.burgle.age.dummies <- lm(as.numeric(wburgl) ~ agegrp7, crime)
summary(model.burgle.age.dummies)
# The coefficents are not significantly different from zero and therefore we cannot reject the null hypothesis.

# e
# The intercept in the model from (b) represents the expected value in the wburgl scale when the age group is at 0 which is lower than the value assigned to the lowest age group. It is therefore not very meaningful.
# The intercept in the model from (c) represents the expected value in the wburgl scale for respondents over 65.
# The intercept in the model from (d) represents the expected value in the wburlg scale for respondents aged 18-24.


#################
### GOING FURTHER
#################


# 1
# a
# Recoding all answers outside scale as NA
crime$wattack[crime$wattack=="Not Applicable"]<-NA
crime$wattack[crime$wattack=="Don't Know"]<-NA
crime$wattack <- droplevels(crime$wattack)

crime$wburgl[crime$wburgl=="Not Applicable"]<-NA
crime$wburgl[crime$wburgl=="Don't Know"]<-NA
crime$wburgl <- droplevels(crime$wburgl)

crime$wmugged[crime$wmugged=="Not Applicable"]<-NA
crime$wmugged[crime$wmugged=="Don't Know"]<-NA
crime$wmugged <- droplevels(crime$wmugged)

crime$wraped[crime$wraped=="Not Applicable"]<-NA
crime$wraped[crime$wraped=="Don't Know"]<-NA
crime$wraped <- droplevels(crime$wraped)

crime$wraceatt[crime$wraceatt=="Not Applicable"]<-NA
crime$wraceatt[crime$wraceatt=="Don't Know"]<-NA
crime$wraceatt <- droplevels(crime$wraceatt)





# Levels show that smallest value is very worried and largest is not at all. This needs to be recoded into a number where 0 indicates not worried and 3 indicates very worried.
levels(crime$wburgl)
crime <- crime %>%
  mutate(wburgl.num = recode(wburgl, "Very worried"=3,                            
                             "Fairly worried"= 2,
                             "Not very worried"= 1,
                             "Not at all worried" = 0))

crime <- crime %>%
  mutate(wmugged.num = recode(wmugged, "Very worried"=3,
                              "Fairly worried"= 2,
                              "Not very worried"= 1,
                              "Not at all worried" = 0))

crime <- crime %>%
  mutate(wraped.num = recode(wraped, "Very worried"=3,
                             "Fairly worried"= 2,
                             "Not very worried"= 1,
                             "Not at all worried" = 0))

crime <- crime %>%
  mutate(wattack.num = recode(wattack, "Very worried"=3,
                              "Fairly worried"= 2,
                              "Not very worried"= 1,
                              "Not at all worried" = 0))
crime <- crime %>%
  mutate(wraceatt.num = recode(wraceatt, "Very worried"=3,
                               "Fairly worried"= 2,
                               "Not very worried"= 1,
                               "Not at all worried" = 0))


crime$worry <- crime$wburgl.num + crime$wmugged.num + crime$wraped.num + crime$wattack.num + crime$wraceatt.num


##########################################################################
# BASE R
##########################################################################
#crime$wburgl.num <- as.factor (ifelse (crime$wburgl == 'Very worried', '3',
#                                       ifelse (crime$wburgl == 'Fairly worried', '2',
#                                               ifelse (crime$wburgl == 'Not very worried', '1',
#                                                       ifelse (crime$wburgl == 'Not at all worried', #'0', '')))))
#
#
#crime$wmugged.num <- as.factor (ifelse (crime$wmugged == 'Very worried', '3',
#                                        ifelse (crime$wmugged == 'Fairly worried', '2',
#                                                ifelse (crime$wmugged == 'Not very worried', '1',
#                                                        ifelse (crime$wmugged == 'Not at all worried', #'0', '')))))
#
#
#crime$wraped.num <- as.factor (ifelse (crime$wraped == 'Very worried', '3',
#                                       ifelse (crime$wraped == 'Fairly worried', '2',
#                                               ifelse (crime$wraped == 'Not very worried', '1',
#                                                       ifelse (crime$wraped == 'Not at all worried', #'0', '')))))
#
#
#crime$wattack.num <- as.factor (ifelse (crime$wattack == 'Very worried', '3',
#                                        ifelse (crime$wattack == 'Fairly worried', '2',
#                                                ifelse (crime$wattack == 'Not very worried', '1',
#                                                        ifelse (crime$wattack == 'Not at all worried', #'0', '')))))
#
#
#crime$wraceatt.num <- as.factor (ifelse (crime$wraceatt == 'Very worried', '3',
#                                         ifelse (crime$wraceatt == 'Fairly worried', '2',
#                                                 ifelse (crime$wraceatt == 'Not very worried', '1',
#                                                         ifelse (crime$wraceatt == 'Not at all worried'#, '0', '')))))
#
#crime$worry <- as.numeric (crime$wburgl.num) + as.numeric (crime$wmugged.num) + as.numeric (crime$wraped.num) + as.numeric (crime$wattack.num) + as.numeric (crime$wraceatt.num)
##########################################################################


# b
summary(crime$worry)
# Median is 3, mean is 2.549
table(crime$worry)
# Mode is 4


# c
levels(crime$educat3)
edu.tab<-table(crime$educat3)
prop.table(edu.tab)
# educat3 has five levels. The first four represent increasing qualification levels. "Other" does not clearly fit into this order and therefore if educat3 was to be used as a continuous variable this should be removed.  

# d
# Using it a continuous variable requires using as.numeric and the exclusion of those responding "other." This can be done by making a new variable or when specifying the data in the model, as below.
model.worry.educat <- lm(worry~as.numeric(educat3), subset.data.frame(crime, educat3!="Other"))
summary(model.worry.educat)
# Treating educat3 as a continuous variable show a significant positive effect implying higher levels of education leads to more worrying about crime. This is the opposite direction tot he one hypothesized. 

# When using educat3 as a factor variable it is not necessary to exclude those who answered "Other" I have here for easier comparisons between the models.
crime <- crime %>% 
  mutate(educat3 = relevel(educat3, ref="None"))

model.worry.educat.factor<- lm(worry ~ educat3, subset.data.frame(crime, educat3!="Other"))
summary(model.worry.educat.factor)
# When compared to those with no qualifications those with Degree or Diploma or O levels / GCSE are more worried to be a victim of crime. 

# e
summary(model.worry.educat)
# R-squared is 0.02793
summary(model.worry.educat.factor)
# R-squared is 0.0252 
# Both of these values are very small with around 3% of the variation in worry explained by education. 

# f
#This can be done using the standard errors from the summary of the model and the corresponding t-value. Or we can use the command confint.
confint(model.worry.educat.factor, "educat3O level/GCSE", level = 0.975)
confint(model.worry.educat.factor, "educat3Degree or diploma", level = 0.975)
# As the high band of the confidence interval for those with a degree is lower than the low band for those without a degree we can say with greater than 97.5% confidence that those with degrees worry less about crime than those with only O-levels or GCSEs.




##################################
### TRANSFORMATION OF VARIABLES
##################################

# 1.
# Clearing and importing the london dataset 
rm(list =ls())
london <- read.csv('london_exercises_9.csv',header = T, stringsAsFactors = T)

#Dplyr solution:
london <- london %>% mutate(unemp_rate = (adults - employed)/adults)
#Base solution:
london$unemp_rate = with(london, (adults - employed)/adults)

# 2.
# a.
ggplot(london, aes(x = unemp_rate, y = crime)) + 
  geom_point() + geom_smooth(method = 'lm') +
  xlab('Unemployment Rate') + ylab('Crime Rate')
# There is a positive relationship with higher unemployment associated with higher crime. There are three major outlying observations.

##########################################################################
# BASE R
##########################################################################
#plot (london$unemp_rate, london$crime,
#      xlab = 'Unemployment Rate', ylab = 'Crime Rate')
#abline (lm (london$crime ~ london$unemp_rate))
##########################################################################


# b.
ggplot(subset(london, crime < 500),aes(x = unemp_rate, y = crime)) + 
  geom_point() + geom_smooth(method = 'lm') +
  xlab('Unemployment Rate') + ylab('Crime Rate')
# Removing the outliers doesn't affect the trend but does accentuate the variability of the data. It highlights a few higher crime rate wards away from the best fit line.

##########################################################################
# BASE R
##########################################################################
#londonc <- london [london$crime < 500,]
#plot (londonc$unemp_rate, londonc$crime,
#      xlab = 'Unemployment Rate', ylab = 'Crime Rate')
#abline (lm (londonc$crime ~ londonc$unemp_rate))
##########################################################################

# c.
ggplot(subset(london, crime < 500),
       aes(x = (unemp_rate), y = log(crime))) + geom_point() + 
  xlab('Unemployment Rate') + ylab('Logged Crime Rate') + geom_smooth(method = 'lm')  
# There is still a positive trend as show by the best fit line. The above line outliers from the previous graph have been reduced in severity. 

##########################################################################
# BASE R
##########################################################################
#londonc$logcrime <- log (londonc$crime)
#plot (londonc$unemp_rate, londonc$logcrime,
#      xlab = 'Unemployment Rate', ylab = 'Logged Crime Rate')
#abline (lm (londonc$logcrime ~ londonc$unemp_rate))
##########################################################################


# d.
model_crime_unemp <- lm(crime ~ unemp_rate, subset(london, crime < 500))
summary(model_crime_unemp)
# There is a positive effect for the unemployment rate in a ward on the crime rate in that ward. The slope coefficient is 176.31. This means that an increase of 0.1 in the unemployment rate, equivalent to 10% more of the population unemployed, results in an increase of 176 crimes per 1,000. This effect is highly significant with a p-value smaller than 0.05. The interval represents the number of crimes per 1,000 people at an unemployment rate of zero. 

model_crime_unemp_log <- lm(log(crime) ~ unemp_rate, subset(london, crime < 500))
summary(model_crime_unemp_log)
# The positive significant effect is still seen with the log transformation. As the dependent variable is log transformed the interpretation is different. The intercept now represents the log crime rate when unemployment is at zero. The true crime rate can be calculated by taking the exponential of the intercept.
#The R2 of the logarithmized model is higher, indicating that this is a better fit for the data. 






