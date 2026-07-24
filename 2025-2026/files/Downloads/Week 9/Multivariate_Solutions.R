# *****************************************************************
# PO12Q - Quantitative Political Analysis: Uncovering Relationships
# Dr Flo Linke
# Week 9, Exercise Solutions - Multivariate Regression
# *****************************************************************

# For this script to work properly, follow the these steps:
# 1. Create a folder as your working directory
# 2. Set the working directory to this folder, and copy/paste the the 
#    file path into the ROOT object in the user settings below
# 3. Within that folder, create a subfolder called "data" 
# 4. Within the "data" folder, create a subfolder called "raw"
# 5. Download the "london_exercises.csv" file from the companion and
#    save it in the "raw" folder

# *****************************************************************
# USER SETTINGS
# *****************************************************************

# Set this to your working directory
ROOT <- "EXAMPLE/PATH"

# Set this to your data folder (likely a subfolder within your working directory)
DATA <- file.path(ROOT, "data")

# Set this to your raw data folder (likely a subfolder within your data folder)
RAW <- file.path(DATA, "raw")

# *****************************************************************
# SETUP AND PACKAGES
# *****************************************************************

library(ggplot2)
library(dplyr)
library(forcats)

# *****************************************************************
# GRAPH FORMATTING
# *****************************************************************

# Graph theme
theme_iqmss <- theme_classic() +
  theme(text = element_text(family = "sans"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(margin = margin(b = 10, t = 9)),
        axis.title.y = element_text(margin = margin(r = 12)),
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 14),
        axis.ticks.length = unit(.1, "cm")) +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

# Graph theme (with math notation)
theme_iqmss_math <- theme_classic() +
  theme(text = element_text(family = "sans"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(margin = margin(b = 10, t = 9)),
        axis.title.y = element_text(margin = margin(r = 12)),
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 14),
        axis.ticks.length = unit(.1, "cm")) +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

# *****************************************************************
# EXERCISE SOLUTIONS
# *****************************************************************

crime <- read.csv(file.path(RAW, "crime.csv"), header = TRUE, stringsAsFactors = TRUE)

# EXERCISE 1: Each respondent was randomly assigned to a different
# module, indicated by split, and only asked a subsection of the
# questions. Which question(s) are part of each module?

# One possible method to answer question:

# Count missing cases per variable
sapply(crime, function(x) sum(is.na(x)))
# resyrago, wdeprivex, cause2m, walkdark, walkday, homealon, wburgl, 
# wmugged, wcarstol, wfromcar, wraped, wattack, wraceatt, worryx and 
# antisocx all have a large number of missing entries. This is an 
# indication they may have been part of modules only shown to a subset 
# of the respondents. 

# Subset the data for each module
levels(crime$split)
crime.a <- subset.data.frame(crime, split == "A (Experience of the police)") 
crime.b <- subset.data.frame(crime, split == "B (Attitudes to the CJS)") 
crime.c <- subset.data.frame(crime, split == "C (Crime preventing)") 
crime.d <- subset.data.frame(crime, split == "D (Online security)") 

# Again, count missing cases per variable within each module
# A variable that has a significant number of missing entries in
# the larger crime dataset but few missing entries in a module
# subset is likely part of that module.

sapply(crime.a, function(x) sum(is.na(x)))
# antisocx is from module A

sapply(crime.b, function(x) sum(is.na(x)))
# wcarstol and wfromcar are from module B

sapply(crime.c, function(x) sum(is.na(x)))
# wburgl wmugged wraped, wattack, wraceatt and worryx are from module C

sapply(crime.d, function(x) sum(is.na(x)))
# cause2m, walkdark, walkday, homealon are from module D


# EXERCISE 2: The antisocx variable is part of module A and asks
# for a score from respondents on how much antisocial behaviour
# is in the neighbourhood.

# 2a: Create a new data set for those who were chosen for the "A"
# module. Call this crime.a
crime.a <- subset.data.frame(crime, split == "A (Experience of the police)") 

# 2b: Run a t-test to see whether women or men have greater perceptions
# of antisocial behaviour. Interpret the results.
with(crime.a, t.test(antisocx ~ sex))
# The mean perception of antisocial behaviour for men was statistically
# significantly different from that of women, with men on average
# perceiving lower levels of antisocial behaviour than women.

# 2c: Create a linear model with antisocx as the dependent variable
# and sex as the independent variable. Interpret the results. How does
# this differ from the t-test?
model.antisoc.sex <- lm(antisocx ~ sex, crime.a)
summary(model.antisoc.sex)
# The sex coefficient is the same as the difference between the means 
# calculated in the t-test. The significance level is also unchanged. 
# The key difference in the linear model is that you can add additional
# variables to the model.


# EXERCISE 3: Previous research has suggested men perceive more antisocial
# behaviour in rural areas than urban areas.

# 3a: Create a linear model using only male respondents from crime.a with
# the dependent variable antisocx and the independent variable rural2. Do
# your findings support previous research?

model.antisoc.rural.men <- lm(antisocx ~ rural2, subset(crime.a, crime.a$sex == "Male"))
summary(model.antisoc.rural.men)
# The statistically significant rural2 coefficient of 0.442 indicates that the 
# model does support previous research suggesting that men perceive more crime 
# in urban areas than rural areas. 

# 3b: Test the same model using only women. How do the two models differ?
model.antisoc.rural.women <- lm(antisocx ~ rural2, subset(crime.a, crime.a$sex == "Female"))
summary(model.antisoc.rural.women)

confint(model.antisoc.rural.men)
confint(model.antisoc.rural.women)
# The coefficient for rural2 is still statistically significant and positive,
# indicating that women similarly perceive more crime in urban areas than rural
# areas. The coefficient value of 0.57801 is greater than the coefficient
# value of 0.442 in the male only model. However, we cannot be confident
# this difference is statistically significant, as indicated by the overlapping 
# confidence intervals between the two models.

# 3c: Using interaction effects, test whether being in a rural area has a
# larger effect on women's perception of antisocial behaviour compared to
# men's? 
model.antisoc.rural.sex.interaction <- lm(antisocx ~ rural2 + sex + rural2*sex, crime.a)
summary(model.antisoc.rural.sex.interaction)
# Being in a urban compared to rural area has a statistically significant positive 
# effect on perceptions of antisocial behaviour. However, both the sex and interaction 
# effect are insignificant, indicating that there is no statistically significant
# difference in the effect of being in a rural area on women and men's perceptions
# of antisocial behaviour.

# 3d: How would you test whether being female has an effect on the perception
# of higher antisocial behaviour in urban compared to rural areas?

# Perform the regression with the interaction term described in 3c. 


# EXERCISE 4: The wburgl variable asks respondents how worried they are about
# being burgled with answers ranging from "Very worried" to "Not at all worried"
# along with "Not applicable" and "Don't know."

# 4a: Recode those with "Not applicable" or "Don't know" as NAs
crime$wburgl[crime$wburgl == "Not Applicable"] <- NA
crime$wburgl[crime$wburgl == "Don't Know"] <- NA
crime$wburgl <- droplevels(crime$wburgl)

# 4b: Using agegrp7 and wburgl as continuous variables, test the hypothesis
# that older people are more worried about being burgled.
model.burgle.age <- lm(as.numeric(wburgl) ~ as.numeric(agegrp7), crime)
summary(model.burgle.age)
# The coefficient for age is not statistically significantly different from 
# zero. We cannot reject the null hypothesis that being worried about being 
# burgled is not affected by age. 

# 4c: Using a dummy variable, test whether those over 65 are more worried about
# burglary than those who are younger.

# Create a variable for whether respondent is over 65
crime$retired <- factor(ifelse(crime$agegrp7 %in% c("65-74", "75+"), "Retired", "Under 65"), levels = c("Under 65", "Retired"))

# Use a linear model to test whether older people are more worried about burglary. Could also use a t-test
model.burgle.retired <- lm(as.numeric(wburgl) ~ retired, crime)
summary(model.burgle.retired)
# The coefficient for retirement is not statistically significantly different from 
# zero and therefore we cannot reject the null hypothesis that being worried
# about being burgled is not affected by being over 65 years old.

# 4d: Using dummies test whether any of the age groups differ significantly
# from the youngest age group
model.burgle.age.dummies <- lm(as.numeric(wburgl) ~ agegrp7, crime)
summary(model.burgle.age.dummies)
# The coefficients are not statistically significantly different from zero and 
# therefore we cannot reject the null hypothesis that being worried about
# being burgled is not affected by age.

# 4e: What does the intercept in each of the three models represent?
# The intercept in the model from (b) represents the expected value for wburgl 
# when the age group is 0 (lower than the value assigned to the lowest age group).
# It is therefore not very meaningful.
# The intercept in the model from (c) represents the expected value for wburgl 
# for respondents under 65.
# The intercept in the model from (d) represents the expected value for wburgl 
# for respondents aged 18-24.


# EXERCISE 5: There are five variables which ask how worried the respondent is
# about being the victim of various crimes: wburgl, wmugged, wraped, wattack,
# and wraceatt.

# 5a: Create an additive variable called worry from these variables so that a
# score of 0 indicates that the respondent answered "Not at all worried" and
# a score of 15 indicates the respondent answered all "Very worried." Tip:
# Make sure to clean the variables for NAs.

# Recode all answers outside scale as NA
crime$wattack[crime$wattack == "Not Applicable"] <- NA
crime$wattack[crime$wattack == "Don't Know"] <- NA
crime$wattack <- droplevels(crime$wattack)

crime$wmugged[crime$wmugged == "Not Applicable"] <- NA
crime$wmugged[crime$wmugged == "Don't Know"] <- NA
crime$wmugged <- droplevels(crime$wmugged)

crime$wraped[crime$wraped == "Not Applicable"] <- NA
crime$wraped[crime$wraped == "Don't Know"] <- NA
crime$wraped <- droplevels(crime$wraped)

crime$wraceatt[crime$wraceatt == "Not Applicable"] <- NA
crime$wraceatt[crime$wraceatt == "Don't Know"] <- NA
crime$wraceatt <- droplevels(crime$wraceatt)

# Note: The above code is a great example of where loops can simplify our code.
# See the example "for loop" below.
# for (col in c("wattack", "wmugged", "wraped", "wraceatt")) {
#   # Replace values
#   crime[[col]][crime[[col]] == "Not Applicable"] <- NA
#   crime[[col]][crime[[col]] == "Don't Know"] <- NA
#   
#   # Drop unused levels
#   crime[[col]] <- droplevels(crime[[col]])
# }

# Recode crime variables so 0 indicates not worried and 3 indicates very worried.
crime <- crime %>%
  mutate(wburgl = fct_relevel(wburgl,
                              "Not at all worried",
                              "Not very worried",
                              "Fairly Worried",
                              "Very Worried"),
         wburgl.num = as.numeric(wburgl) - 1
         ) 

crime <- crime %>%
  mutate(wmugged = fct_relevel(wmugged,
                              "Not at all worried",
                              "Not very worried",
                              "Fairly Worried",
                              "Very Worried"),
         wmugged.num = as.numeric(wmugged) - 1
  )

crime <- crime %>%
  mutate(wraped = fct_relevel(wraped,
                               "Not at all worried",
                               "Not very worried",
                               "Fairly Worried",
                               "Very Worried"),
         wraped.num = as.numeric(wraped) - 1
  )

crime <- crime %>%
  mutate(wattack = fct_relevel(wattack,
                               "Not at all worried",
                               "Not very worried",
                               "Fairly Worried",
                               "Very Worried"),
         wattack.num = as.numeric(wattack) - 1
  )

crime <- crime %>%
  mutate(wraceatt = fct_relevel(wraceatt,
                               "Not at all worried",
                               "Not very worried",
                               "Fairly Worried",
                               "Very Worried"),
         wraceatt.num = as.numeric(wraceatt) - 1
  )

# Create additive variable "worry"
crime$worry <- crime$wburgl.num + crime$wmugged.num + crime$wraped.num + crime$wattack.num + crime$wraceatt.num

# 5b: What are the mode, mean, and median for worry?

mean(crime$worry, na.rm = TRUE)
# Mean is 4.775

median(crime$worry, na.rm = TRUE)
# Median is 4

table(crime$worry)
# Mode is 5


# EXERCISE 6:

# 6a: Describe the variable educat3. How could it be modified to be used as 
# continuous variable? 

levels(crime$educat3)
edu.tab <- table(crime$educat3)
prop.table(edu.tab)
# educat3 has five levels. The first four can be releveled as increasing 
# qualification levels and used as a continuous variable. "Other" does not 
# clearly fit into this order. If educat3 was to be used as a continuous
# variable, other should be removed.

# 6b: Using educat3 as both a continuous and factor variable evaluate the
# statement "Worry about being a victim of crime is higher in those with
# lower education levels."

# educat3 as continuous variable
crime <- crime %>%
  mutate(educat3 = fct_relevel(educat3,
                                "None",
                                "O level/GCSE",
                                "Apprenticeship or A/AS level",
                                "Degree or diploma",
                                "Other"),
  )

model.worry.educat <- lm(worry ~ as.numeric(educat3), subset.data.frame(crime, educat3 != "Other"))
summary(model.worry.educat)
# Using educat3 as a continuous variable yields a statistically significant
# negative association between education level and worries about being a victim
# of crime. This supports the hypothesis statement.

# educat3 as factor variable

# When educat3 is used as a factor variable, "Other" does not need to be excluded.

# Model with "Other"
model.worry.educat.factor.wother <- lm(worry ~ educat3, crime)
summary(model.worry.educat.factor.wother)

# Model without "Other"
model.worry.educat.factor.nother <- lm(worry ~ educat3, subset.data.frame(crime, educat3 != "Other"))
summary(model.worry.educat.factor.nother)
# Using educat3 as a factor variable yields statistically significant negative
# coefficients for before "Apprenticeship or A/AS level" and "Degree or diploma"
# The results are consistent across both models. Again, this supports the hypothesis 
# statement that worry of being a victim of crime is higher in those with
# lower education levels. 

# 6c: What are the R^2 values for the two models calculated? Which is higher?
# Does this make that model better than the other?
summary(model.worry.educat)
# R^2 is 0.0205
summary(model.worry.educat.factor.nother)
# R^2 is 0.0238 

# Although the model using educat3 as a factor variable has a higher R^2, 
# both models have relatively small R^2, explaining less than 2.5% of the
# variation in worry. This is an indication that neither model is very strong.


# 6d: Calculate 97.5 confidence intervals for the coefficients for those with
# GCSE and those with Degrees. What does this tell you?
confint(model.worry.educat.factor.nother, "educat3O level/GCSE", level = 0.975)
confint(model.worry.educat.factor.nother, "educat3Degree or diploma", level = 0.975)

# The upper bound of the confidence interval for those with a degree or diploma
# is less than the lower bound of the confidence interval for those who only
# achieved O or GCSE level. Therefore, we can say with 97.5% confidence those with
# degrees worry less about crime than those with only O-levels or GCSEs.


# EXERCISE 7: 

# 7a: Create a model with worry as the dependent variable, age as a
# continuous, and education, sex, and rural status as dummy variables.
model.worry.educat.full <- lm(worry ~ as.numeric(agegrp7) + 
                                educat3 + 
                                sex + 
                                rural2, 
                              subset.data.frame(crime, educat3 != "Other"))

# 7b: Interpret the statistical significance of the intercept and the coefficients.
summary(model.worry.educat.full)
# All variables, other than educat3O level/GCSE, were statistically significant.
# Increasing age is associated with a decrease in worry about crime. Increasing
# education, with the exception of educat3O level/GCSE, is similarly associated
# with a decrease in worry about crime. Men are, on average, less worried
# about crime. Those in urban areas are, on average, more worried about crime.
# The intercept is meaningless in this context, as it represents the hypothetical
# worry of an individual of age 0 (the other variable values are feasible).

# 7c: Interpret the effect sizes of the four variables.

# On average:
# As age increases by one level (not one year, but one age group ~ 9-10 years),
# worry about crime decreases by 0.115.
# Relative to those with no education, Apprenticeship or A/AS level and Degree 
# or diploma, worry 0.324 and 0.938 less about crime, respectively.
# Men worry 1.499 less about crime than women.
# Those living in urban areas worry 1.347 more about crime than those living
# in rural areas.

# 7d: What is the adjusted R^2 for this model? How can you interpret it?
summary(model.worry.educat.full)
# Adjusted R^2 is 0.105, indicating that, after accounting for the
# number of predictors used, the model only explains a relatively small
# portion of the variation in worry. This suggests that other factors 
# beyond those considered might be involved in determining the crime worry level
# of people. 


# EXERCISE 8:

# 8a: Using a bar chart plot the mean worry score across age groups for men and
# women. Describe the data.
ggplot(crime, aes(x = agegrp7, y = worry, fill = sex)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  labs(
    x = "Age Group",
    y = "Mean Worry Score",
    fill = "Gender"
  ) +
  theme_iqmss +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
# Among women, worry about crime appears to decrease as age increases. For men,
# there is less of a clear pattern. Across all age groups, women consistently
# have higher worry about crime than men.

# 8b: Estimate the effect of sex (categorical) and age (continuous) on worry
# scores. Do the independent variables interact?

# Model without interaction
model.worry.age.sex <- lm(worry ~ as.numeric(agegrp7) + sex, crime)
summary(model.worry.age.sex)
# Men, on average, have statistically significantly lower worry about crime
# than women (roughly -1.59 units). Across sexes, increasing age is associated
# with decreasing worry about crime.

# Model with interaction
model.worry.age.sex.interaction <- lm(worry ~ as.numeric(agegrp7) + sex + sex*as.numeric(agegrp7), crime)
summary(model.worry.age.sex.interaction)
# The interaction term between sex and age is statistically significant. Men, still,
# on average, have statistically significantly lower worry about crime than women.
# However, the association between age and perceptions of crime is different
# between men and women. Among women, increasing age is associated with decreasing
# worry about crime. Among men, however, increasing age is associated with slightly
# increasing worry about crime.


# EOF