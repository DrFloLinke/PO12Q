# *****************************************************************
# PO12Q - Quantitative Political Analysis: Uncovering Relationships
# Dr Flo Linke
# Exercise Solutions - Bivariate Regression
# *****************************************************************

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

# *****************************************************************
# GRAPH FORMATTING
# *****************************************************************

# Graph theme
theme_iqmss <- theme_classic() +
  theme(text = element_text(family = "sans"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(margin = margin(b = 10, t=9)),
        axis.title.y = element_text(margin = margin(r = 12)),
        legend.title = element_text(size = 24), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 24),
        axis.ticks.length=unit(.1, "cm")) +
  theme(
    panel.background = element_rect(fill = 'transparent'),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = 'transparent', color = NA),
    legend.box.background = element_rect(fill = 'transparent', color = NA)
  )

# Graph theme (with math notation)
theme_iqmss_math <- theme_classic() +
  theme(text = element_text(family = "sans"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(margin = margin(b = 10, t=9)),
        axis.title.y = element_text(margin = margin(r = 12)),
        legend.title = element_text(size = 24), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 22),
        axis.ticks.length=unit(.1, "cm")) +
  theme(
    panel.background = element_rect(fill = 'transparent'),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = 'transparent', color = NA),
    legend.box.background = element_rect(fill = 'transparent', color = NA)
  )

# *****************************************************************
# EXERCISE SOLUTIONS
# *****************************************************************

london <- read.csv(file.path(RAW, "london_exercises.csv"), header = TRUE, stringsAsFactors = TRUE)

# EXERCISE 1: Examine the London data. What hypothesis could be
# tested using the variables contained in the dataset?

str(london)
head(london)
summary(london)

# Possible answers include: 
# the relationship between crime and house prices
# the relationship between age and unemployment
# the relationship between population density and crime
# the relationship between unemployment and crime
# etc.


# EXERCISE 2: Crime rate is defined as the number of crimes 
# committed per 100,000 people in a given area.

# 2a: Calculate crime rate for each of the wards.

# Crime is the number of crimes committed per 1,000 residents
london$crime.rate <- london$crime * 100

# 2b: Evaluate the variable's distribution using a histogram and
# summary statistics. 

# Histogram
ggplot(london, aes(x = crime.rate)) + 
  geom_histogram(aes(y = after_stat(count)/sum(after_stat(count))), bins = 100) +
  xlab("Crime Rate (Per 100,000 People)") +
  ylab("Proportion") + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_iqmss +
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 10))
# Summary statistics
summary(london$crime.rate)

# 2c: Which wards had the most crimes committed in them? Find the
# names of the wards in which the crime rate was higher than the
# 99th percentile of the variable's distribution.

# Dplyr solution:
london %>% filter(crime.rate > quantile(crime.rate, probs = 0.99)) %>% 
  select(ward.name, crime.rate) %>% 
  arrange(-crime.rate)
# Base solution:
outliers <- london[which(with(london, crime.rate > quantile(crime.rate, probs = 0.99))),
                   colnames(london) %in% c("ward.name", "crime.rate")]
outliers[order(-outliers$crime.rate),]


# EXERCISE 3: London is divided into 32 Boroughs and the City of 
# London. Examine the variation in crime on Borough level.

# 3a: Calculate crime rate for each of the Boroughs
london <- london %>%
  mutate(total.crime = crime * population / 1000)
crime.rate.borough <- london %>%
  group_by(borough) %>%
  summarise(total.crime = sum(total.crime), population = sum(population)) %>%
  mutate(crime.rate = (total.crime / population) * 100000)


# 3b: Present the Borough-level crime rates using a table and a
# bar chart, with Borough order descending by crime rate.

# Table
crime.rate.borough %>% 
  select(borough, crime.rate) %>% 
  arrange(-crime.rate) %>% 
  print(n = 33)

# Bar Chart
crime.rate.borough %>% 
  select(borough, crime.rate) %>%
  ggplot() + 
  geom_bar(aes(x = reorder(borough, crime.rate), y = crime.rate), stat = "identity") +
  ylab("Crime Rate (Per 100,000 People)") + 
  xlab("Borough") + 
  theme_iqmss +
  coord_flip()


# EXERCISE 4: Unemployment rate, defined as the ratio of people in 
# full time employment to population of working age, is often said 
# to be related to crime. 

# 4a: Generate an unemployment rate variable for each of the wards.

# Dplyr solution:
london <- london %>% 
  mutate(unemp.rate = (adults - employed)/adults)
# Base solution:
london$unemp.rate2 <- with(london, (adults - employed)/adults)

# 4b: Examine the distribution of the unemployment rate.
ggplot(london, aes(x = unemp.rate)) + 
  geom_histogram(aes(y = after_stat(count)/sum(after_stat(count))), bins = 60) +
  xlab("Unemployment Rate") + 
  ylab("Proportion") + 
  theme_iqmss

# 4c: Create a scatter plot of the relationship between 
# unemployment and crime. Make sure to choose the right axis for 
# each of the variables and to label the axes correctly. Interpret
# the results.

# Scatter plot
ggplot(london, aes(x = unemp.rate, y = crime.rate)) + 
  geom_point() + 
  xlab("Unemployment Rate") + 
  ylab("Crime Rate (Per 100,000 People)") + 
  theme_iqmss

# The crime rate appears to increase with unemployment rate. There
# are several outliers in the data which might exert significant 
# influence on the OLS regression estimates. 



# EXERCISE 5: Estimate a regression model for the relationship 
# between unemployment rate and crime rate. Interpret the results.

model.crime.unemp <- lm(crime.rate ~ unemp.rate, data = london)
summary(model.crime.unemp)

# 5a: Interpretation:
  
  # i: The intercept is statistically significant. It indicates that 
  # in a hypothetical ward with 0% unemployment, there would be a 
  # crime rate of 2464 per 100,000 people.

  # ii: The slope coefficient is statistically significant. A 1 
  # percentage point increase (a 0.01 increase) in the unemployment 
  # rate is associated with an increase of 176.09 in the crime rate 
  # per 100,000 people.
  
  # iii: The distributions of residuals
  ggplot() + 
    geom_histogram(aes(x = model.crime.unemp$residuals), bins = 100) + 
    xlab("Residual") +
    ylab("Proportion") + 
    theme_iqmss
  # Generally, the residuals are approximately normally distributed
  # around 0. However, there are multiple positive outliers. This 
  # is consistent with the scatter plot from the previous exercise.
  
  # iv: The R^2 of 0.038 indicates that the unemployment rate explains 
  # only 3.8% of the variation in crime rate. This is a poor fit,
  # implying our model is not ideal for the data.

# 5b: Add the regression line to the scatter plot from Exercise 4.
ggplot(london, aes(x = unemp.rate, y = crime.rate)) + 
  geom_point() + 
  geom_smooth(method = lm) +
  xlab("Unemployment Rate") + 
  ylab("Crime Rate (Per 100,000 People)") + 
  theme_iqmss
  
# 5c: Calculate the 95% confidence interval for the coefficient
# and the slope of the model.
confint(model.crime.unemp, level = 0.95)

# 5d: Run the command 
  predict(model.crime.unemp, newdata = data.frame(unemp.rate = 0.4))
# and explain what it does in plain English.
  
  # Using the model we estimated for the relationship between unemployment
  # rate and crime rate, this command predicts what the crime rate would be 
  # in a hypothetical ward where the unemployment rate is 0.4 (i.e. 40%). 
  # It does this by plugging 0.4 into the regression equation as the value 
  # of unemp.rate and returning the corresponding predicted value of crime
  # rate based on the fitted line.


# EXERCISE 6: It can be hypothesised that a ward with higher median 
# household income will have a lower rate of crime compared to a 
# ward with lower median household income.

# 6a: Run a regression model testing this hypothesis.
model.crime.inc <- lm(crime.rate ~ income, data = london)
summary(model.crime.inc)

# 6b: Interpret the coefficients, their significance levels, 
# and the R^2 of the model.

# The intercept is statistically significant. It indicates that in a 
# hypothetical ward with median income of £0, there would be a crime 
# rate of 3939 per 100,000 people. However, this is meaningless, as a 
# ward with median income of £0 is nonsensical. The slope coefficient 
# is also statistically significant. A £1 increase in median household 
# income is associated with an increase of 0.113 in the crime rate per 
# 100,000 people. The R^2 of 0.012 indicates that median household 
# income explains only 1.2% of the variation in crime rate. This is a 
# poor fit.

# 6c: Produce a scatter plot and add the regression line.
ggplot(london, aes(x = income, y = crime.rate)) +
  geom_point() +
  geom_smooth(method = lm) + 
  xlab("Median Household Income") + 
  ylab("Crime Rate (Per 100,000 People)") +
  theme_iqmss
  
# 6d: What conclusions can you draw from the model and plot with respect
# to the hypothesis?

# The model shows a statistically significant positive association
# between crime rates and median household income in areas. This
# is the opposite of what was predicted by the hypothesis. However,
# we can see from the scatterplot that there are multiple outliers
# likely skewing this result. Furthermore, the low model R^2 indicates
# that our model does not explain well the variation in crime rates.
# Thus, while this model has results that contradict our conclusion,
# the model best demonstrates that a simple bivariate relationship
# between crime rates and median household income is not effective.
# To further test our hypothesis, we would need to consider other variables,
# discussed more in Week 8.


# EXERCISE 7: The mean age of a ward can be said to influence the rate 
# of crime in that ward.

# 7a: Come up with a hypothesis for the relationship between the 
# mean age of a ward and the rate of crime in that ward. Briefly 
# justify why you chose this hypothesis.

# One potential hypothesis is that as the mean age of a ward increases,
# the rate of crime will decrease. This is because older people
# are less physically capable of committing crimes and more financially
# stable, therefore, having less need for committing crimes.

# A second possible hypothesis is that as the mean age of a ward increases,
# the rate of crime will increase. This is because areas with lower
# mean ages have more families with young children, and families
# have lower propensity to commit crimes.

# 7b: Run a regression model testing this hypothesis and interpret
# the results of the model.
model.crime.age <- lm(crime.rate ~ age, data = london)
summary(model.crime.age)

# Both the intercept and slope coefficients are statistically significant.
# The intercept indicates that in a hypothetical ward with an average age
# of 0, there would be 20126.44 crimes per 100,000 people. This is, of course,
# meaningless as this area could not exist. The slope coefficient indicates
# that a 1 year increase in average age is associated with a decline of 
# 328.70 in the crime rate per 100,000 people. This supports the hypothesis 
# that an older ward will have a lower crime rate (although whether the 
# hypothesis is rejected or not depends upon the hypothesis you formulated). 
# However, the R^2 is very poor, only able to explain 1.8% of the crime 
# rate variation.


# EXERCISE 8: It can be hypothesised that a ward with a larger 
# number of benefit recipients will have a higher rate of crime
# compared to a ward with fewer benefit recipients.

# 8a: Explain some reasons why this might be the case.
# Wards with larger numbers of people receiving benefits are often
# poorer wards, indicating a greater financial incentive to commit crime.
# Wards with larger numbers of people receiving benefits are similarly
# often wards with higher unemployment, which is likely to create
# life instability, also potentially leading to higher crime rates.
# Furthermore, wards with larger numbers of people receiving benefits
# are more likely to live in council housing, with associated social
# problems that may lead to higher crime rates.

# 8b: Run a regression model testing this hypothesis and interpret 
# the results of the model. What does this interpretation suggest 
# about your hypothesis?
model.crime.benefits <- lm(crime.rate ~ benefits, data = london)
summary(model.crime.benefits)
# Both the intercept and slope coefficients are statistically significant.
# The intercept indicates that in a hypothetical ward with 0% of the
# population receiving work-related benefits, there would be 6867.30
# crimes per 100,000 people. The slope coefficient indicates that a 1
# percentage point increase in the share of the population receiving 
# benefits is associated with an increase of 116.90 in the crime rate 
# per 100,000 people. This provides evidence that supports our hypothesis. 
# However, the R^2 is very poor, being able to explain less than 1% 
# of the variation in the crime rate.

# 8c: Produce a scatter plot and add the regression line.
ggplot(london, aes(x = benefits, y = crime.rate)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab("Percent of Ward Receiving Benefits") + 
  ylab("Crime Rate (Per 100,000 People)") +
  theme_iqmss

# 8d: Compare the regression model with the models from Exercises 6 and 7.
# Which is better able to explain the crime rate of a ward and why?
(summary(model.crime.inc))$r.squared
(summary(model.crime.age))$r.squared
(summary(model.crime.benefits))$r.squared
# The age model has the highest R^2 (0.018) compared to the income (0.012)
# and benefits (0.006) models, meaning it is better able to explain the
# crime rate of London wards, although it is still incredibly low.


# EXERCISE 9: London Boroughs are classified as either "Inner" or 
# "Outer" boroughs.

# 9a: Recreate the scatter plot from Exercise 4c with different colours
# for the inner and outer boroughs subsetting the data to wards with a
# crime rate below 500.
ggplot(subset(london, crime < 500),
       aes(x = unemp.rate, y = crime, color = inner)) + 
  geom_point() + 
  xlab("Unemployment Rate") + 
  ylab("Crime Rate") + 
  scale_color_discrete(name = NULL, labels = c("Outer London", "Inner London")) + 
  theme_iqmss

# 9b: Estimate a regression model for the relationship between the
# unemployment rate and crime rate in inner and outer boroughs

# Inner boroughs
model.crime.unemp.inner <- lm(crime ~ unemp.rate, 
                              data = subset(london, inner == TRUE & 
                                              crime < 500))
summary(model.crime.unemp.inner)
# Outer boroughs
model.crime.unemp.outer <- lm(crime ~ unemp.rate, 
                              data = subset(london, inner == FALSE &
                                              crime < 500))
summary(model.crime.unemp.outer)

  # i: Compare the coefficients for the two models, how can you
  # interpret these?
  # The coefficient is larger in the model of the outer boroughs,
  # this implies that a rise in the unemployment rate is associated 
  # with a greater rise in crime rate in outer rather than inner 
  # boroughs. 
  
  # ii: Compare and interpret the R^2 statistics. Does unemployment
  # better explain crime in inner or outer boroughs?
  # The higher R-squared statistic in the outer boroughs implies 
  # that unemployment has a greater link to crime rate in the outer 
  # boroughs than the inner boroughs.
  
  # iii: Using the models estimated predict what the crime rate
  # would be in an inner and outer borough given a ward with an
  # unemployment rate of 0.45.
  predict(model.crime.unemp.inner, newdata = data.frame(unemp.rate = 0.45))
  predict(model.crime.unemp.outer, newdata = data.frame(unemp.rate = 0.45))
  # You would expect crime to be lower in an outer borough with an 
  # unemployment rate of 0.45


# EOF