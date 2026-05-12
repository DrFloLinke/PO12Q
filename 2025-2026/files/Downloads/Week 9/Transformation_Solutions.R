# *****************************************************************
# PO12Q - Quantitative Political Analysis: Uncovering Relationships
# Dr Flo Linke
# Week 9, Exercise Solutions - Transforming Variables
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


# EXERCISE 1: This Section uses the london\_exercises data set. This 
# is the same data set we used in the section on bivariate regression. 
# 1. The first exercise (Exercise (a)) will cover some data preparation 
# done in the bivariate section. If you have already done that section, 
# feel free to skip it and start with question b) at the end of your script 
# from the bivariate exercises.

london <- read.csv(file.path(RAW, "london_exercises.csv"), header = TRUE, stringsAsFactors = TRUE)

# 9a: Unemployment rate, defined as the ratio of people in full time unemployment
# to population of working age is often said to be related to crime. Generate
# an unemployment rate variable for each of the wards.

london <- london %>% 
  mutate(unemp_rate = (adults - employed) / adults)

# 9b: It is theorised that unemployment is a driving factor behind crime rates.

# 9bi: Plot a scatter graph with unemployment rate, crime rate, and the regression
# line that may be used to evaluate the theory. Describe the plot and the best fit
# line.
ggplot(london, aes(x = unemp_rate, y = crime)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab("Unemployment Rate") + 
  ylab("Crime Rate") + 
  theme_iqmss
# There appears to be a positive association between crime rates and unemployment
# rates. The scatter plot also seems to show three main outliers with very high
# crime rates but more typical unemployment rates.

# 9bii: Plot the graph again excluding wards with a crime rate greater than 500.
# Describe the plot and the best fit line.
ggplot(subset(london, crime < 500), aes(x = unemp_rate, y = crime)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab("Unemployment Rate") + 
  ylab("Crime Rate") + 
  theme_iqmss
# Removing the outliers does not significantly impact the trend. However,
# the updated scatter plot highlights the significant variation in crime rates.
# There remain multiple high crime wards far from the line of best fit.

# 9biii: Plot another graph excluding wards with a crime rate of over 500
# with a crime rate log transformed. Describe the plot and the best fit line.
ggplot(subset(london, crime < 500), aes(x = unemp_rate, y = log(crime))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  xlab("Unemployment Rate") + 
  ylab("Logged Crime Rate") +
  theme_iqmss
# There is still a clear positive association between unemployment rate and
# the log of the crime rate. However, there are far fewer outliers and the 
# line of best fit appears to much better fit the data.

# 9biv: Build both models. Interpret both including the effect size. Which model
# fits the data better?

model_crime_unemp <- lm(crime ~ unemp_rate, subset(london, crime < 500))
summary(model_crime_unemp)

# Unemployment has a statistically significant positive association with crime.
# On average, an increase in the unemployment rate of 0.01 (a 1% increase in the
# unemployment rate) is associated with a 1.7631 increase in the crime rate per
# 1000 people. In a hypothetical ward with an unemployment rate of 0, the model 
# estimates a crime rate of 20.36 crimes per 1000 people.

model_crime_unemp_log <- lm(log(crime) ~ unemp_rate, subset(london, crime < 500))
summary(model_crime_unemp_log)
# Unemployment has a statistically significant positive association with the log
# of crime. On average, an increase in the unemployment rate of 0.01 is associated
# with a 2.24% increase in the crime rate. In a hypothetical ward with an
# unemployment rate of 0, the model estimates a log crime rate of 3.526 (26.13
# crimes per 1000 people). The R^2 is higher in the logarithmized model, indicating
# that it better fits the data.


# EOF