# *****************************************************************
# PO12Q - Quantitative Political Analysis: Uncovering Relationships
# Dr Flo Linke
# Exercise Solutions - Testing the CLA
# *****************************************************************

# For this script to work properly, follow the these steps:
# 1. Create a folder as your working directory
# 2. Set the working directory to this folder, and copy/paste the the 
#    file path into the ROOT object in the user settings below
# 3. Within that folder, create a subfolder called "data" 
# 4. Within the "data" folder, create a subfolder called "raw"
# 5. Download the "london_exercises_12.csv" file from the companion and
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

library(tidyverse)
library(lmtest)
library(car)
library(sandwich)
library(lawstat)
library(forecast)
library(DescTools)
library(corrtable)

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


# EXERCISE 1: Variable Creation

# 1a: Calculate an unemployment rate variable for each ward.

london <- london %>%
  mutate(unemployment = 100 - (employed / adults) * 100)

# 1b: Calculate a population density variable for each ward.

london <- london %>%
  mutate(density = ((children + adults + elderly) / area))


# EXERCISE 2: Estimate a bivariate regression model for the 
# relationship between education and unemployment. Conduct 
# diagnostics on the model.

# 2a: Estimate the model.

summary(model_unemployment <- lm(unemployment ~ education, data = london))

# 2b: Test for heteroscedasticity.

# i: Produce a scatter plot with the regression line and examine
# the spread of residuals visually.

# Base R
plot(london$education, london$unemployment)
abline(model_unemployment)

# ggplot
ggplot(london, aes(education, unemployment)) +
  geom_point(size=0.9) +
  stat_smooth(method = lm, colour="#e57726") +
  theme_iqmss
# A funnel is discernible here, narrowing as the values of 
# education increase.

# ii: Conduct the Breusch-Pagan test.

bptest(model_unemployment, studentize = TRUE)
# The test is significant, and we reject the null hypothesis of 
# homoscedasticity. We have violated the assumption.


# 2c: Test for correct functional form.
# i: Conduct the RESET test.

resettest(model_unemployment)
# The test is significant, and so we have misspecified the model.

# ii: Estimate a polynomial model.

summary(model_unemployment_poly <- lm(unemployment ~ 
                                        poly(education, 2), data = london))
summary(model_unemployment)
# The polynomial (second order) is significant, and so the 
# relationship between education and unemployment follows a 
# curvilinear relationship, reaching a maximum in the centre 
# range of education.

# iii: Plot the linear and polynomial fits.

ggplot(london, aes(education, unemployment)) + 
  geom_point(size=0.9) +
  stat_smooth(method = lm, formula = y ~ x, colour = "#8a1e00") +
  stat_smooth(method = lm, formula = y ~ poly(x, 2), colour = "#e57726") +
  theme_iqmss
# This confirms the reasoning in 2c.ii.

# iv: Re-run the RESET test on the polynomial model.

resettest(model_unemployment_poly)
# The RESET test is insignificant, we have correctly specified 
# the model.


# EXERCISE 3: Estimate a multivariate regression model for the 
# relationship between crime, density, unemployment, and inner 
# borough status. Conduct diagnostics on the model.

# 3a: Estimate the model.

summary(model_crime <- lm(crime ~ 
                            density + unemployment + inner, data = london))

# 3b: Test for correct functional form.

# Conduct the RESET test.

resettest(model_crime)
# The RESET test is insignificant, we have correctly specified 
# the regression function.

# 3c: Test for multicollinearity.

# i: Examine the correlation matrix.

correlation_matrix(london[c('density', 'unemployment', 'inner')],
                   digits = 2,
                   use = "lower",
                   show_significance = TRUE)
# "inner" and "density" are moderately positively correlated. 
# This makes sense substantively, and leads to collinearity here. 
# There is no perfect collinearity however, and so the assumption 
# is not violated.

# ii & iii: Calculate the Variance Inflation Factors.

vif(model_crime)
# There is no multicollinearity to worry about in this model.



# EXERCISE 4: Estimate a multivariate regression model for the
# relationship between turnout, education, benefits, migration, 
# and inner borough status. Conduct diagnostics on the model.

# 4a: Estimate the model.

summary(model_turnout <- lm(turnout ~
                              education + benefits + 
                              migration + inner, data = london))


# 4b: Test for heteroscedasticity.

# i: Is this assumption likely to be violated?

# Yes, as this assumption is most frequently violated in this 
# type of data.

# ii: Conduct the Breusch-Pagan test.

bptest(model_turnout)
# The test is statistically significant, and so we reject the 
# null hypothesis of homoscedasticity. We have violated the 
# assumption.

# iii: Estimate heteroscedasticity-consistent standard errors.

coeftest(model_turnout, vcov = vcovHC(model_turnout, type = "HC3"))
# The standard errors change, but this does not change our 
# acceptance of significance for individual coefficients.

#
# EOF
#