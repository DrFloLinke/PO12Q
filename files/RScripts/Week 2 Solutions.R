# *****************************************************************
# PO12Q: Quantitative Political Analysis: Uncovering Relationships
# Dr Flo Linke
# WEEK 2 - EXERCISES SOLUTIONS
# *****************************************************************

# *****************************************************************
# USER SETTINGS
# *****************************************************************

### FOR THIS SCRIPT TO WORK PROPERLY, FOLLOW THESE STEPS:
# 1. Create the folder structure shown below on your computer.
# 2. Save the required data files inside the "raw" folder.
# 3. Copy and paste your project folder path into the ROOT object below.

### FOLDER STRUCTURE
# project_folder/
# ├── data/
#     ├── raw/
#         ├── anes.csv

# Copy and paste your project folder path here
ROOT <- ""

# Do not change the code below. These lines automatically link your subfolders
# to the ROOT path you set above.
DATA <- file.path(ROOT, "data")
RAW <- file.path(DATA, "raw")

# *****************************************************************
# SETUP AND PACKAGES
# *****************************************************************

library(car)
library(tidyverse)
library(pwr)

# *****************************************************************
# GRAPH FORMATTING
# *****************************************************************

# Graph theme
theme_iqmss <- function(base_size = 12, title_size = base_size + 2) {
  theme_classic() +
    theme(
      text = element_text(family = "sans"),
      axis.text = element_text(size = base_size),
      axis.title = element_text(size = title_size),
      axis.text.x = element_text(margin = margin(b = 10, t = 9)),
      axis.title.y = element_text(margin = margin(r = 12)),
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size),
      plot.title = element_text(size = title_size),
      axis.ticks.length = unit(.1, "cm"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    )
}

# Graph theme (with math notation)
theme_iqmss_math <- function(base_size = 12, title_size = base_size + 2) {
  theme_classic() +
    theme(
      text = element_text(family = "sans"),
      axis.text = element_text(size = base_size),
      axis.title = element_text(size = title_size),
      axis.text.x = element_text(margin = margin(b = 10, t = 9)),
      axis.title.y = element_text(margin = margin(r = 12)),
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size),
      plot.title = element_text(size = title_size),
      axis.ticks.length = unit(.1, "cm"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    )
}

# *****************************************************************
# WEEK 2 EXERCISE SOLUTIONS
# *****************************************************************

# SETUP: Set your working directory to the location of the anes csv file
# and load the csv file into a data frame called anes in R. Complete this step
# before attempting any of the exercises below.
anes <- read.csv(file.path(RAW, "anes.csv"))

anes$income <- replace(anes$income, anes$income == 99, NA)
anes$inc_fac <- factor(anes$income, ordered = TRUE)
anes <- anes %>%
  mutate(
    income_fac = recode_values(
      inc_fac,
      "1"  ~ 2500,
      "2"  ~ 7499.5,
      "3"  ~ 12499.5,
      "4"  ~ 17499.5,
      "5"  ~ 22499.5,
      "6"  ~ 27499.5,
      "7"  ~ 32499.5,
      "8"  ~ 37499.5,
      "9"  ~ 42499.5,
      "10" ~ 47499.5,
      "11" ~ 52499.5,
      "12" ~ 57499.5,
      "13" ~ 62499.5,
      "14" ~ 67499.5,
      "15" ~ 72499.5,
      "16" ~ 77499.5,
      "17" ~ 82499.5,
      "18" ~ 87499.5,
      "19" ~ 92499.5,
      "20" ~ 97499.5,
      "21" ~ 112499.5,
      "22" ~ 137499.5,
      "23" ~ 162499.5,
      "24" ~ 187499.5,
      "25" ~ 224999.5,
      "26" ~ 500000
    )
  )
anes$inc <- as.numeric(as.character(anes$income_fac))




# EXERCISE 1: Social factors, like sex and education, are often used to explain
# the variation in income across different social groups.

# 1a: Conduct a correlation test on the variables sex and inc using the appropriate
# correlation coefficient. What is the relationship between these two variables
# and is this relationship statistically significant at the 95% confidence level?

# Point-Biserial correlation (one binary and one continuous variable), so, use
# method = "Pearson".
cor.test(anes$sex, anes$inc, method = "pearson")
# The test indicates a statistically significant weak negative correlation (-0.162125)

# 1b: Convert the educ variable into an ordered factor variable called educ_fac
anes$educ_fac <- factor(anes$educ, ordered = TRUE)

# 1c: Conduct a correlation test on the variables educ_fac and inc_fac using the
# appropriate correlation coefficient. What is the relationship between these
# two variables and is this relationship statistically significant at the 95%
# confidence level?

# Both variables are ordinal, so, use method = "spearman." cor.test() requires
# numeric values as input, so, first code variables as numeric
cor.test(as.numeric(anes$educ_fac), as.numeric(anes$inc_fac), method = "spearman")
# The test indicates a statistically significant moderate positive correlation (0.4740324)
# You can safely ignore the warning in a large survey such as ANES

# 1d: Convert the educ variable into a binary factor variable called educ_bin,
# with 0 representing no degree and 1 representing a degree, using 4 as the
# cut-off point.
anes <- anes %>%
  mutate(educ_bin = cut(educ,
                        breaks = c(1, 4, 8),
                        labels = c(0, 1),
                        include.lowest = TRUE))


# 1e: Conduct a correlation test on the variables sex and educ_bin using the
# appropriate correlation coefficient. What is the relationship between these
# two variables and is this relationship statistically significant at the 95%
# confidence level?

# Phi correlation (two binary variables), so, use method = "pearson". cor.test()
# requires numeric values as input, so, first code educ_bin as numeric.
cor.test(anes$sex, as.numeric(anes$educ_bin), method = "pearson")
# The test indicates a statistically significant weak negative correlation (-0.1672546)


# EXERCISE 2: Investigate the relationship between a respondent's age (age) and their
# income (inc).

# 2a: Pearson correlation test.
age_inc_pearson <- cor.test(anes$age, anes$inc, method = "pearson")
age_inc_pearson
# Read the estimate (direction and strength) and the p-value (significance at 95%).

# 2b: Scatter plot and Spearman comparison.
ggplot(anes, aes(x = age, y = inc)) +
  geom_point() +
  labs(x = "Age", y = "Income ($)") +
  theme_iqmss()
cor.test(anes$age, anes$inc, method = "spearman")
# Income is highly skewed with extreme outliers, so Pearson's (which relies on means
# and standard deviations) can be distorted by them. Spearman's, being rank-based, is
# robust to outliers and makes no normality assumption, so it is arguably the more
# appropriate coefficient here. Compare the two estimates.

# 2c: Power of the Pearson test.
n_ai <- sum(complete.cases(anes$age, anes$inc))
pwr.r.test(n = n_ai,
           r = abs(age_inc_pearson$estimate),
           sig.level = 0.05,
           alternative = "two.sided")
# With several thousand complete observations, power is very high even for a weak
# correlation, but still below the 80% threshold.

# 2d: Observations needed to detect a medium correlation (r = 0.3) at 80% power.
pwr.r.test(r = 0.3, sig.level = 0.05, power = 0.80, alternative = "two.sided")
# About 85 observations are required (n = 84.07).


# EXERCISE 3: Investigate how support for Biden and non-voting participation relate
# to sex.

# 3a: Point-biserial correlation (one binary, one continuous variable), so use
# method = "pearson".
sex_biden <- cor.test(anes$sex, anes$ftbiden1, method = "pearson")
sex_biden
# Read the estimate (direction and strength) and the p-value (significance at 95%).
# The result is insignificant, so we cannot reject the null hypothesis of no correlation

# 3b: Power of the test (treating the coefficient as the effect size r).
n_sb <- sum(complete.cases(anes$sex, anes$ftbiden1))
pwr.r.test(n = n_sb,
           r = abs(sex_biden$estimate),
           sig.level = 0.05,
           alternative = "two.sided")
# Power is very small, giving us only a 5.6% chance of correctly rejecting 
# the null hypothesis if there is a true correlation of this size in the population.

# 3c: Phi correlation (two binary variables), so use method = "pearson". cor.test()
# requires numeric values as input, so first code participation as numeric.
anes$participation <- factor(anes$particip_none, labels = c("yes", "no"))
cor.test(anes$sex, as.numeric(anes$participation), method = "pearson")
# This is highly statistically significant (very small p-value) but the correlation 
# is very weak (0.1024211), so, while there is a relationship between sex and non-voting 
# participation, it is not a strong one.

