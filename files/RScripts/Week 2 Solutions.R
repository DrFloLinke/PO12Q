# *****************************************************************
# PO12Q: Quantitative Political Analysis: Uncovering Relationships
# Dr Flo Linke
# WEEK 2 - EXERCISES SOLUTIONS
# *****************************************************************

# *****************************************************************
# SETUP AND PACKAGES
# *****************************************************************

library(tidyverse)
library(modelsummary)
library(tinytable)
library(corrtable)
library(pwr)
# Install the vdem package:
# pak::pak("vdeminstitute/vdemdata")
library(vdemdata)

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
# DATA LOADING AND PREPARATION
# *****************************************************************


vdem_raw <- vdemdata::vdem

vdem <- filter(vdem_raw, year==2020)

vdem <- select(vdem,
               country_name,
               v2x_cspart,
               v2x_libdem,
               v2x_polyarchy,
               v2x_regime,
               v2xcl_rol)

# Descriptives
datasummary(All(vdem) ~ N + Mean + SD + Min + P25 + Median + P75 + Max,
            data = vdem)

## ------------------------------------------------------------------------
## 1. Electoral Democracy Index and Liberal Democracy Index

# 1a. Hypotheses
# H0: rho = 0. There is no linear relationship between the Electoral
#     Democracy Index and the Liberal Democracy Index in the population.
# H1: rho != 0. There is a linear relationship between the two indices
#     in the population.


# 1b. Scatter plot

ggplot(vdem, aes(x = v2x_polyarchy, y = v2x_libdem)) +
  geom_point() +
  geom_smooth(method = 'lm',se = F,colour = '#e57726', linewidth=1.5)+
  labs(x="Electoral Democracy Index", y="Liberal Democracy Index") +
  theme_iqmss()


# 1c. Pearson's correlation coefficient and significance test

correlation_matrix(
  vdem[,c("v2x_polyarchy","v2x_libdem")],
  type = "pearson",
  digits = 2,
  decimal.mark = ".",
  use = "lower",
  show_significance = TRUE,
  replace_diagonal = FALSE,
  replacement = ""
)

q1_complete <- complete.cases(vdem[,c("v2x_polyarchy","v2x_libdem")])

cor.test(
  vdem$v2x_polyarchy[q1_complete],
  vdem$v2x_libdem[q1_complete],
  method = "pearson",
  alternative = "two.sided"
)


# 1d. Interpretation and assessment of the hypotheses

# The correlation coefficient is highly statistically significant (p<0.001).
# Its value is very close to one (0.9787249) and therefore indicates a strong,
# positive relationship between the two indices. 
# This was indicated by the scatter plot in part b) where the observations align
# closely to a straight line. The null hypothesis of no linear relationship can be rejected.




# 1e. Post-hoc statistical power

pwr.r.test(
  n = 179,
  r = 0.9787249,
  sig.level = 0.05,
  alternative = "two.sided"
)

# As the effect size is large, and the test in part d) was highly significant, 
# the post-hoc power is very high (1.000). This means that the probability of 
# correctly rejecting the null hypothesis of no linear relationship is almost 
# certain given the sample size and effect size.
# However, post-hoc estimations such as this, do not provide us with new information,
# as lambda is equal to t_obs in such a scenario and thus power just re-expresses
# the information of the p-value.


# 1ef. Prospective power


pwr.r.test(
  r = 0.30,
  power = 0.80,
  sig.level = 0.05,
  alternative = "two.sided"
)

# To detect a correlation of r=0.3 with a power of 0.8 at a 0.05 significance level, 
# a sample size of 84 is required. This means that if we were to conduct a study with 
# 84 observations, we would have an 80% chance of correctly rejecting the null hypothesis 
# of no linear relationship if the true correlation in the population is 0.3.


## ------------------------------------------------------------------------
## 2. Liberal Democracy Index and Rule of Law Index

# 2a. Hypotheses
# H0: rho = 0. There is no linear relationship between the Liberal Democracy
#     Index and the Rule of Law Index in the population.
# H1: rho != 0. There is a linear relationship between the two indices in the
#     population.


# 2b. Scatter plot

ggplot(vdem, aes(x = v2x_libdem, y = v2xcl_rol)) +
  geom_point() +
  geom_smooth(method = 'lm',se = F,colour = '#e57726', linewidth=1.5) +
  labs(x="Liberal Democracy Index", y="Rule of Law Index") +
  theme_iqmss()


# 2c. Pearson's correlation coefficient and significance test

correlation_matrix(
  vdem[,c("v2x_libdem","v2xcl_rol")],
  type = "pearson",
  digits = 2,
  decimal.mark = ".",
  use = "lower",
  show_significance = TRUE,
  replace_diagonal = FALSE,
  replacement = ""
)

q2_complete <- complete.cases(vdem[,c("v2x_libdem","v2xcl_rol")])

cor.test(
  vdem$v2x_libdem[q2_complete],
  vdem$v2xcl_rol[q2_complete],
  method = "pearson",
  alternative = "two.sided"
)

# The correlation coefficient is highly statistically significant (p<0.001).
# Its value is larger than 0.5 (0.9787249) and therefore indicates a strong,
# positive relationship between the two indices. 


# 2d. A more suitable coefficient

# Pearson's correlation measures linear association. The scatter plot suggests
# a curved and monotonic relationship. Spearman's correlation is more suitable
# because it measures monotonic association using ranks and does not require
# the relationship to be linear.

correlation_matrix(
  vdem[,c("v2x_libdem","v2xcl_rol")],
  type = "spearman",
  digits = 2,
  decimal.mark = ".",
  use = "lower",
  show_significance = TRUE,
  replace_diagonal = FALSE,
  replacement = ""
)

cor.test(
  vdem$v2x_libdem[q2_complete],
  vdem$v2xcl_rol[q2_complete],
  method = "spearman",
  alternative = "two.sided",
  exact = FALSE
)


# 2e. Interpretation and assessment of the hypotheses

# The correlation coefficient is highly statistically significant (p<0.001).
# Its value is very close to one (0.9348255) and therefore indicates a strong,
# positive relationship between the two indices. 
# The significant Pearson coefficient from part c) already provided evidence 
# against the null hypothesis, but Spearman provides a more appropriate assessment
# of this relationship.



## ------------------------------------------------------------------------
## 3. Civil Society Participation and Liberal Democracy

# 3a. Scatter plot

ggplot(vdem, aes(x = v2x_libdem, y = v2x_cspart)) +
  geom_point() +
  # geom_smooth(method = 'lm',se = F,colour = '#e57726', linewidth=1.5)+
  labs(x="Liberal Democracy Index", y="Civil Society Participation Index") +
  theme_iqmss()


# Add a LOESS line to examine the form of the relationship.

ggplot(vdem, aes(x = v2x_libdem, y = v2x_cspart)) +
  geom_point() +
  geom_smooth(method = 'loess',se = F,colour = '#e57726', linewidth=1.5) +
  labs(x="Liberal Democracy Index", y="Civil Society Participation Index") +
  theme_iqmss()

# The LOESS line indicates a positive, monotonic relationship between the two indices, 
# but it is not linear. The relationship is stronger at lower values of the Liberal Democracy Index



# 3b. Pearson's coefficient

correlation_matrix(
  vdem[,c("v2x_libdem","v2x_cspart")],
  type = "pearson",
  digits = 2,
  decimal.mark = ".",
  use = "lower",
  show_significance = TRUE,
  replace_diagonal = FALSE,
  replacement = ""
)

q3_complete <- complete.cases(vdem[,c("v2x_libdem","v2x_cspart")])

cor.test(
  vdem$v2x_libdem[q3_complete],
  vdem$v2x_cspart[q3_complete],
  method = "pearson",
  alternative = "two.sided"
)

# Pearson's correlation is not suitable here, as the relationship is not linear.
# Instead, it is positive and monotonic. Spearman's correlation is more suitable
# here because it uses the ranks of individual observations.


# 3c. Spearman's correlation coefficient and significance test

correlation_matrix(
  vdem[,c("v2x_libdem","v2x_cspart")],
  type = "spearman",
  digits = 2,
  decimal.mark = ".",
  use = "lower",
  show_significance = TRUE,
  replace_diagonal = FALSE,
  replacement = ""
)

cor.test(
  vdem$v2x_libdem[q3_complete],
  vdem$v2x_cspart[q3_complete],
  method = "spearman",
  alternative = "two.sided",
  exact = FALSE
)


# 3d. Comparison with Pearson's correlation

# Whilst the Pearson correlation came in at 0.7905285, Spearman's correlation
# is higher at 0.8403842. This difference reflects the non-linearity of the 
# relationship, as Spearman's correlation is based on ranks and is therefore 
# less affected by the curvature of the relationship. The positive sign of both 
# coefficients indicates that higher values of the Liberal Democracy Index are 
# generally associated with higher values of the Civil Society Participation.



## ------------------------------------------------------------------------
## 4. Spearman's correlation with two ordinal variables

# 4a. Recode the Rule of Law Index into four ordered categories.

vdem <- vdem %>%
  mutate(
    rule_law = cut(
      v2xcl_rol,
      breaks = c(0, 0.25, 0.5, 0.75, 1.1),
      labels = FALSE,
      right = FALSE,
      include.lowest = TRUE
    ),
    rule_law_factor = factor(
      rule_law,
      levels = 1:4,
      labels = c("Low", "Medium-low", "Medium-high", "High"),
      ordered = TRUE
    )
  )

table(vdem$rule_law_factor, useNA = "ifany")


# 4b. Apply the same procedure to the Civil Society Participation Index.

vdem <- vdem %>%
  mutate(
    civil_society = cut(
      v2x_cspart,
      breaks = c(0, 0.25, 0.5, 0.75, 1.1),
      labels = FALSE,
      right = FALSE,
      include.lowest = TRUE
    ),
    civil_society_factor = factor(
      civil_society,
      levels = 1:4,
      labels = c("Low", "Medium-low", "Medium-high", "High"),
      ordered = TRUE
    )
  )

table(vdem$civil_society_factor, useNA = "ifany")


# 4c. Assess the assumption of a monotonic relationship.

q4_table <- table(
  vdem$rule_law_factor,
  vdem$civil_society_factor,
  useNA = "no"
)

q4_table

round(prop.table(q4_table, margin = 1), 3)

# The jittered plot displays the ordinal categories while reducing overplotting.

ggplot(vdem, aes(x = rule_law, y = civil_society)) +
  geom_jitter(width = 0.12, height = 0.12, alpha = 0.5) +
  geom_smooth(method = 'lm',se = F,colour = '#e57726', linewidth=1.5) +
  scale_x_continuous(breaks = 1:4) +
  scale_y_continuous(breaks = 1:4) +
  labs(x="Rule of Law Category", y="Civil Society Participation Category") +
  theme_iqmss()

# Both the tables and the jitter graph provide us with strong evidence of a positive, 
# monotonic relationship: as one variable moves from Low to High, the distribution 
# of the other variable also consistently shifts toward higher categories. 
# This is also apparent in the figure: observations are concentrated in dense
# clusters along the diagonal, while clusters further away from the diagonal are
# smaller and less dense. 


# 4d. Spearman's correlation coefficient and significance test

correlation_matrix(
  vdem[,c("rule_law","civil_society")],
  type = "spearman",
  digits = 2,
  decimal.mark = ".",
  use = "lower",
  show_significance = TRUE,
  replace_diagonal = FALSE,
  replacement = ""
)

q4_complete <- complete.cases(vdem[,c("rule_law","civil_society")])

cor.test(
  vdem$rule_law[q4_complete],
  vdem$civil_society[q4_complete],
  method = "spearman",
  alternative = "two.sided",
  exact = FALSE
)

# The correlation coefficient is highly statistically significant (p<0.001).
# Its value is > 0.5 (0.698245) which according to Cohen's conventions indicates
# a strong, positive relationship between the two indices. The positive sign 
# indicates that both ordinal indexes increase together, confirming the assessment 
# in part c).


## ------------------------------------------------------------------------
## 5. Point-biserial correlation

# V-Dem's Regimes of the World variable is coded as:
# 0 = closed autocracy
# 1 = electoral autocracy
# 2 = electoral democracy
# 3 = liberal democracy

# 5a. Recode it at the boundary between electoral autocracy and electoral
# democracy: 0 = autocracy and 1 = democracy.

vdem <- vdem %>%
  mutate(regime = recode(v2x_regime,
                         '0' = 0,
                         '1' = 0,
                         '2' = 1,
                         '3' = 1))

vdem <- vdem %>%
  mutate(regime_factor = factor(
    regime,
    levels = c(0, 1),
    labels = c("Autocracy", "Democracy")
  ))

# table the new variable against the original to see the distribution
table(vdem$v2x_regime, vdem$regime_factor, useNA = "ifany")


# 5b. Point-biserial correlation

correlation_matrix(
  vdem[,c("regime","v2xcl_rol")],
  type = "pearson",
  digits = 2,
  decimal.mark = ".",
  use = "lower",
  show_significance = TRUE,
  replace_diagonal = FALSE,
  replacement = ""
)

q5_pb_complete <- complete.cases(vdem[,c("regime","v2xcl_rol")])

cor.test(
  vdem$regime[q5_pb_complete],
  vdem$v2xcl_rol[q5_pb_complete],
  method = "pearson",
  alternative = "two.sided"
)

# Group means help interpret the sign of the point-biserial coefficient.

vdem %>%
  group_by(regime_factor) %>%
  summarise(
    n = sum(!is.na(v2xcl_rol)),
    mean_rule_law = mean(v2xcl_rol, na.rm = TRUE),
    sd_rule_law = sd(v2xcl_rol, na.rm = TRUE)
  )

# The correlation coefficient is highly statistically significant (p<0.001).
# Its value is > 0.5 (0.7420089) which according to Cohen's conventions indicates
# a strong, positive relationship between the two indices.
# Because autocracy is coded 0 and democracy is coded 1, a positive coefficient 
# means democracies have a higher average Rule of Law Index than autocracies. 
# The group means confirm this: the mean Rule of Law Index for autocracies is 0.458, 
# while for democracies it is 0.859. 


# 5c. Spearman's correlation between the original ordinal regime variable and
# the four-category rule-of-law variable.

correlation_matrix(
  vdem[,c("v2x_regime","rule_law")],
  type = "spearman",
  digits = 2,
  decimal.mark = ".",
  use = "lower",
  show_significance = TRUE,
  replace_diagonal = FALSE,
  replacement = ""
)

q5_spearman_complete <- complete.cases(vdem[,c("v2x_regime","rule_law")])

cor.test(
  vdem$v2x_regime[q5_spearman_complete],
  vdem$rule_law[q5_spearman_complete],
  method = "spearman",
  alternative = "two.sided",
  exact = FALSE
)

# The correlation coefficient is highly statistically significant (p<0.001).
# Its value is > 0.5 (0.776819) which according to Cohen's conventions indicates
# a strong, positive relationship between the two indices.
# The correlation between the original ordinal regime variable and the four-category
# rule-of-law variable is slightly larger than the point-biserial correlation between 
# the dichotomized regime variable and the continuous rule-of-law variable. This is 
# because the Spearman correlation retains more information about the ordered categories 
# of both variables, while the point-biserial correlation discards information by 
# dichotomizing the regime variable.




## ------------------------------------------------------------------------
## 6. Phi coefficient

# 6a. Dichotomize rule of law using 0.75 as the cut-off point.
# 0 = below 0.75; 1 = 0.75 or higher.

vdem <- vdem %>%
  mutate(rule_law_bin = as.integer(v2xcl_rol >= 0.75))


# 6b. Cross-tabulation

q6_table_075 <- table(vdem$rule_law_bin, vdem$regime_factor)
q6_table_075

round(prop.table(q6_table_075, margin = 1) * 100, 1)

# A positive direction is very clearly apparent here, as 
# the proportion of cases at or above 0.75 is larger among democracies (90.7%) 
# than among autocracies.


# 6c. Phi coefficient
# For two variables coded 0 and 1, Pearson's correlation is the phi coefficient.

correlation_matrix(
  vdem[,c("rule_law_bin","regime")],
  type = "pearson",
  digits = 2,
  decimal.mark = ".",
  use = "lower",
  show_significance = TRUE,
  replace_diagonal = FALSE,
  replacement = ""
)

q6_complete_075 <- complete.cases(vdem[,c("rule_law_bin","regime")])

cor.test(
  vdem$rule_law_bin[q6_complete_075],
  vdem$regime[q6_complete_075],
  method = "pearson",
  alternative = "two.sided"
)


# The chi-squared test gives the equivalent large-sample test of association
# for this 2 x 2 table.

chisq.test(q6_table_075, correct = FALSE)


# 6d. Interpretation

# The correlation coefficient is highly statistically significant (p<0.001).
# Its value is > 0.5 (0.7561555) which according to Cohen's conventions indicates
# a strong, positive relationship between the two indices.
# This means that democracies are disproportionately found in the high-rule-of-law 
# category and autocracies are disproportionately found below the cut-off. 
# A larger absolute phi indicates a larger difference in the conditional distributions
# which is what we have seen in the crosstabulations in part b).
# Despite these large differences, a significance test is still necessary, because an 
# apparent difference in a sample can occur through sampling variation.


# 6e. Repeat the analysis using 0.5 as the cut-off point.

vdem <- vdem %>%
  mutate(rule_law_bin_05 = as.integer(v2xcl_rol >= 0.5))

q6_table_05 <- table(vdem$rule_law_bin_05, vdem$regime)
q6_table_05

round(prop.table(q6_table_05, margin = 1) * 100, 1)

correlation_matrix(
  vdem[,c("rule_law_bin_05","regime")],
  type = "pearson",
  digits = 2,
  decimal.mark = ".",
  use = "lower",
  show_significance = TRUE,
  replace_diagonal = FALSE,
  replacement = ""
)

q6_complete_05 <- complete.cases(vdem[,c("rule_law_bin_05","regime")])

cor.test(
  vdem$rule_law_bin_05[q6_complete_05],
  vdem$regime[q6_complete_05],
  method = "pearson",
  alternative = "two.sided"
)

chisq.test(q6_table_05, correct = FALSE)

# The association is stronger using the 0.75 cut-off because this threshold 
# produces greater separation in the conditional distributions of rule of law 
# across the two regime categories. Almost all regime-1 observations pass the 
# 0.50 threshold, but nearly half of regime-0 observations also pass it. 
# At 0.75, most regime-0 observations fall below the threshold, while most 
# regime-1 observations remain above it.
# This comparison also demonstrates that results based on dichotomization can
# be sensitive to the chosen threshold and discard information contained in the
# original continuous measure.


#
# EOF
#