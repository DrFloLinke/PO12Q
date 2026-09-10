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
## 1. The Electoral Democracy Index measures whether governments are chosen through free and fair elections, while the Liberal Democracy Index additionally measures civil liberties, the rule of law, and constraints on executive power. We want to find out the extent to which these two measures are related. 

# a. State the null and alternative hypotheses for a Pearson correlation test.

# H0: rho = 0. There is no linear relationship between the Electoral
#     Democracy Index and the Liberal Democracy Index in the population.
# H1: rho != 0. There is a linear relationship between the two indices
#     in the population.


# b. Plot the relationship between the two variables in a scatter plot. 

ggplot(vdem, aes(x = v2x_polyarchy, y = v2x_libdem)) +
  geom_point() +
  geom_smooth(method = 'lm',se = F,colour = '#e57726', linewidth=1.5)+
  labs(x="Electoral Democracy Index", y="Liberal Democracy Index") +
  theme_iqmss()


# c. Calculate Pearson's correlation coefficient and test for significance.

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


# d. Interpret the results and assess the hypotheses from part a.

# The correlation coefficient is highly statistically significant (p<0.001).
# Its value is very close to one (0.9787249) and therefore indicates a strong,
# positive relationship between the two indices. 
# This was indicated by the scatter plot in part b) where the observations align
# closely to a straight line. The null hypothesis of no linear relationship can be rejected.




# e. Conduct a test of statistical power for this coefficient and assess whether a post-hoc test such as this is useful in applied research.

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


# f. Which sample size would be required to detect a correlation of $r=0.3$ with a power of 0.8 and a significance level of 0.05?

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
## 2. To delve deeper into the relationship between democracy and rule of law, we want to find out the extent to which the Liberal Democracy Index is related to the Rule of Law Index. 

# a. State the null and alternative hypotheses for a Pearson correlation test.

# H0: rho = 0. There is no linear relationship between the Liberal Democracy
#     Index and the Rule of Law Index in the population.
# H1: rho != 0. There is a linear relationship between the two indices in the
#     population.


# b. Plot the relationship between the two variables in a scatter plot.

ggplot(vdem, aes(x = v2x_libdem, y = v2xcl_rol)) +
  geom_point() +
  geom_smooth(method = 'lm',se = F,colour = '#e57726', linewidth=1.5) +
  labs(x="Liberal Democracy Index", y="Rule of Law Index") +
  theme_iqmss()


# c. Calculate Pearson's correlation coefficient and test for significance.

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


# d. Based on the scatter plot in part b), why might Pearson's correlation not be the most suitable coefficient? Choose a more suitable correlation coefficient and calculate it. Test for significance.

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


# e. Interpret the results and assess the hypotheses from part a.

# The correlation coefficient is highly statistically significant (p<0.001).
# Its value is very close to one (0.9348255) and therefore indicates a strong,
# positive relationship between the two indices. 
# The significant Pearson coefficient from part c) already provided evidence 
# against the null hypothesis, but Spearman provides a more appropriate assessment
# of this relationship.



## ------------------------------------------------------------------------
## 3. We would expect that liberal democracies enable their civil society to participate more freely than autocracies. To test this, we want to find out the extent to which the Civil Society Participation Index (`v2x_cspart`) is related to the Liberal Democracy Index (`v2x_libdem`).

# a. Plot the relationship between the two variables in a scatter plot, adding a LOESS line to examine the form of the relationship

ggplot(vdem, aes(x = v2x_libdem, y = v2x_cspart)) +
  geom_point() +
  geom_smooth(method = 'loess',se = F,colour = '#e57726', linewidth=1.5) +
  labs(x="Liberal Democracy Index", y="Civil Society Participation Index") +
  theme_iqmss()

# The LOESS line indicates a positive, monotonic relationship between the two indices, 
# but it is not linear. The relationship is stronger at lower values of the Liberal Democracy Index



# b. Calculate Pearson's correlation and drawing on the results of part a) explain why it might not be the most suitable coefficient.

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


# c. Calculate Spearman's correlation coefficient and test for significance.

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


# d. Explain what the application of Spearman's correlation has changed in comparison with Pearson's correlation.

# Whilst the Pearson correlation came in at 0.7905285, Spearman's correlation
# is higher at 0.8403842. This difference reflects the non-linearity of the 
# relationship, as Spearman's correlation is based on ranks and is therefore 
# less affected by the curvature of the relationship. The positive sign of both 
# coefficients indicates that higher values of the Liberal Democracy Index are 
# generally associated with higher values of the Civil Society Participation.



## ------------------------------------------------------------------------
## 4. To explore how Spearman's correlation works in the context of two ordinal variables, we will look at the relationship between the rule of law and civil society participation.

# a. Recode the variable `v2xcl_rol` into an ordered factor with four levels. Choose 0.25, 0.5, and 0.75 as cut points.

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


# b. Apply the same procedure to `v2x_cspart`.

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


# c. Is the assumption of a monotonic relationship between the two variables met? 

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


# d. Calculate Spearman's correlation coefficient and test for significance.

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
## 5. There is considerable debate in the democratization literature, whether democracy is an all-or-nothing affair.

# a. Recode the variable `v2x_regime` into a dichotomous variable with two levels: democracy and autocracy. Choose the cut point between electoral autocracies and electoral democracies.

# V-Dem's Regimes of the World variable is coded as:
# 0 = closed autocracy
# 1 = electoral autocracy
# 2 = electoral democracy
# 3 = liberal democracy

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


# b. Calculate the point-biserial correlation coefficient between the dichotomized `v2x_regime` and the continuous `v2xcl_rol`. Test for significance.

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


# c. Calculate Spearman's correlation for the variables `v2x_regime` and `rule_law`. Why is the point-biserial correlation coefficient different from Spearman's correlation coefficient in part c)?

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
## 6. We suspect that the relationship between regime type and rule of law is quite pronounced at the high end of the rule of law index. To test this, we will dichotomize the rule of law index and compare it with the dichotomized regime variable.

# a. Turn the rule of law index into a binary variable, choosing 0.75 as the cut-off point.

vdem <- vdem %>%
  mutate(rule_law_bin = as.integer(v2xcl_rol >= 0.75))


# b. Create a cross-tabulation between the dichotomized regime variable and the dichotomized rule of law variable. Is a direction of association apparent?

q6_table_075 <- table(vdem$rule_law_bin, vdem$regime_factor)
q6_table_075

round(prop.table(q6_table_075, margin = 1) * 100, 1)

# A positive direction is very clearly apparent here, as 
# the proportion of cases at or above 0.75 is larger among democracies (90.7%) 
# than among autocracies.


# c. Calculate the phi coefficient and test for significance.

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


# d. Compare the results of the test with your expectation from part b). Explain how the correlation coefficient corresponds to the conditional distributions in the crosstabulation, and why a test of significance is necessary to assess the relationship between two variables.

# The correlation coefficient is highly statistically significant (p<0.001).
# Its value is > 0.5 (0.7561555) which according to Cohen's conventions indicates
# a strong, positive relationship between the two indices.
# This means that democracies are disproportionately found in the high-rule-of-law 
# category and autocracies are disproportionately found below the cut-off. 
# A larger absolute phi indicates a larger difference in the conditional distributions
# which is what we have seen in the crosstabulations in part b).
# Despite these large differences, a significance test is still necessary, because an 
# apparent difference in a sample can occur through sampling variation.


# e. Repeat steps a)–d) using 0.5 as the cut-off point for the rule of law index. How do the results differ and why?

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