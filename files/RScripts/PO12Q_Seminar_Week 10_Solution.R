################################
## SOLUTIONS, WEEK 10, PART 1 ##
################################

setwd("~/OneDrive - University of Warwick/Warwick/Modules/PO12Q/Seminars/PO12Q_Seminar_Week 10/Solutions")

# 1. Load the data set `london.csv` as an object called "london"
london <- read.csv("Data/london_11.csv")

#2. Subset the data to a random sample as follows:
set.seed(123)
london_sam <- sample_frac(london, 0.65, replace=FALSE)


# 3. Regress GCSE scores on `income` and interpret the results.
model1 <- lm(gcse ~ income, data=london_sam)
summary(model1)

##>> jncome is significant, and has a positive effect on GCSE scores: for every unit of additional household income the GCSE score rises by 7.750e-05 points on average. We explain 16% of the variance in GCSE scores with this variable. 



# 4. Breusch-Pagan Test by Hand
#a. Extract the residuals from model 1 and store their squared values in a new object called `ressq`

ressq <- resid(model1)^2

#b. Regress the squared residuals on the original independent variable.

model1res <- lm(ressq~income, data=london_sam)

#c. Store the number of observations of this new model in an object called `N`.

N <- nobs(model1res)

#d. Obtain the model fit measure R$^2$ for this model.

summary(model1res)

#e. Calculate the test-statistic for the Breusch-Pagan test as the product of `N` and R$^2$

chisq <- 0.001857*N

#f. Conduct a Chi-Squared test with one degree of freedom.

pchisq(chisq, df=1, lower.tail=FALSE)

##>> The test is insignificant, and therefore we accept the null-hypothesis of homoscedasticity. We have not violated this assumption. 

#5. Breusch-Pagan Test with the `lmtest` package
#a. Install and load the `lmtest` package

library(lmtest)

#b. Call

bptest(model1)

##>> Same results.

#c. Compare the results with the results obtained in Exercise 4.
##>> Same results.

#6. Heteroscedasticity
#a. Now let's introduce heteroscedasticity:

london_heterosc <- filter(london, income<28000 | income>100000)

#b. Estimate a new model, and note the significance of coefficients.\label{ex:6b}

model2 <- lm(gcse ~ income, data=london_heterosc)

#c. Conduct a Breusch-Pagan Test with the `lmtest` package.

bptest(model2)

##>> This is significant, and thus we reject the null-hypothesis which means we have heteroscedasticity. 

#d. Install and load the `sandwich` package.

library(sandwich)

#e. The `sandwich` package replaces $\Omega$ with a new diagonal which takes into account heteroscedasticity. The default is `HC3` -- if you ever work with somebody using Stata then you need to replace this with `HC1` to obtain the same results.  Conduct a significance test with robust standard errors (with the `sandwich` function `coeftest`):

coeftest(model2, vcov = vcovHC(model2, type="HC3"))

#f. Compare the significance to the results in 6b and explain the reason for the difference. 
##>> the slope coefficient is no longer significant, as we the standard error is now wider, and thus creates a smaller t-value. 


## (Multi-)Collinearity

#1. We will continue using the `london_sam` data frame. 
#2. Regress GCSE scores on `income` and `idaci` in a multiple regression model. Interpret the results. 

model3 <- lm(gcse ~ income + idaci, data=london_sam)
summary(model3)

# 3. Install and load the `car` package.

library(car)

#4. Calculate the Variance Inflation Factor (VIF) for this model as follows:

vif(model3)

#5. Interpret the results.
##>> VIF is <10, therefore we don't have collinearity.

#6. Regress GCSE scores on `houseprice`. Store the results in an object called `model4`. Note the significance of the slope coefficient and interpret the results.

model4 <- lm(gcse ~ houseprice, data=london_sam)
summary(model4)

#7. Let's introduce some collinearity by estimating the following model:

model5 <- lm(gcse ~ income + houseprice, data=london_sam)
summary(model5)

#8. Calculate the VIF for model 5. Interpret the results.

vif(model5)

#9. Drawing on the results of Exercise 8, explain the significance of the slope coefficients of Model 1 (Exercise 3, Subsection 1.1) and Model 4 (Exercise 6 of this Subsection). 

##>> Individually the variables income and houseprice produce significant slope coefficients. Due to high collinearity between the two variables, the results in Model 5 render houseprice insignificant. Income is a better predictor of GCSE scores, because it produces a higher R-Squared measure than houseprice.


## GAUSS MARKOV THEOREM

## see Solutions pdf################################
## SOLUTIONS, WEEK 10, PART 1 ##
################################

setwd("~/OneDrive - University of Warwick/Warwick/Modules/PO12Q/Seminars/PO12Q_Seminar_Week 10/Solutions")

# 1. Load the data set `london.csv` as an object called "london"
london <- read.csv("Data/london_11.csv")

#2. Subset the data to a random sample as follows:
set.seed(123)
london_sam <- sample_frac(london, 0.65, replace=FALSE)


# 3. Regress GCSE scores on `income` and interpret the results.
model1 <- lm(gcse ~ income, data=london_sam)
summary(model1)

##>> jncome is significant, and has a positive effect on GCSE scores: for every unit of additional household income the GCSE score rises by 7.750e-05 points on average. We explain 16% of the variance in GCSE scores with this variable. 



# 4. Breusch-Pagan Test by Hand
#a. Extract the residuals from model 1 and store their squared values in a new object called `ressq`

ressq <- resid(model1)^2

#b. Regress the squared residuals on the original independent variable.

model1res <- lm(ressq~income, data=london_sam)

#c. Store the number of observations of this new model in an object called `N`.

N <- nobs(model1res)

#d. Obtain the model fit measure R$^2$ for this model.

summary(model1res)

#e. Calculate the test-statistic for the Breusch-Pagan test as the product of `N` and R$^2$

chisq <- 0.001857*N

#f. Conduct a Chi-Squared test with one degree of freedom.

pchisq(chisq, df=1, lower.tail=FALSE)

##>> The test is insignificant, and therefore we accept the null-hypothesis of homoscedasticity. We have not violated this assumption. 

#5. Breusch-Pagan Test with the `lmtest` package
#a. Install and load the `lmtest` package

library(lmtest)

#b. Call

bptest(model1)

##>> Same results.

#c. Compare the results with the results obtained in Exercise 4.
##>> Same results.

#6. Heteroscedasticity
#a. Now let's introduce heteroscedasticity:

london_heterosc <- filter(london, income<28000 | income>100000)

#b. Estimate a new model, and note the significance of coefficients.\label{ex:6b}

model2 <- lm(gcse ~ income, data=london_heterosc)

#c. Conduct a Breusch-Pagan Test with the `lmtest` package.

bptest(model2)

##>> This is significant, and thus we reject the null-hypothesis which means we have heteroscedasticity. 

#d. Install and load the `sandwich` package.

library(sandwich)

#e. The `sandwich` package replaces $\Omega$ with a new diagonal which takes into account heteroscedasticity. The default is `HC3` -- if you ever work with somebody using Stata then you need to replace this with `HC1` to obtain the same results.  Conduct a significance test with robust standard errors (with the `sandwich` function `coeftest`):

coeftest(model2, vcov = vcovHC(model2, type="HC3"))

#f. Compare the significance to the results in 6b and explain the reason for the difference. 
##>> the slope coefficient is no longer significant, as we the standard error is now wider, and thus creates a smaller t-value. 


## (Multi-)Collinearity

#1. We will continue using the `london_sam` data frame. 
#2. Regress GCSE scores on `income` and `idaci` in a multiple regression model. Interpret the results. 

model3 <- lm(gcse ~ income + idaci, data=london_sam)
summary(model3)

# 3. Install and load the `car` package.

library(car)

#4. Calculate the Variance Inflation Factor (VIF) for this model as follows:

vif(model3)

#5. Interpret the results.
##>> VIF is <10, therefore we don't have collinearity.

#6. Regress GCSE scores on `houseprice`. Store the results in an object called `model4`. Note the significance of the slope coefficient and interpret the results.

model4 <- lm(gcse ~ houseprice, data=london_sam)
summary(model4)

#7. Let's introduce some collinearity by estimating the following model:

model5 <- lm(gcse ~ income + houseprice, data=london_sam)
summary(model5)

#8. Calculate the VIF for model 5. Interpret the results.

vif(model5)

#9. Drawing on the results of Exercise 8, explain the significance of the slope coefficients of Model 1 (Exercise 3, Subsection 1.1) and Model 4 (Exercise 6 of this Subsection). 

##>> Individually the variables income and houseprice produce significant slope coefficients. Due to high collinearity between the two variables, the results in Model 5 render houseprice insignificant. Income is a better predictor of GCSE scores, because it produces a higher R-Squared measure than houseprice.


## GAUSS MARKOV THEOREM

## see Solutions pdf