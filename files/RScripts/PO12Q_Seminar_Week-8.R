################################################
# PO12Q, WORKSHEET, WEEK 9
################################################

# set wd
setwd("~/OneDrive - University of Warwick/Warwick/Modules/PO12Q/Seminars/PO12Q_Seminar_Week 8")

# load packages
library(stargazer)
library(tidyverse)

# load data set
wdi <- read.csv("WDI_PO12Q.csv")

#Run Models for Table 7
wdi_life <- lm(gdppc ~ lifeexp, data = wdi)
summary(wdi_life)


wdi_urban <- lm(gdppc ~ urban, data = wdi)
summary(wdi_urban)


wdi_joint <- lm(gdppc ~ lifeexp + urban, data = wdi)
summary(wdi_joint)

# This produces Table 7
stargazer(wdi_life, wdi_urban, wdi_joint,
          header=F, 
          font.size = "scriptsize", 
          omit.stat = c("f", "ser"),
          dep.var.labels   = "per capita GDP",
          covariate.labels = c("Life Expectancy", "Urbanisation"))

#gdppc_i_hat= -74786.14_hat + 1012.17_hat life expectancy_i + 273.74_hat urban_i

#Run additional models for Table 8
wdi_lit <- lm(gdppc ~ literacy, data = wdi)

wdi_joint1 <- lm(gdppc ~ lifeexp + urban + literacy, data = wdi)


# This produces Table 8
stargazer(wdi_life, wdi_urban, wdi_joint, wdi_lit, wdi_joint1,
          header=F, 
          font.size = "scriptsize", 
          omit.stat = c("f", "ser"),
          dep.var.labels   = "per capita GDP",
          covariate.labels = c("Life Expectancy", "Urbanisation", "Literacy"))


#Run models for Table 9
wdi_1 <- lm(gdppc ~ literacy + lifeexp, data = wdi)

wdi_2 <- lm(gdppc ~ literacy + urban, data = wdi)


# This produces Table 9
stargazer(wdi_enrol, wdi_1, wdi_2,
          header=F, 
          font.size = "scriptsize", 
          omit.stat = c("f", "ser"),
          dep.var.labels   = "per capita GDP",
          covariate.labels = c("Literacy", "Life Expectancy", "Urbanisation"))


# Answer to question on infant mortality (bottom, page 5)
wdi_3 <- lm(gdppc ~ literacy + infant, data = wdi)
summary(wdi_3)
# >> Yes, it does.
# >> We conclude that regardless of measurement, health is more important in explaining the level of wealth in a country than education. 


#Run models for Table 10
life <- lm(gdppc ~ lifeexp, data = wdi)
infant <- lm(gdppc ~ infant, data = wdi)


# This produces Table 10
stargazer(life, infant,
          header=F, 
          font.size = "tiny", 
          omit.stat = c("f", "ser"),
          dep.var.labels   = "per capita GDP",
          covariate.labels = c("Life Expectancy", "Infant Mortality"))

# >>Life expectancy - the R-Squared is far higher.
