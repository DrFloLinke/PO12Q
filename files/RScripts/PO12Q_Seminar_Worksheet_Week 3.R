# PO12Q, Worksheet Week 3, Exercise 1
#####################################

# set working directory
setwd("~/OneDrive - University of Warwick/Warwick/Modules/PO12Q/Seminars/PO12Q_Seminar_Week 3")

# Load required packages
library(readxl)
library(ggplot2)

# load data set
gujarati <- read_excel("Gujarati2.xlsx")

# create graph
ggplot(gujarati, aes(x=totalexp, y=foodexp)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE)+
  theme_classic()
