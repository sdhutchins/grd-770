# Lab 11 --------------------------------------------------------------------
library(car)
library(pastecs)
library(tidyverse)

# Load data
load("module-6/T-test.RData")


# t-test Assumptions
#
# 1. Independence (independent t-test only)
# 
# 2. Normality
# 
# 3. Homoscedasticity
# 
# 4. Continuous Outcome

# Independent t-test example
t.test(formula = time ~ group, data = sniff.ind, 
       paired = FALSE, var.equal = FALSE)

sniff.ind %>%
  group_by(group) %>%
  summarize(mean(time), sd(time))

# Dependent t-test example
t.test(time ~ group, sniff.dep, paired = TRUE, var.equal = TRUE)

sniff.dep %>%
  group_by(group) %>%
  summarize(mean(time),
            sd(time))