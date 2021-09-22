# Lab 9 --------------------------------------------------------------------
library(tidyverse)
library(broom)
library(knitr)
library(kableExtra)
library(QuantPsyc)
library(ggthemr)

# Set theme
ggthemr('fresh')

load("module-5/framingham.RData")

sysBPvsAge <- lm(formula = sysBP ~ age, data = fhs)

summary(sysBPvsAge)

# Reporting a linear regression table
sysBPvsAge_tidy <- tidy(sysBPvsAge)

sysBPvsAge_tidy

kable_styling(kable(sysBPvsAge_tidy, digits = 4, col.names = c("Predictor", "Estimate", "SE", "t-value", "p-value")))

# Plotting the Regression
ggplot(fhs, aes(x = age, y = sysBP)) +
  geom_point() +
  geom_smooth(method = "lm")

# Multiple Linear Regression
sysBPvsAgeandGluc <- lm(formula = sysBP ~ age + glucose, data = fhs)

summary(sysBPvsAgeandGluc)

# Generate standardized beta coefficients
lm.beta(sysBPvsAgeandGluc)

# Reporting a linear regression
#
# Reporting our results:
#   
#   Multiple regression analysis was used to investigate whether glucose and age are significantly associated with systolic blood pressure in 50 individuals from the Framingham Heart Study. Together, age and glucose accounted for 40.23% of the variance (R-squared = 0.4023, p = 5.6e-6, N = 50). The partial regression coefficients for age and glucose were both significant [age: Estimate = 0.5939 ± 0.2440, Beta = 0.286, t = 2.433, p = 0.019; glucose: Estimate = 0.4615 ± 0.1110, Beta = 0.490, t = 4.158, p = 0.0001].
# 
# Explanation of the inserted variables:
#   
#   R-squared: Multiple R-squared term (not adjusted)
# 
# p: gotten from the very bottom next to the F-Statistic
# 
# N: Sample size
# 
# For each independent variable:
#   
#   Estimate: Estimate for that variable ± Std. Error for that variable
# 
# Beta: Standardized regression coefficient (from lm.beta)
# 
# t: t value from the table for that variable
# 
# p: p value from the table for that variable



# Lab 10 --------------------------------------------------------------------
library(pastecs)
library(broom)
library(car)
library(tidyverse)

# Reviewing our model
sysBPvsAge <- lm(formula = sysBP ~ age, data = fhs)

summary(sysBPvsAge)

# Checking
# There are 4 assumptions that apply to simple linear regression:
#   
#   - Independence
# 
#   - Linearity
# 
#   - Homoscedasticity
# 
#   - Normality

# Independence
# There are no statistical tests for the independence assumption. However, 
# from the data and the way it is collected, you can determine whether each 
# sample is independent from the other samples in the dataset. You can say 
# something along the lines of: “The independence assumption is met because 
# the data for each sample was collected independently from the other samples”. 
# In our case, we know that all 50 rows represent 50 different individuals 
# and so this assumption is clearly met.

# Linearity
# As we have discussed previously, we assess linearity using a scatterplot. 
# Let’s go ahead and recreate that plot, again adding a regression line 
# using geom_smooth. Bear in mind that the geom_smooth overlay is simply
# used to aid visual inspection of the data, and the scatterplot alone 
# should be enough to assess linearity.

ggplot(data = fhs, mapping = aes(x = age, y = sysBP)) +
  geom_point() +
  geom_smooth(method = "lm")

# Homoscedasticity of Residuals
sysBPvsAge_Aug <- augment(sysBPvsAge)

mean(sysBPvsAge_Aug$.resid)

# Residual plots
ggplot(data = sysBPvsAge_Aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

# Normality
# Normality will be tested in the same way we have done 
# previously: histograms, Q-Q plots, boxplots, and statistical tests. 
# Please refer to the previous labs if you need to review this content.
# 
# We have now tested all of the required assumptions for a simple linear 
# regression, but there is one more assumption for multiple 
# linear regression: multicollinearity.

# Multicollinearity
# Multicollinearity occurs when predictor variables in a multiple linear
# regression are highly correlated. This causes problems with both parameter
# estimation and prediction, so we need to make sure our predictors are fairly 
# unique. We will test multicollinearity by calculating the variance 
# inflation factor using the vif function from the car package.
# 
# In order to use the function, you only need to pass the model into the vif function
# 
# vif(model)

# Let’s use vif to identify the main effects of age and glucose on systolic blood pressure.

sysBPvsAgeandGlucose <- lm(formula = sysBP ~ age + glucose, data = fhs)

vif(sysBPvsAgeandGlucose)