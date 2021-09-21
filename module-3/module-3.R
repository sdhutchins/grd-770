# Lab 6 --------------------------------------------------------------------

# Assessing Normality Graphically

library(sciplot)
library(car)
library(pastecs)
library(tidyverse)
library(ggthemr)

# Set theme
ggthemr("light")

load("module-3/microbiome2.RData")

# Histogram
ggplot(data = microbiome2, mapping = aes(x = Fir)) +
  geom_histogram(mapping = aes(y = ..density..), bins = 30, color = "black") +
  stat_function(fun = "dnorm", args = list(mean = mean(microbiome2$Fir), sd = sd(microbiome2$Fir)))

# QQ Plot
ggplot(data = microbiome2, mapping = aes(sample = Fir)) +
  geom_qq() +
  geom_qq_line()

# Boxplot
ggplot(data = microbiome2, mapping = aes(y = Fir)) +
  geom_boxplot()

# Assessing Normality Mathematically

stat.desc(x = microbiome2$Fir, basic = FALSE, desc = TRUE, norm = TRUE)

# Skewness can be either positive or negative. 
# Closer to 0 means less skewed. Positive values mean there 
# is a pile-up of values on the left side of the distribution
# while a negative value means they are piled on the right side.

# Kurtosis can be positive or negative. Positive values mean 
# the tail are heavy and drop rapidly, negative values indicate a 
# much more gradual fall.

# Second assumption: Homoscedascity
# Use leveneTest from the car package
leveneTest(Fir ~ Sex, microbiome2)