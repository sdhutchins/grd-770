# Lab 7 --------------------------------------------------------------------
library(pastecs)
library(tidyverse)
library(ggthemr)

# Set theme
ggthemr('fresh')

# Load the data
load("module-4/ABI.RData")

# Testing Normality Assumption of Tau across all tissues
# Histogram
ggplot(data = ABI, mapping = aes(x = tau)) +
  geom_histogram(mapping = aes(y = ..density..), bins = 20, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(ABI$tau, na.rm = TRUE), 
                                         sd = sd(ABI$tau, na.rm = TRUE)))

# Q-Q plot
ggplot(data = ABI, mapping = aes(sample = tau)) + 
  geom_qq() + 
  geom_qq_line()

# Boxplot
ggplot(data = ABI, mapping = aes(y = tau)) + 
  geom_boxplot()

# Mathematical Normality tests using the stat.desc function
stat.desc(ABI$tau, basic = FALSE, desc = TRUE, norm = TRUE)

# Wrap in the enframe function to output it in a tidy tibble.
enframe(stat.desc(ABI$tau, basic = FALSE, desc = TRUE, norm = TRUE))


# Now, let’s further clean up our output with the pivot_wider function. 
# We used the opposite function (pivot_longer) in an earlier lab.
# Basically, this function will take all of the names in the name column 
# and make them column names, with the value column filling in the values 
# for those columns. Again, the pipe will make this much easier. We’ll want 
# to assign the output of all of this to an object for easier viewing. 
# Go ahead and click on the name of the output table in your Environment 
# to view it.
tau_norm_table <- stat.desc(ABI$tau, basic = FALSE, desc = TRUE, norm = TRUE) %>% 
  enframe() %>% 
  pivot_wider(names_from = name, values_from = value)


# Testing Normality Assumption of Phosporylated Tau in each tissue individually
# Graphical Normality tests
# Assess normality graphically for the ptau variable across all tissues

# Histogram
ggplot(data = ABI, mapping = aes(x = ptau)) + 
  geom_histogram(mapping = aes(y = ..density..), bins = 20, color = 'black') + 
  stat_function(fun = dnorm, args = list(mean = mean(ABI$ptau, na.rm = TRUE), 
                                         sd = sd(ABI$ptau, na.rm = TRUE)))

# Q-Q plot
ggplot(data = ABI, mapping = aes(sample = ptau)) + 
  geom_qq() + 
  geom_qq_line()

# Boxplot
ggplot(data = ABI, mapping = aes(y = ptau)) + 
  geom_boxplot()

# Faceted Histogram
ggplot(data = ABI, mapping = aes(x = ptau)) + 
  geom_histogram(mapping = aes(y = ..density..), bins = 20, color = 'black') +
  facet_wrap(~ struct)

# Faceted Q-Q plot
ggplot(data = ABI, mapping = aes(sample = ptau)) + 
  geom_qq() + 
  geom_qq_line() +
  facet_wrap(~ struct)

# Faceted Boxplot
ggplot(data = ABI, mapping = aes(y = ptau)) + 
  geom_boxplot() +
  facet_wrap(~ struct)

# Mathematical normality tests.
ptau_norm_table <- stat.desc(ABI$ptau, basic = FALSE, desc = TRUE, norm = TRUE) %>% 
  enframe() %>% 
  pivot_wider(names_from = name, values_from = value)

# Default aggregate behavior
# Perform stat.desc by struct
aggregate(ptau ~ struct, data = ABI, FUN = stat.desc, 
          basic = FALSE, desc = TRUE, norm = TRUE)

# Modified code to output a tidy tibble
ptau_norm_table2 <- aggregate(ptau ~ struct, data = ABI, 
                              FUN = stat.desc, basic = FALSE, desc = TRUE, 
                              norm = TRUE, 
                              simplify = FALSE) %>% unnest_wider(col = ptau)


### Practice
# Test the normality assumption for mip_1a the following two ways:
#       1. Across all tissues
#       2. In each tissue individually

# 1. 
mip_1a_norm_table <- stat.desc(ABI$mip_1a, basic = FALSE, desc = TRUE, norm = TRUE) %>% 
  enframe() %>% 
  pivot_wider(names_from = name, values_from = value)

# 2.
mip_1a_norm_table2 <- aggregate(mip_1a ~ struct, data = ABI, 
                              FUN = stat.desc, basic = FALSE, desc = TRUE, 
                              norm = TRUE, 
                              simplify = FALSE) %>% unnest_wider(col = mip_1a)

# Lab 8 --------------------------------------------------------------------
