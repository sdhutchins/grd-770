# Lab 4 --------------------------------------------------------------------
# Plotting Real Data Using ggplot
library(tidyverse)
library(ggthemr)

# Set theme
ggthemr("light")

realdata <- read_delim(file = "module-2/realdata.csv", delim = ",")

ggplot(data = realdata, mapping = aes(x = Group, y = Activity)) +
  geom_bar(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.3) +
  geom_jitter(width = 0.3)

ggplot(data = realdata, mapping = aes(x = Group, y = Activity)) +
  geom_bar(mapping = aes(fill = Group), stat = "summary", fun = "mean", show.legend = FALSE) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.3) +
  geom_jitter(width = 0.3) +
  facet_wrap(~Strain) +
  labs(title = "Group A Activity vs Group B Activity, separated into Strain 1 and 2")

# Lab 5 --------------------------------------------------------------------
# Central Limit Theorem Lab
library(tidyverse)
library(sciplot)

# Load microbiome data
load("module-2/microbiome.RData")

ggplot(data = microbiome, mapping = aes(x = Fir)) +
  geom_histogram(binwidth = 0.01, color = "black")

ggplot(data = microbiome, mapping = aes(x = Bacter)) +
  geom_histogram(binwidth = 0.01, color = "black")


ggplot(data = microbiome, mapping = aes(x = Actin)) +
  geom_histogram(binwidth = 0.01, color = "black")

ggplot(data = microbiome, mapping = aes(x = Verru)) +
  geom_histogram(binwidth = 0.01, color = "black")

# Perform statistics on data
mean(microbiome$Fir)

sd(microbiome$Fir)

se(microbiome$Fir)

# Learning new functions
# Learning pivot_longer
microbiome_long <- pivot_longer(
  data = microbiome, cols = Fir:Verru,
  names_to = "Phylum",
  values_to = "Percentage"
)

head(microbiome_long)

# Learning group_by
microbiome_grouped <- group_by(microbiome_long, Phylum)

# Learning summarize
microbiome_summary <- summarize(microbiome_grouped,
  mean = mean(Percentage),
  sd = sd(Percentage),
  se = se(Percentage)
)
# Show summary data
microbiome_summary


# Generate repeated sampling of data
# Use the replicate function to replicate the mean of the sample data
# by Phylum
Fir_means <- replicate(n = 1000, expr = mean(sample(
  x = microbiome$Fir,
  size = 30,
  replace = FALSE
)))
Bacter_means <- replicate(n = 1000, expr = mean(sample(
  x = microbiome$Bacter,
  size = 30,
  replace = FALSE
)))
Actin_means <- replicate(n = 1000, expr = mean(sample(
  x = microbiome$Actin,
  size = 30,
  replace = FALSE
)))
Verru_means <- replicate(n = 1000, expr = mean(sample(
  x = microbiome$Verru,
  size = 30,
  replace = FALSE
)))
# Create a tibble of the phylum means
microbiome_means <- tibble(
  Fir = Fir_means, Bacter = Bacter_means,
  Actin = Actin_means, Verru = Verru_means
)

# Use pivot_longer to elongate the data so it's easier to plot
microbiome_means_long <- pivot_longer(microbiome_means,
  cols = Fir:Verru,
  names_to = "Phylum",
  values_to = "Sample_Mean"
)

# Plot the means
ggplot(data = microbiome_long, mapping = aes(x = Percentage)) +
  geom_histogram(binwidth = 0.01, color = "black") +
  facet_wrap(~Phylum, scales = "free_y")

# Verify the central limit theorem
ggplot(data = microbiome_means_long, mapping = aes(x = Sample_Mean, fill = Phylum)) +
  geom_histogram(bins = 40, color = "black") +
  facet_wrap(~Phylum, scales = "free")
