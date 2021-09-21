# Lab 2 --------------------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(ggthemr)

data("iris")

iris$Sepal.Length

sl <- iris$Sepal.Length

filtered_iris <- filter(iris, Sepal.Length < 5)

mean(filtered_iris$Sepal.Length)
mean(iris$Sepal.Length)
mean(sl)


# Practice
a <- filter(iris, Petal.Width > 1)
b <- filter(iris, Petal.Width < 1)
mean(a$Petal.Width)
mean(b$Petal.Width)


# Practice Challenge
x <- filter(iris, Petal.Length == 1.4)
mean(x$Petal.Length)
y <- filter(iris, Petal.Length >= 1.4)
mean(y$Petal.Length)
z <- filter(iris, Petal.Length <= 1.4)
mean(z$Petal.Length)

# Lab 3 --------------------------------------------------------------------
# Plotting Sample Data using 
ggthemr('grape')
ggplot(data = iris, mapping = aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
    geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "Sepal Width", y = "Sepal Length",
    title = "Plotting Sepal Length vs Sepal Width for Three Species of Iris") +
  scale_colour_ggthemr_d()
