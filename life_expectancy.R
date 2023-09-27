# ******************************************************************************
# *                               COPYRIGHT NOTICE                             *
# ******************************************************************************
# *                                                                            *
# *  This code is authored by Marcelo Villalobos Diaz                          *
# *  You are free to use, modify, and distribute this code, provided           *
# *  you give appropriate credit by including the author's name.               *
# *                                                                            *
# *  Copyright (c) 2023 Marcelo Villalobos Diaz                                *
# *                                                                            *
# ******************************************************************************

# loads the libraries for this project
library(ggplot2)
library(readr)
library(dplyr)

# imports and inspects the data
data <- read_csv("life_expectancy_by_country.csv")
head(data)

# creates a vector using the life_expectancy column
life_expectancy <- data %>%
    pull(life_expectancy)

# finds the quartiles of the life_expectancy vector
life_expectancy_quartiles <- quantile(life_expectancy, c(0.25, 0.5, 0.75))
print("Below are the quartiles for the life expectancy around the world:")
life_expectancy_quartiles

# plots histogram of the life_expectancy vector. See PDF.
hist(life_expectancy)

# creates a vextor using the GDP column
gdp <- data %>%
    pull(GDP)

# finds the GDP median
median_gdp <- quantile(gdp, c(0.5))
sprintf("This is the median GDP: %f", median_gdp)

# now lets devide the GDP in two groups (low and high GDP)
low_gdp <- data %>%
    filter(GDP <= median_gdp) %>%
    pull(life_expectancy)

high_gdp <- data %>%
    filter(GDP > median_gdp) %>%
    pull(life_expectancy)

# now lets find the quartiles for each group (low and high gpd)
low_gdp_quartiles <- quantile(low_gdp, c(0.25, 0.5, 0.75))
low_gdp_quartiles

high_gdp_quartiles <- quantile(high_gdp, c(0.25, 0.5, 0.75))
high_gdp_quartiles

# lets plot histograms for each group (low and high GDP). See PDF.
hist(low_gdp, col = "red")

hist(high_gdp, col = "blue")

# In conclusion we can see that people usually live longer if the
# country where they live has a high GDP.

# Added a scatter plot to see the relationship between life expectancy and GDP
scatter_plot <- ggplot(data, aes(x = GDP, y = life_expectancy)) +
  geom_point() +
  labs(
    title = "Scatter Plot of Life Expectancy vs. GDP",
    x = "GDP",
    y = "Life Expectancy"
  ) +
  theme_minimal()

print(scatter_plot)