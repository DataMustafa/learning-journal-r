Premiums <- read.csv("Premiums.csv")
install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
zone_policy_count <- Premiums %>%
  group_by(ZONE_NAME) %>%
  summarise(policy_count = n())
mode_count <- get_mode(zone_policy_count$policy_count)
mode_count
install.packages("car")
library(car)
boxplot(Premiums$Vintage_Period, main = "Boxplot of Vintage Period", ylab = "Vintage Period")
outliers <- outlier(Premiums$Vintage_Period)
outliers
boxplot(Premiums$Vintage_Period, main = "Boxplot of Vintage Period", ylab = "Vintage Period")
outliers <- boxplot.stats(Premiums$Vintage_Period)$out
outliers
library(car)
outliers <- outlier(Premiums$Vintage_Period)
outliers
install.packages("outliers")
library(outliers)
outliers <- outlier(Premiums$Vintage_Period)
outliers
install.packages("e1071")
library(e1071)
skew_kurtosis <- Premiums %>%
  group_by(ZONE_NAME) %>%
  summarise(
    skewness = skewness(Premium),
    kurtosis = kurtosis(Premium)
  )

skew_kurtosis
install.packages("ggplot2")
library(ggplot2)
ggplot(Premiums, aes(x = Vintage_Period, y = Premium)) +
  geom_point() +
  labs(title = "Scatter plot of Premium vs Vintage Period", x = "Vintage Period", y = "Premium")
correlation <- cor(Premiums$Premium, Premiums$Vintage_Period)
correlation