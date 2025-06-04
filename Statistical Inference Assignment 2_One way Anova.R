data <- read.csv("M2 T2_anova assignment data (1).csv")
str(data)
head(data)
data$Zone <- with(data, ifelse(StoreNo %in% c(1:4, 13:16), "West",
                               ifelse(StoreNo %in% c(5:8, 17:20), "South", "East")))
data$Zone <- as.factor(data$Zone)
data$Campaign <- as.factor(data$Campaign)
head(data)
boxplot(Growth ~ Campaign, data = data,
        col = c("lightblue", "lightgreen"),
        main = "Sales Growth by Campaign Type")
t_test_result <- t.test(Growth ~ Campaign, data = data, var.equal = TRUE)
print(t_test_result)
anova_model <- aov(Growth ~ Campaign * Zone, data = data)
summary(anova_model)
interaction.plot(data$Zone, data$Campaign, data$Growth,
                 col = c("red", "blue"),
                 legend = TRUE,
                 xlab = "Zone", ylab = "Sales Growth", main = "Interaction Plot")