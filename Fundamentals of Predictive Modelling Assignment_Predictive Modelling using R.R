install.packages("readr")
library(readr)
house_data <- read_csv("House Price Data.csv")
cat("1. Data Structure:\n")
str_house_data <- capture.output(str(house_data))
cat(str_house_data, sep = "\n")
cat("\n1. First Few Rows of Data:\n")
head_house_data <- capture.output(head(house_data))
cat(head_house_data, sep = "\n")
n <- nrow(house_data)
train_size <- floor(0.8 * n)
set.seed(123) # Set a random seed for reproducibility
train_indices <- sample(1:n, train_size, replace = FALSE)
train_data <- house_data[train_indices, ]
test_data <- house_data[-train_indices, ]
cat("\n2. Training and Test Data Set Sizes:\n")
cat("Training data set size:", nrow(train_data), "\n")
cat("Test data set size:", nrow(test_data), "\n")
model <- lm(Price ~ Area + Distance + Schools, data = train_data)
cat("\n3. Regression Model Summary:\n")
summary_model <- capture.output(summary(model))
cat(summary_model, sep = "\n")

cat("\n4. Significant Variables and Coefficients:\n")
model_summary <- summary(model)
coefficients <- coef(model)
p_values <- model_summary$coefficients[, "Pr(>|t|)"]
significant_vars <- names(p_values[p_values < 0.05]) # For example, at 0.05 significance level
if (length(significant_vars) > 0) {
  cat("Significant variables:\n")
  for (var in significant_vars) {
    cat("- ", var, "\n", sep = "")
    cat("  Coefficient: ", coefficients[var], "\n", sep = "")
  }
} else {
  cat("No significant variables found.\n")
}

cat("\n5. R-squared and Adjusted R-squared Values:\n")
r_squared <- model_summary$r.squared
adjusted_r_squared <- model_summary$adj.r.squared

cat("R-squared value: ", r_squared, "\n", sep = "")
cat("Adjusted R-squared value: ", adjusted_r_squared, "\n", sep = "")

cat("\n5. Interpretation of R-squared and Adjusted R-squared:\n")
cat("-----------------------------------------------------------\n")
cat("The R-squared value of the model is: ", r_squared, ".\n", sep="")
cat("This means that approximately ", round(r_squared * 100, 2),
    "% of the variation in house prices can be explained by house area, distance to the nearest metro station, and the number of schools.\n", sep="")
cat("The adjusted R-squared value is: ", adjusted_r_squared, ".\n", sep="")
cat("This value provides a more realistic estimate of the model's explanatory power by taking into account the number of variables in the model.\n", sep="")

if (r_squared - adjusted_r_squared < 0.05) { # 0.05 is used as an example threshold
  cat("The fact that the adjusted R-squared value is close to the R-squared value suggests that the variables in the model contribute significantly to the explanatory power.\n")
} else {
  cat("The fact that the adjusted R-squared value is lower than the R-squared value suggests that some of the variables in the model may be unnecessary.\n")
}

cat("\nImportant Note:\n")
cat("* R-squared and adjusted R-squared values are important measures for evaluating the fit of the model, but they are not sufficient on their own.\n")
cat("  It is important to also check other statistical measures and assumptions to assess the accuracy and generalizability of the model.\n")
cat("* A high R-squared value indicates that the model fits the data well, but this does not guarantee how well the model will predict future data.\n")

install.packages("car")
library(car)
cat("\n6. Multicollinearity Check:\n")
vif_values <- vif(model)
cat("VIF Values:\n")
vif_values_output <- capture.output(print(vif_values))
cat(vif_values_output,sep="\n")

cat("\nCorrelation Matrix:\n")
cor_matrix <- cor(train_data[, c("Area", "Distance", "Schools")])
cor_matrix_output <- capture.output(print(cor_matrix))
cat(cor_matrix_output,sep="\n")

cooksd <- cooks.distance(model)
hat_values <- hatvalues(model)
studentized_residuals <- rstandard(model)
n <- nrow(train_data)
cooks_threshold <- 4/n
p <- length(coef(model)) # Number of parameters in the model
hat_threshold <- 2*p/n

cat("\n7. Influential Observations:\n")
cat("Observations with Cook's Distance greater than the threshold:", which(cooksd > cooks_threshold), "\n")
cat("Observations with hat values greater than the threshold:", which(hat_values > hat_threshold), "\n")
cat("Observations with studentized residuals greater than 3:", which(abs(studentized_residuals) > 3), "\n")

cat("\n7. Cook's Distance Plot:\n")
plot_cooks_distance <- plot(cooksd, main = "Cook's Distance", ylab = "Cook's D")
abline(h = cooks_threshold, col = "red", lty = 2)
text(which(cooksd > cooks_threshold), cooksd[cooksd > cooks_threshold],
     labels = which(cooksd > cooks_threshold), pos = 4, cex = 0.8)
print(plot_cooks_distance)

cat("\n8. Normality Check of Errors (Q-Q Plot):\n")
qq_norm_plot <- qqnorm(residuals(model), main="Normal Q-Q Plot")
qqline(residuals(model))
print(qq_norm_plot)

cat("\n8. Shapiro-Wilk Normality Test Result:\n")
shapiro_test_result <- shapiro.test(residuals(model))
print(shapiro_test_result)

cat("\n9. Heteroscedasticity Check (Residuals vs. Fitted Values Plot):\n")
residual_plot <- plot(fitted(model), residuals(model), main = "Residuals vs. Fitted Values", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

print(residual_plot)

cat("\n8. Normality Check of Errors (Q-Q Plot):\n")
qq_norm_plot <- qqnorm(residuals(model), main="Normal Q-Q Plot")
qqline(residuals(model))
print(qq_norm_plot)

cat("\n8. Shapiro-Wilk Normality Test Result:\n")
shapiro_test_result <- shapiro.test(residuals(model))
print(shapiro_test_result)

install.packages("lmtest")
library(lmtest)

cat("\n9. Heteroscedasticity Check (Residuals vs. Fitted Values Plot):\n")
residual_plot <- plot(fitted(model), residuals(model), main = "Residuals vs. Fitted Values", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at zero
print(residual_plot)

cat("\n9. Breusch-Pagan Heteroscedasticity Test Result:\n")
bp_test_result <- bptest(model)
print(bp_test_result)

cat("\n10. RMSE Calculation:\n")
train_predictions <- predict(model, newdata = train_data)
train_rmse <- sqrt(mean((train_data$Price - train_predictions)^2))
cat("Training data set RMSE:", train_rmse, "\n")

test_predictions <- predict(model, newdata = test_data)
test_rmse <- sqrt(mean((test_data$Price - test_predictions)^2))
cat("Test data set RMSE:", test_rmse, "\n")