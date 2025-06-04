birth_weight_data <- read.csv("BIRTH WEIGHT_csv.csv")
head(birth_weight_data)
table(birth_weight_data$LOW, birth_weight_data$AGE)
table(birth_weight_data$LOW, birth_weight_data$LWT)
table(birth_weight_data$LOW, birth_weight_data$RACE)
table(birth_weight_data$LOW, birth_weight_data$SMOKE)
table(birth_weight_data$LOW, birth_weight_data$PTL)
table(birth_weight_data$LOW, birth_weight_data$HT)
table(birth_weight_data$LOW, birth_weight_data$UI)
table(birth_weight_data$LOW, birth_weight_data$FTV)
independent_vars <- names(birth_weight_data)[-which(names(birth_weight_data) == "LOW")]
for (var in independent_vars) {
  print(paste("Cross-tabulation of LOW and", var))
  print(table(birth_weight_data$LOW, birth_weight_data[[var]]))
  cat("\n")
}

model <- glm(LOW ~ AGE + LWT + factor(RACE) + SMOKE + PTL + HT + UI + FTV,
             data = birth_weight_data,
             family = binomial(link = "logit"))

summary(model)
predicted_probabilities <- predict(model, type = "response")


predicted_low_0.4 <- ifelse(predicted_probabilities > 0.4, 1, 0)
classification_table_0.4 <- table(birth_weight_data$LOW, predicted_low_0.4)
print("Classification Table (Cut-off = 0.4):")
print(classification_table_0.4)

predicted_low_0.3 <- ifelse(predicted_probabilities > 0.3, 1, 0)
classification_table_0.3 <- table(birth_weight_data$LOW, predicted_low_0.3)
print("Classification Table (Cut-off = 0.3):")
print(classification_table_0.3)

predicted_low_0.55 <- ifelse(predicted_probabilities > 0.55, 1, 0)
classification_table_0.55 <- table(birth_weight_data$LOW, predicted_low_0.55)
print("Classification Table (Cut-off = 0.55):")
print(classification_table_0.55)

calculate_metrics <- function(confusion_matrix) {
  TP <- confusion_matrix[2, 2]
  TN <- confusion_matrix[1, 1]
  FP <- confusion_matrix[1, 2]
  FN <- confusion_matrix[2, 1]
  
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  misclassification_rate <- (FP + FN) / sum(confusion_matrix)
  
  return(list(Sensitivity = sensitivity, Specificity = specificity, MisclassificationRate = misclassification_rate))
}

metrics_0.4 <- calculate_metrics(classification_table_0.4)
print("Metrics for Cut-off = 0.4:")
print(metrics_0.4)

metrics_0.3 <- calculate_metrics(classification_table_0.3)
print("Metrics for Cut-off = 0.3:")
print(metrics_0.3)

metrics_0.55 <- calculate_metrics(classification_table_0.55)
print("Metrics for Cut-off = 0.55:")
print(metrics_0.55)

install.packages("pROC")
library(pROC)

roc_curve <- roc(birth_weight_data$LOW, predicted_probabilities)
plot(roc_curve, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(0, 1, lty = 2) # Add a diagonal line for reference

auc_value <- auc(roc_curve)
print(paste("Area Under the Curve (AUC):", auc_value))
