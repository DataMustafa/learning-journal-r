pain_nprs <- read.csv("NPRS DATA.csv")
median_before <- median(pain_nprs$NPRS_before)
median_after <- median(pain_nprs$NPRS_after)
cat("Median NPRS Before Treatment:", median_before, "\n")
cat("Median NPRS After Treatment:", median_after, "\n")
group_a <- subset(pain_nprs, Group == "A")
t_test_a <- t.test(group_a$NPRS_before, group_a$NPRS_after, paired = TRUE)
cat("T-statistic for Group A:", t_test_a$statistic, "\n")
cat("P-value for Group A:", t_test_a$p.value, "\n")
if (t_test_a$p.value < 0.05) {
  cat("Post-treatment NPRS is significantly less for Group A\n")
} else {
  cat("No significant difference in Group A\n")
}
group_b <- subset(pain_nprs, Group == "B")
t_test_b <- t.test(group_b$NPRS_before, group_b$NPRS_after, paired = TRUE)
cat("T-statistic for Group B:", t_test_b$statistic, "\n")
cat("P-value for Group B:", t_test_b$p.value, "\n")
if (t_test_b$p.value < 0.05) {
  cat("Post-treatment NPRS is significantly less for Group B\n")
} else {
  cat("No significant difference in Group B\n")
}
group_a$change <- group_a$NPRS_before - group_a$NPRS_after
group_b$change <- group_b$NPRS_before - group_b$NPRS_after
t_test_change <- t.test(group_a$change, group_b$change)
cat("T-statistic for change:", t_test_change$statistic, "\n")
cat("P-value for change:", t_test_change$p.value, "\n")
if (t_test_change$p.value < 0.05) {
  cat("The change in NPRS is significantly different between Group A and Group B\n")
} else {
  cat("No significant difference in the change between groups\n")
}
install.packages("ggplot2")
library(ggplot2)
pain_nprs$change <- pain_nprs$NPRS_before - pain_nprs$NPRS_after
ggplot(pain_nprs, aes(x = Group, y = change)) +
  geom_boxplot() +
  ggtitle("Change in NPRS for Group A and Group B") +
  xlab("Group") +
  ylab("Change in NPRS")

