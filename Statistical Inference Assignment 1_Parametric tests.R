install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
pain_vas <- read.csv("VAS DATA.csv")
head(pain_vas)
group_a <- filter(pain_vas, Group == "A")
shapiro.test(group_a$VAS_before)
shapiro.test(group_a$VAS_after)
t.test(group_a$VAS_before, group_a$VAS_after, paired = TRUE, alternative = "greater")
wilcox.test(group_a$VAS_before, group_a$VAS_after, paired = TRUE, alternative = "greater")

group_b <- filter(pain_vas, Group == "B")
shapiro.test(group_b$VAS_before)
shapiro.test(group_b$VAS_after)
t.test(group_b$VAS_before, group_b$VAS_after, paired = TRUE, alternative = "greater")
wilcox.test(group_b$VAS_before, group_b$VAS_after, paired = TRUE, alternative = "greater")

pain_vas <- pain_vas %>%
  mutate(VAS_change = VAS_before - VAS_after)
shapiro.test(pain_vas$VAS_change[pain_vas$Group == "A"])
shapiro.test(pain_vas$VAS_change[pain_vas$Group == "B"])
t.test(VAS_change ~ Group, data = pain_vas, alternative = "greater")
wilcox.test(VAS_change ~ Group, data = pain_vas, alternative = "greater")
install.packages("ggplot2")
library(ggplot2)


ggplot(pain_vas, aes(x = Group, y = VAS_change, fill = Group)) +
  geom_boxplot() +
  labs(title = "Change in Pain Level by Group", y = "VAS Change (Before - After)") +
  theme_minimal()