Premiums <- read.csv("Premiums.csv")
Claims <- read.csv("Claims.csv")
merged_data <- merge(Premiums, Claims, by = "POLICY_NO")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("RColorBrewer")
library(dplyr)
library(ggplot2)
library(RColorBrewer)
mean_premium_zone <- merged_data %>%
  group_by(ZONE_NAME) %>%
  summarise(mean_premium = mean(Premium, na.rm = TRUE))
ggplot(mean_premium_zone, aes(x = ZONE_NAME, y = mean_premium, fill = ZONE_NAME)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +  # Set color palette from Color Brewer
  labs(title = "Mean Premium by Zone", x = "Zone", y = "Mean Premium") +
  theme_minimal()
ggplot(merged_data, aes(x = ZONE_NAME, fill = Sub_Plan)) +
  geom_bar(aes(weight = Premium), position = "stack") +
  scale_fill_brewer(palette = "Paired") +  # Set color palette from Color Brewer
  labs(title = "Stacked Bar Chart of Premium Amount by Zone and Sub Plan", x = "Zone", y = "Premium Amount") +
  theme_minimal()
heatmap_data <- merged_data %>%
  group_by(Plan, ZONE_NAME) %>%
  summarise(avg_premium = mean(Premium, na.rm = TRUE))
ggplot(heatmap_data, aes(x = Plan, y = ZONE_NAME, fill = avg_premium)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +  # Color gradient for heatmap
  labs(title = "Heatmap of Average Premium by Plan and Zone", x = "Plan", y = "Zone") +
  theme_minimal()
pie_data <- merged_data %>%
  group_by(Sub_Plan) %>%
  summarise(total_premium = sum(Premium, na.rm = TRUE))
ggplot(pie_data, aes(x = "", y = total_premium, fill = Sub_Plan)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Convert to polar coordinates for a pie chart
  scale_fill_brewer(palette = "Dark2") +  # Color palette from Color Brewer
  labs(title = "Premium Amount Distribution by Sub Plan") +
  theme_void()
