install.packages("dplyr")    
install.packages("openxlsx")
library(dplyr)
library(openxlsx)
Premiums <- read.csv("Premiums.csv", stringsAsFactors = FALSE)
str(Premiums)   
head(Premiums) 
num_rows <- nrow(Premiums)
num_cols <- ncol(Premiums)

print(paste("Number of rows:", num_rows))
print(paste("Number of columns:", num_cols))
head(Premiums, 10)  
tail(Premiums, 5) 
summary(Premiums)
Premium_sorted <- Premiums %>% arrange(desc(Premium_Amount))
colnames(Premiums)
Premium_sorted <- Premiums %>% arrange(desc(Premium))
top5_policies <- head(Premium_sorted, 5)
bottom5_policies <- tail(Premium_sorted, 5)
print("Top 5 Policies by Premium Amount:")
print(top5_policies)
print("Bottom 5 Policies by Premium Amount:")
print(bottom5_policies)
sum_assured_by_region <- Premiums %>%
  group_by(Region) %>%
  summarise(Total_Sum_Assured = sum(Sum_Assured, na.rm = TRUE))

print(sum_assured_by_region)
sum_assured_by_region <- Premiums %>%
  group_by(REGION) %>%
  summarise(Total_Sum_Assured = sum(Sum_Assured, na.rm = TRUE))
print(sum_assured_by_region)
subset_data <- Premiums %>%
  filter(Plan == "Asia Standard Plan" & Sum_Assured <= 50000) %>%
  select(Policy_No,ZONE_NAME,Plan, Sum_Assured)

print("Subset Data:")
print(subset_data)
Premiums %>%
  filter(Plan == "Asia Standard Plan" & Sum_Assured <= 50000) %>%
  select(POLICY_NO, ZONE_NAME, Plan, Sum_Assured)
print("Subset Data:")
print(subset_data)
subset_data <- Premiums %>%
  filter(Plan == "Asia Standard Plan" & Sum_Assured <= 50000) %>%
  select(POLICY_NO, ZONE_NAME, Plan, Sum_Assured)
print("Subset Data:")
print(subset_data)
