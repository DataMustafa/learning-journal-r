mat <- matrix(sample(1:10, 12, replace = TRUE), nrow = 3, byrow = TRUE)
rownames(mat) <- c("A", "B", "C")
colnames(mat) <- c("Q", "W", "E", "R")
print(mat)
x <- 24
y <- "Hello World"
z <- 93.65

print(class(x))  # numeric
print(class(y))  # character
print(class(z))  # numeric

x_factor <- as.factor(x)
y_factor <- as.factor(y)
z_factor <- as.factor(z)

print(x_factor)
print(y_factor)
print(z_factor)
q <- 65.9836
sqrt_q <- round(sqrt(q), 3)
print(sqrt_q)
log_q <- log10(q) < 2
print(log_q)
x <- c("Intelligence", "Knowledge", "Wisdom", "Comprehension")
x_sub <- substr(x, 1, 4)
print(x_sub)
y <- "I am"
z <- "intelligent"
sentence <- paste(y, z)
print(sentence)
x_upper <- toupper(x)
print(x_upper)
a <- c(3,4,14,17,3,98,66,85,44)
result <- ifelse(a %% 3 == 0, "Yes", "No")
print(result)
b <- c(36,3,5,19,2,16,18,41,35,28,30,31)
for (num in b) {
  if (num < 30) {
    print(num)
  }
}
Date <- "01/30/18"
Date_new <- as.Date(Date, format = "%m/%d/%y")
print(Date_new)
day_of_week <- weekdays(Date_new)
month_of_date <- months(Date_new)
print(day_of_week)
print(month_of_date)
date_diff <- Sys.Date() - Date_new
print(date_diff)