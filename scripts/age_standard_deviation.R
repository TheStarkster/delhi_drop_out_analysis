library(dplyr)
library(ggplot2)
library(lubridate)


students <- read.csv("dropped_out_school_students.csv")


students$dob <- dmy(students$dob) # Convert dob to Date format assuming DD/MM/YYYY
students$age <- as.integer(interval(start = students$dob, end = Sys.Date()) / years(1)) # Calculate age


std_dev_age <- sd(students$age, na.rm = TRUE)
print(std_dev_age)


ggplot(students, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "#2ecc71", color = "#27ae60") +  # Set fill color using hexadecimal
  geom_vline(aes(xintercept = mean(age, na.rm = TRUE)),
             color = "#2c3e50", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean(age, na.rm = TRUE) + std_dev_age),
             color = "blue", linetype = "dotted", size = 1) +
  geom_vline(aes(xintercept = mean(age, na.rm = TRUE) - std_dev_age),
             color = "blue", linetype = "dotted", size = 1) +
  labs(title = "Age Distribution of Dropped Out Students",
       x = "Age",
       y = "Frequency") +
  theme_minimal()




mean_age <- mean(students$age, na.rm = TRUE)
median_age <- median(students$age, na.rm = TRUE)
std_dev_age <- sd(students$age, na.rm = TRUE)
min_age <- min(students$age, na.rm = TRUE)
max_age <- max(students$age, na.rm = TRUE)



summary_description <- paste("The analysis of dropped out students reveals an average age of", round(mean_age, 2),
                             "years with a median of", median_age, "years. The ages range from", min_age, "to", max_age,
                             "years, indicating a wide variety of ages among the students. The standard deviation of the ages is",
                             round(std_dev_age, 2), "years, suggesting that the age distribution", 
                             "has a variability of", round(std_dev_age, 2), "years from the mean. This information is crucial for understanding the demographic profile of students who have dropped out.")
print(summary_description)