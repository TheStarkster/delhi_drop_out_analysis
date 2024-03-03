library(dplyr)
library(ggplot2)

data <- read.csv("dropped_out_school_students.csv", stringsAsFactors = FALSE)

data$dob <- as.Date(data$dob, format = "%d/%m/%Y")
current_year <- as.numeric(format(Sys.Date(), "%Y"))
data$age <- current_year - as.numeric(format(data$dob, "%Y"))
data$age <- data$age - ifelse(format(Sys.Date(), "%m-%d") < format(data$dob, "%m-%d"), 1, 0)

data$age_group <- cut(data$age, 
                      breaks = c(0, 5, 10, 15, 20, 25, Inf), 
                      labels = c("0-5", "6-10", "11-15", "16-20", "21-25", "26+"),
                      right = FALSE)

# Adjusted geom_bar to set fill color to "#2ecc71"
ggplot(data, aes(x = age_group)) + 
  geom_bar(fill = "#2ecc71", show.legend = FALSE) +  # Set fill color here
  labs(title = "Count of Dropped-Out Students by Age Group",
       x = "Age Group",
       y = "Count of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))