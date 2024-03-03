library(dplyr)
library(ggplot2)

students_data <- read.csv("dropped_out_school_students.csv")

students_data[students_data$id == 20190553251, "class"] <- "VII"

students_data$dob <- as.Date(students_data$dob, format="%d/%m/%Y")
current_year <- as.numeric(format(Sys.Date(), "%Y"))
students_data$age <- current_year - as.numeric(format(students_data$dob, "%Y"))
# Assuming 'students_data' is your dataframe
students_data$class <- factor(students_data$class, 
                              levels = c("Nursery", "KG", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"),
                              ordered = TRUE)

ggplot(students_data, aes(x=age, y=class)) +
  geom_point(color="#2ecc71") +  # Specify the color of the points
  labs(title="Scatter Plot of Age vs. Class", 
       x="Age", 
       y="Class") +
  theme_minimal()  # Use a minimal theme for a cleaner look