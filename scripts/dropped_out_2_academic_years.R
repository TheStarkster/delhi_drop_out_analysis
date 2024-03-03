# Load the ggplot2 package
library(ggplot2)

# Data Preparation
attendance_data <- data.frame(
  Year = c("Academic Year 22-23", "Academic Year 23-24", "Dropped Out"),
  Students = c(1450933, 1444501, 6432),
  Fill = c("Total", "Total", "Dropped Out")
)

# Plot
ggplot(attendance_data, aes(x = Year, y = Students, fill = Fill)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Total" = "#2ecc71", "Dropped Out" = "red")) +
  geom_text(aes(label = Students), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Student Attendance Records Comparison",
       x = "Academic Year",
       y = "Number of Students",
       fill = "Category")