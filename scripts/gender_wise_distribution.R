library(ggplot2)
library(dplyr)
library(RColorBrewer)


drop_out_data <- read.csv("data/sub_set_drop_out_student.csv")

# Filter out the 'Transgender' category and count the number of students by gender
gender_counts <- drop_out_data %>%
  filter(gender %in% c("Male", "Female")) %>%
  group_by(gender) %>%
  summarise(count = n())

# Calculate percentages
gender_counts$percentage <- (gender_counts$count / sum(gender_counts$count)) * 100

# Define specific colors for Male and Female
colors <- c("Male" = "#1289A7", "Female" = "#81e6e2")

# Create the pie chart with percentages
ggplot(gender_counts, aes(x = "", y = count, fill = gender)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = colors) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5))



# Create a summary statement
summary_statement <- paste("In the academic year 2023-2024, of the students who dropped out,",
                           round(gender_counts$percentage[gender_counts$gender == "Male"], 1),
                           "% were male and",
                           round(gender_counts$percentage[gender_counts$gender == "Female"], 1),
                           "% were female.")

summary_statement
