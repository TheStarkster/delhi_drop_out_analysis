library(ggplot2)
library(dplyr)

# Assuming your dataframe is named 'drop_out_data' and you have an 'age' column
drop_out_data <- read.csv("data/sub_set_drop_out_student.csv")

# Count the number of students for each age and arrange in descending order
age_counts <- drop_out_data %>%
  group_by(age) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(16)

# Create the plot
ggplot(age_counts, aes(x = reorder(as.factor(age), -count), y = count)) +
  geom_bar(stat = "identity", fill = "#1289A7") +
  theme_minimal() +
  labs(title = "Age-wise Distribution of Student Dropouts",
       x = "Age",
       y = "Number of Dropouts")