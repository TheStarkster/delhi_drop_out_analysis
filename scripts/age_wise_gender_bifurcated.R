library(ggplot2)
library(dplyr)

# Assuming your dataframe is named 'drop_out_data' and you have 'age' and 'gender' columns
drop_out_data <- read.csv("data/sub_set_drop_out_student.csv")

# Filter out genders other than Male and Female
drop_out_data <- drop_out_data %>%
  filter(gender %in% c("Male", "Female"))

# Count the number of students per age and gender
age_gender_counts <- drop_out_data %>%
  group_by(age, gender) %>%
  summarise(count = n()) %>%
  ungroup()

# Get total counts per age to determine the top 16 ages
total_counts_per_age <- age_gender_counts %>%
  group_by(age) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  head(16)  # Select top 16 ages

# Filter the original dataset to include only top 16 ages
age_gender_counts <- age_gender_counts %>%
  filter(age %in% total_counts_per_age$age)

# Make female counts negative for the bi-directional chart
age_gender_counts <- age_gender_counts %>%
  mutate(count = ifelse(gender == "Female", -count, count))

age_gender_counts <- age_gender_counts %>%
  mutate(hjust_value = ifelse(count > 0, 
                              ifelse(abs(count) < 1300, -0.1, 0.5), 
                              ifelse(abs(count) < 1300, 1.1, 0.5)))

# Create the plot with dynamic label adjustment for both sides
ggplot_object <- ggplot(age_gender_counts, aes(x = reorder(as.factor(age), total_counts_per_age$total_count[match(age, total_counts_per_age$age)]), y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = abs(count), hjust = hjust_value), 
             vjust = 0.5,
             color = "black", fill = "white", size = 3) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent", colour = "grey"),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Age",
       y = "Number of Dropouts") +
  scale_fill_manual(values = c("Female" = "#12CBC4", "Male" = "#1289A7"))

print(ggplot_object)
