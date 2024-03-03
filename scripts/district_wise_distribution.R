library(ggplot2)
library(dplyr)

drop_out_data <- read.csv("data/sub_set_drop_out_student.csv")

# Filter out transgender and keep only Male and Female
drop_out_data <- drop_out_data %>%
  filter(gender %in% c("Male", "Female"))

# Count the number of students per district and gender
district_gender_counts <- drop_out_data %>%
  group_by(district_name, gender) %>%
  summarise(count = n()) %>%
  ungroup()

# Get total counts per district to determine the top 8 districts
total_counts_per_district <- district_gender_counts %>%
  group_by(district_name) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  head(8)  # Select top 8 districts

# Filter the original dataset to include only top 8 districts
district_gender_counts <- district_gender_counts %>%
  filter(district_name %in% total_counts_per_district$district_name)

# Make female counts negative for the bi-directional chart
district_gender_counts <- district_gender_counts %>%
  mutate(count = ifelse(gender == "Female", -count, count))

# Create the plot
ggplot_object <- ggplot(district_gender_counts, aes(x = reorder(district_name, total_counts_per_district$total_count[match(district_name, total_counts_per_district$district_name)]), y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = abs(count)), position = position_nudge(y = ifelse(district_gender_counts$count > 0, -0.5, 0.5)), 
             hjust = ifelse(district_gender_counts$count > 0, 0.5, 0.5), vjust = 0.5,
             color = "black", fill = "white", label.size = 0.2) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent", colour = "grey"),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "District",
       y = "Number of Dropouts") +
  scale_fill_manual(values = c("Female" = "#12CBC4", "Male" = "#1289A7"))  # Set colors for female and male

# Display the plot
print(ggplot_object)




summary_string <- paste("Among the top 8 districts, the highest number of dropouts is in", district_counts$district_name[1],
                        "with", district_counts$count[1], "dropouts. These top 8 districts alone account for",
                        sum(district_counts$count), "dropouts.", sep = " ")

summary_string