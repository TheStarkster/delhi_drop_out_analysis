library(ggplot2)
library(dplyr)

drop_out_data <- read.csv("data/sub_set_drop_out_student.csv")

# Count the number of students per district, arrange in descending order, and select top 10
district_counts <- drop_out_data %>%
  group_by(district_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(8)  # Select top 10 districts

# Create the plot
ggplot(district_counts, aes(x = reorder(district_name, count), y = count)) +
  geom_bar(stat = "identity", fill = "#1289A7") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent", colour = "grey")) +
  labs(title = "Top 8 Districts by Dropout Count",
       x = "District",
       y = "Number of Dropouts") +
  coord_flip()  # Flip coordinates to make it horizontal



summary_string <- paste("Among the top 8 districts, the highest number of dropouts is in", district_counts$district_name[1],
                        "with", district_counts$count[1], "dropouts. These top 8 districts alone account for",
                        sum(district_counts$count), "dropouts.", sep = " ")

summary_string
