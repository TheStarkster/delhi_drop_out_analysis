library(tidyverse)
dropout_data <- read_csv("dropped_out_school_students.csv")


dropout_summary <- dropout_data %>%
  group_by(district_name) %>%
  summarise(dropouts = n())


ggplot(dropout_summary, aes(x = reorder(district_name, dropouts), y = dropouts)) +
  geom_bar(stat = "identity", fill = "#2ecc71") +
  theme_minimal() +
  labs(title = "Dropouts by District",
       x = "District Name",
       y = "Number of Dropouts") +
  coord_flip() # This flips the axes to make the labels readable
