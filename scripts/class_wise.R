library(dplyr)
library(ggplot2)

data <- read.csv("dropped_out_school_students.csv", stringsAsFactors = FALSE)

data[data$id == 20190553251, "class"] <- "VII"

dropout_summary <- data %>%
  group_by(class) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


ggplot(dropout_summary, aes(x = reorder(class, -count), y = count)) +
  geom_bar(stat = "identity", fill = "#2ecc71") +
  theme_minimal() +
  labs(title = "Number of Dropouts per Class", x = "Class", y = "Number of Dropouts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))