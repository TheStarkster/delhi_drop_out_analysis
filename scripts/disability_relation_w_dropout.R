library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For dealing with dates and times
library(ggplot2)    # For creating visualizations

data <- read.csv("dropped_out_school_students.csv")

data[data$id == 20190553251, "class"] <- "VII"

# Convert dob to age
data$dob <- as.Date(data$dob, format="%d/%m/%Y")
data$age <- as.integer(lubridate::year(Sys.Date()) - lubridate::year(data$dob))

# Filter students with disabilities
data_disability <- data %>% filter(cwsn == "Yes")

# Ensure 'class' is an ordered factor
data_disability$class <- factor(data_disability$class, levels = c("Nursery", "KG", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"), ordered = TRUE)
data$cwsn_status <- ifelse(data$cwsn == "Yes", "Yes", ifelse(data$cwsn == "No", "No", "Blank"))


# class X, North-East / North-West, age 17-18, No, Boys School

ggplot(data, aes(x = age, fill = cwsn_status)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  labs(title = "Age Distribution of Dropouts by CWSN Status", x = "Age", y = "Frequency") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")


