library(ggplot2)
library(dplyr)
library(RColorBrewer)


drop_out_data <- read.csv("data/sub_set_drop_out_student.csv")

# Correcting the disability type values
drop_out_data$disability_type <- ifelse(drop_out_data$disability_type == "Intellectual Disabil", 
                                        "Intellectual Disability", drop_out_data$disability_type)

# Filter out rows with null, blank, or "N/A" values in disability_type and count the number of students per disability type
disability_counts <- drop_out_data %>%
  filter(!is.na(disability_type), disability_type != "", disability_type != "N/A") %>%
  group_by(disability_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Create the plot
ggplot(disability_counts, aes(x = reorder(disability_type, count), y = count, fill = disability_type)) +
  geom_bar(stat = "identity", color = "white", fill = "#1289A7") +
  theme_minimal() +
  labs(title = "Distribution of Dropouts by Disability Type",
       x = "Disability Type",
       y = "Number of Dropouts") +
  coord_flip()  # Flip coordinates to make it horizontal






# Total records with a disability
total_disability_records <- sum(disability_counts$count)
# Total number of records in the dataset
total_records <- nrow(drop_out_data)

# Summary statement
if(nrow(disability_counts) > 0) {
  most_common_disability <- disability_counts$disability_type[1]
  most_common_count <- disability_counts$count[1]
  most_common_percentage <- round((most_common_count / total_disability_records) * 100, 2)
  
  disability_records_percentage <- round((total_disability_records / total_records) * 100, 2)
  
  summary_statement <- paste("Out of", total_records, "students, there are", total_disability_records, 
                             "records with a disability, accounting for", disability_records_percentage, "% of the total.",
                             "The most common disability type is", most_common_disability, "with", most_common_count, 
                             "dropouts, constituting", most_common_percentage, "% of the disability cases.")
} else {
  summary_statement <- "No data available for dropouts by disability type."
}

# Print the summary statement
print(summary_statement)