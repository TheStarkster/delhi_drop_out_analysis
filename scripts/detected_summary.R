drop_out_data <- read.csv("data/sub_set_drop_out_student.csv")

total_records <- nrow(drop_out_data)

# Number of students detected by the Early Warning System
detected_count <- sum(drop_out_data$is_detected, na.rm = TRUE)

# Number of students not detected
not_detected_count <- total_records - detected_count

# Number of detected students with a recorded reason for absence
reason_for_absence_count <- sum(drop_out_data$is_detected & !is.na(drop_out_data$reason_for_absence), na.rm = TRUE)

# Percentage of students detected
detected_percentage <- (detected_count / total_records) * 100

# Percentage of students not detected
not_detected_percentage <- (not_detected_count / total_records) * 100

# Creating the summary statement with the added information
summary_statement <- paste("Out of a total of", total_records, "students,",
                           detected_count, "(",
                           round(detected_percentage, 2), "%) were detected by the Early Warning System.",
                           "Among the detected students,", reason_for_absence_count, 
                           "had a recorded reason for absence, indicating follow-up calls were made.",
                           "Additionally,", not_detected_count, "(",
                           round(not_detected_percentage, 2), "%) were not detected.")

summary_statement