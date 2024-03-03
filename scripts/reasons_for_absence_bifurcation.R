library(ggplot2)
library(dplyr)

# Load the data
drop_out_data <- read.csv("data/sub_set_drop_out_student.csv")

# Remove records where reason_for_absence is null or blank
drop_out_data <- drop_out_data %>%
  filter(!is.na(reason_for_absence) & reason_for_absence != "")

# Rename specific reasons for absence
drop_out_data <- drop_out_data %>%
  mutate(reason_for_absence = case_when(
    reason_for_absence == "Moved back to Village/ different State" ~ "Out of Delhi",
    reason_for_absence == "Want to change school / already changed school" ~ "Want to change school",
    reason_for_absence == "No interest in studies/ not interested in going to school (Mental health)" ~ "Not interested in Studies",
    reason_for_absence == "Respondent is unaware of absenteeism" ~ "Unaware Absenteeism",
    reason_for_absence == "Denial of admission/ registration/ name struck out" ~ "Denial of admission",
    reason_for_absence == "Infrastructure/ Resource related" ~ "Infra Issue",
    reason_for_absence == "Denial of resources (book/uniform) / Did not receive subsidy" ~ "Denial of resources",
    reason_for_absence == "Sexual Offences or sexually inappropriate behaviour towards the student" ~ "POCSO",
    reason_for_absence == "Bullying/ Physical abuse" ~ "Bullying",
    reason_for_absence == "Respondent denied absenteeism" ~ "Absenteeism denied",
    TRUE ~ reason_for_absence
  ))

# Filter out genders other than Male and Female
drop_out_data <- drop_out_data %>%
  filter(gender %in% c("Male", "Female"))

# Count the number of students per reason_for_absence and gender
absence_gender_counts <- drop_out_data %>%
  group_by(reason_for_absence, gender) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ungroup()

# Get total counts per reason_for_absence to determine the top categories
total_counts_per_absence <- absence_gender_counts %>%
  group_by(reason_for_absence) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  head(21)  # Select top categories

# Filter the original dataset to include only top categories
absence_gender_counts <- absence_gender_counts %>%
  filter(reason_for_absence %in% total_counts_per_absence$reason_for_absence)

# Make female counts negative for the bi-directional chart
absence_gender_counts <- absence_gender_counts %>%
  mutate(count = ifelse(gender == "Female", -count, count),
         hjust_value = ifelse(count > 0, ifelse(abs(count) < 1300, -0.1, 0.5), ifelse(abs(count) < 1300, 1.1, 0.5)))

# Create the plot
ggplot_object <- ggplot(absence_gender_counts, aes(x = reorder(reason_for_absence, total_counts_per_absence$total_count[match(reason_for_absence, total_counts_per_absence$reason_for_absence)]), y = count, fill = gender)) +
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
  labs(x = "Reason for Absence",
       y = "Number of Dropouts") +
  scale_fill_manual(values = c("Female" = "#12CBC4", "Male" = "#1289A7"))

# Display the plot
print(ggplot_object)