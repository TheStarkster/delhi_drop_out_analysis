library(sf)
library(ggplot2)
library(dplyr)

delhi_districts <- st_read('assets/delhi_shape_files/DELHI_DISTRICT_BDY.shp')
drop_out_data <- read.csv("data/sub_set_drop_out_student.csv")

# Convert District to lowercase in delhi_districts
delhi_districts$District <- tolower(delhi_districts$District)

district_names <- unique(delhi_districts$District)

# Print the list of district names
print(district_names)

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


top_reasons <- drop_out_data %>%
  group_by(reason_for_absence) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:3) %>%
  pull(reason_for_absence)


# Assuming we are working with the first top reason
current_reason <- top_reasons[1]

# Filter data for this reason
filtered_data <- drop_out_data %>%
  filter(reason_for_absence == current_reason) %>%
  mutate(district_name = tolower(district_name))  # Ensure district names are in lowercase

# Count occurrences in each district
district_counts_current_reason <- filtered_data %>%
  group_by(district_name) %>%
  summarise(count = n()) %>%
  ungroup()

# Merge with spatial data
delhi_map_data_current_reason <- merge(x = delhi_districts, y = district_counts_current_reason, 
                                       by.x = 'District', by.y = 'district_name', 
                                       all.x = TRUE)

# Replace NA with 0 for districts without records
delhi_map_data_current_reason$count[is.na(delhi_map_data_current_reason$count)] <- 0

# Plotting
ggplot(data = delhi_map_data_current_reason) +
  geom_sf(aes(fill = count)) +
  labs(title = paste('District-wise Distribution for Reason:', current_reason),
       fill = 'Count') +
  scale_fill_gradient(low = "lightblue", high = "red", na.value = "grey") +
  theme_minimal()

