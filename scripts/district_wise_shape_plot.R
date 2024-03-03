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

# Prepare your dropout data
# Convert district_name to lowercase and rename "north west a" to "shahadra"
drop_out_data <- drop_out_data %>%
  mutate(district_name = ifelse(tolower(district_name) == "north west a", "shahadra", tolower(district_name)))

district_counts <- drop_out_data %>%
  group_by(district_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Merge the spatial data with the dropout data using a left join
delhi_map_data <- merge(x = delhi_districts, y = district_counts, 
                        by.x = 'District', by.y = 'district_name', 
                        all.x = TRUE)

# Replace NA with 0 for districts without dropout records
delhi_map_data$count[is.na(delhi_map_data$count)] <- 0

# Define a threshold for high dropout rates
high_dropout_threshold <- 5000  # Adjust this value based on your data and requirements

# Calculate centroids of districts
delhi_map_data$centroid <- st_centroid(delhi_map_data$geometry)

# Create a data frame for text labels
label_data <- subset(delhi_map_data, count >= high_dropout_threshold)
label_data$lon <- st_coordinates(label_data$centroid)[,1]
label_data$lat <- st_coordinates(label_data$centroid)[,2]

# Plot the data with district labels for high dropout areas
ggplot(data = delhi_map_data) +
  geom_sf(aes(fill = count)) +
  geom_text(data = label_data, aes(x = lon, y = lat, label = District), 
            color = "black", size = 3, check_overlap = TRUE) +
  scale_fill_gradient(low = "lightblue", high = "red", na.value = "grey") +
  labs(title = 'District-wise Dropout Distribution in Delhi', fill = 'Dropout Count') +
  theme_minimal()
