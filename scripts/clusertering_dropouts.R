# Load necessary libraries
library(tidyverse)
library(lubridate)
library(cluster) # For clustering
library(factoextra) # For visualization

# Read the dataset
df <- read.csv("dropped_out_school_students.csv")

# Exclude specified columns and convert dob to age
df <- df %>% select(-c(id, school_id, latitude, longitude)) %>%
  mutate(dob = dmy(dob), 
         age = as.integer(Sys.Date() %>% year() - dob %>% year()),
         class = as.numeric(factor(class, levels = unique(class)))) %>%
  select(-dob) # Remove dob after calculating age

# Convert all other relevant categorical variables to numeric
df$gender <- as.numeric(factor(df$gender))
df$shift <- as.numeric(factor(df$shift))
df$school_level <- as.numeric(factor(df$school_level))
df$district_name <- as.numeric(factor(df$district_name))
# Add more conversion as needed based on the actual data

# Normalize the data
df_scaled <- scale(df %>% select(age, class, gender, shift, school_level, district_name))

# Perform k-means Clustering
set.seed(123)
k <- 2
clusters <- kmeans(df_scaled, centers = k)

# Add cluster assignments to the dataframe
df$cluster <- clusters$cluster

# PCA for visualization
pca_result <- prcomp(df_scaled)

# Get the loadings (contribution of each variable to each PC)
# loadings <- pca_result$rotation
# 
# # View the loadings to understand the importance of each feature
# print(loadings)
# 
# # Optionally, visualize the importance of each feature on the first two principal components
# fviz_pca_var(pca_result, col.var = "contrib", 
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
#              repel = TRUE) # Avoid text overlapping



# df_pca <- as.data.frame(pca_result$x)
# df_pca$cluster <- as.factor(clusters$cluster)
# 
# ggplot(df_pca, aes(x = PC1, y = PC2, color = cluster)) +
#  geom_point(alpha = 0.5) +
#  theme_minimal() +
#  labs(title = "Cluster Analysis of Dropped Out Students",
#       x = "Principal Component 1",
#       y = "Principal Component 2",
#       color = "Cluster")


# Accessing PCA Loadings
loadings <- pca_result$rotation

# Viewing the loadings for PC1 and PC2
# print(loadings[, 1:2])


# View the loadings to understand the importance of each feature
print(loadings)

# Optionally, visualize the importance of each feature on the first two principal components
fviz_pca_var(pca_result, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Avoid text overlapping

