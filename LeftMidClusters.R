# Load necessary libraries
library(readxl)
library(dplyr)
library(scales)
library(ggplot2)
library(cluster)
#install.packages("factoextra")
library(factoextra)

# Load the data
file_path <- "D:/Documents/Football Stats/LeftMid.xlsx"
df <- read_excel(file_path, sheet = "Worksheet", skip = 1)

df <- df %>% select(-PPRk)

# Remove duplicate columns
df <- df[, !duplicated(names(df))]

# Select only numeric columns for clustering
numeric_cols <- df %>% select(where(is.numeric))

# Check for missing values and handle them
numeric_cols <- numeric_cols %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Normalize the data
numeric_cols_normalized <- numeric_cols %>%
  mutate(across(everything(), ~ rescale(.)))

# Determine the optimal number of clusters using the Elbow method
fviz_nbclust(numeric_cols_normalized, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Number of Clusters")

# Set number of clusters (adjust based on the Elbow method)
num_clusters <- 5

# Run k-means clustering
set.seed(123) # For reproducibility
kmeans_result <- kmeans(numeric_cols_normalized, centers = num_clusters, nstart = 25)

# Assign cluster labels to players
df <- df %>%
  mutate(cluster = kmeans_result$cluster)

# Perform PCA for visualization
pca_result <- prcomp(numeric_cols_normalized, scale. = TRUE)

# Create a data frame with PCA results and cluster labels
pca_df <- as.data.frame(pca_result$x) %>%
  select(PC1, PC2) %>%
  mutate(Player = df$Player, cluster = df$cluster)

# Plot the clusters
ggplot(pca_df, aes(x = PC1, y = PC2, color = factor(cluster), label = Player)) +
  geom_point(size = 2) +
  geom_text(vjust = 1, hjust = 1, size = 3, check_overlap = TRUE) +
  labs(title = "Player Clusters",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_minimal()

# Evaluate the quality of the clustering with the silhouette score
silhouette_score <- silhouette(kmeans_result$cluster, dist(numeric_cols_normalized))
fviz_silhouette(silhouette_score) +
  labs(title = "Silhouette Plot for K-means Clustering")

# Print the average silhouette width
avg_silhouette <- mean(silhouette_score[, 3])
print(paste("Average silhouette width:", round(avg_silhouette, 2)))

# Print players in each cluster
clustered_players <- df %>% select(Player, cluster)
print("Players in each cluster:")
print(clustered_players %>% arrange(cluster))

# Filter players from Cluster 1
cluster_1_players <- df %>% filter(cluster == 5)

# Print players in Cluster 1
print("Players in Cluster 1:")
print(cluster_1_players %>% select(Player, cluster))

# Remove duplicate columns
df <- df[, !duplicated(names(df))]

# Select only numeric columns for clustering and key metrics
numeric_cols <- df %>% select(where(is.numeric))

# Check for missing values and handle them
numeric_cols <- numeric_cols %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Define key metrics
key_metrics <- c('PPA', 'Att', 'xA', 'PFT', 'TMid 3rd')

# Check if key metrics are present in the dataset
if (!all(key_metrics %in% names(numeric_cols))) {
  stop("Some key metrics are not present in the dataset.")
}

# Normalize the key metrics
normalized_metrics <- numeric_cols %>%
  select(all_of(key_metrics)) %>%
  mutate(across(everything(), ~ rescale(.))) 

# Create a normalized score (mean of normalized metrics)
df <- df %>%
  mutate(normalized_score = rowMeans(normalized_metrics, na.rm = TRUE))

# Run k-means clustering (assumes k-means clustering has already been done)
# df <- df %>%
#   mutate(cluster = kmeans_result$cluster)

# Filter players from Cluster 1
cluster_1_players <- df %>% filter(cluster == 5)

# Perform PCA for visualization on Cluster 1 players
# Normalize data for only Cluster 1 players
numeric_cols_cluster_1 <- cluster_1_players %>%
  select(where(is.numeric)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  select(where(~ sd(.) > 0)) # Remove constant columns

# Check if there are enough columns left for PCA
if (ncol(numeric_cols_cluster_1) < 2) {
  stop("Not enough variables left for PCA after removing constant columns.")
}

# Perform PCA
pca_result_cluster_1 <- prcomp(numeric_cols_cluster_1, scale. = TRUE)

# Extract loadings
loadings <- pca_result_cluster_1$rotation

# Print loadings for PC1 and PC2
print("Loadings for PC1:")
print(loadings[,1]) # Loadings for PC1

print("Loadings for PC2:")
print(loadings[,2]) # Loadings for PC2

# Print variance explained
explained_variance <- summary(pca_result_cluster_1)$importance[2,] # Proportion of variance
print("Variance Explained by PC1 and PC2:")
print(explained_variance)

# Create a data frame with PCA results
pca_df_cluster_1 <- as.data.frame(pca_result_cluster_1$x) %>%
  select(PC1, PC2) %>%
  mutate(Player = cluster_1_players$Player, 
         normalized_score = cluster_1_players$normalized_score)

# Plot the PCA results for Cluster 1 players
ggplot(pca_df_cluster_1, aes(x = PC1, y = PC2, color = normalized_score, label = Player)) +
  geom_point(size = 2) +
  geom_text(vjust = 1, hjust = 1, size = 3, check_overlap = TRUE) +
  scale_color_viridis_c() +
  labs(title = "PCA of Players in Cluster 1 with Normalized Score",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Normalized Score") +
  theme_minimal()

# Print players in Cluster 1 with their normalized scores
print("Players in Cluster 1 with their normalized scores:")
print(cluster_1_players %>% select(Player, cluster, normalized_score))

# Select only numeric columns for further clustering
numeric_cols_cluster_1 <- cluster_1_players %>%
  select(where(is.numeric)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  select(where(~ sd(.) > 0)) # Remove constant columns

# Check if there are enough columns left for clustering
if (ncol(numeric_cols_cluster_1) < 2) {
  stop("Not enough variables left for clustering after removing constant columns.")
}

# Normalize the data
numeric_cols_cluster_1_normalized <- numeric_cols_cluster_1 %>%
  mutate(across(everything(), ~ rescale(.)))

# Determine the optimal number of clusters using the Elbow method
wss <- function(k) {
  kmeans(numeric_cols_cluster_1_normalized, k, nstart = 30)$tot.withinss
}

# Compute and plot WSS for a range of cluster numbers
k.values <- 1:10
wss_values <- sapply(k.values, wss)

plot(k.values, wss_values, type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow Method for Optimal Number of Clusters")

# Set the number of clusters based on the Elbow or Silhouette method
num_clusters_cluster_1 <- 3 # Adjust based on your findings

# Run k-means clustering on Cluster 1 data
set.seed(123) # For reproducibility
kmeans_result_cluster_1 <- kmeans(numeric_cols_cluster_1_normalized, centers = num_clusters_cluster_1, nstart = 40)

# Assign new cluster labels to players in Cluster 1
cluster_1_players <- cluster_1_players %>%
  mutate(subcluster = kmeans_result_cluster_1$cluster)

# Perform PCA for visualization on the new clusters
pca_result_cluster_1 <- prcomp(numeric_cols_cluster_1_normalized, scale. = TRUE)

# Create a data frame with PCA results and cluster information
pca_df_cluster_1 <- as.data.frame(pca_result_cluster_1$x) %>%
  select(PC1, PC2) %>%
  mutate(Player = cluster_1_players$Player, 
         subcluster = cluster_1_players$subcluster)

# Plot the PCA results for the new clusters
ggplot(pca_df_cluster_1, aes(x = PC1, y = PC2, color = as.factor(subcluster), label = Player)) +
  geom_point(size = 2) +
  geom_text(vjust = 1, hjust = 1, size = 3, check_overlap = TRUE) +
  labs(title = "PCA of Players in Cluster 1 with Subclusters",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Subcluster") +
  theme_minimal()

# Print players in Cluster 1 with their new subcluster assignments
print("Players in Cluster 1 with their new subcluster assignments:")
print(cluster_1_players %>% select(Player, subcluster))

# Filter data for subcluster 2
subcluster_2_players <- cluster_1_players %>% filter(subcluster == 2)

# View the players in subcluster 2
print("Players in Subcluster 2:")
print(subcluster_2_players)

# Select numeric columns for subcluster 2 analysis
numeric_cols_subcluster_2 <- subcluster_2_players %>%
  select(where(is.numeric)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  select(where(~ sd(.) > 0)) # Remove constant columns if any

# Normalize the data for subcluster 2
numeric_cols_subcluster_2_normalized <- numeric_cols_subcluster_2 %>%
  mutate(across(everything(), ~ rescale(.)))

# Perform PCA
pca_result_subcluster_2 <- prcomp(numeric_cols_subcluster_2_normalized, scale. = TRUE)

# Create a data frame with PCA results
pca_df_subcluster_2 <- as.data.frame(pca_result_subcluster_2$x) %>%
  select(PC1, PC2) %>%
  mutate(Player = subcluster_2_players$Player,
         normalized_score = subcluster_2_players$normalized_score)


# Plot the PCA results for Subcluster 2
ggplot(pca_df_subcluster_2, aes(x = PC1, y = PC2, color = normalized_score, label = Player)) +
  geom_point(size = 3) +
  geom_text(vjust = 1, hjust = 1, size = 3, check_overlap = TRUE) +
  labs(title = "PCA of Players in Subcluster 2",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Normalized Score") +
  theme_minimal()

