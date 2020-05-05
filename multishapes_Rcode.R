#####################################################
##### Simulated Dataset 1: Multishapes Data #########
#####################################################

library(factoextra)
library(dbscan)
library(kernlab)

# Original multishapes dataset from factoextra package
multi_data <- factoextra::multishapes
multi_data_obs <- as.matrix(multishapes[,1:2])

# Finding optimal eps value for dbscan
knn_matrix <- kNN(multi_data_obs,4)
knn_matrix_new <- sort(knn_matrix$dist[,4])
plot(order(knn_matrix_new),knn_matrix_new,main="Optimized Eps using KNNS", xaxt = 'n')
axis(1, at = seq(0, 1100, by = 20))

# The optimal eps is approximately 0.15
knn_matrix_new[1030]           

# We are calculating the run time of each algorithm on multishapes data.
start_time_spec <- Sys.time()
specc_multishapes <- specc(as.matrix(multi_data_obs), centers = 6)
end_time_spec <- Sys.time()

start_time_kmeans <- Sys.time()
kmean_multishapes <- kmeans(multi_data_obs, centers = 6, nstart = 50)
end_time_kmeans <- Sys.time()

start_time_dbscan <- Sys.time()
dbscan_multishapes <- dbscan::dbscan(multi_data_obs, eps = 0.15, minPts = 4)
end_time_dbscan <- Sys.time()

# Creating a table with run time results
multishapes_timed_matrix <- matrix(0,1,3)
multishapes_timed_matrix[1,1] <- round(end_time_spec - start_time_spec, digits = 4)
multishapes_timed_matrix[1,2] <- round(end_time_kmeans - start_time_kmeans, digits = 4)
multishapes_timed_matrix[1,3] <- round(end_time_dbscan - start_time_dbscan, digits = 4)
colnames(multishapes_timed_matrix) <- c("Spectral", "K-means", "DBSCAN")
multishapes_timed_matrix

# Since multishapes cannot be simulated repeatedly, we will
# calculate the three external indices using the original dataset.

# Creating a matrix of ARI indices 
ARI_matrix <- matrix(0,1,3)
ARI_matrix[1,1] <- ARI(multi_data$shape, specc_multishapes)
ARI_matrix[1,2] <- ARI(multi_data$shape, kmean_multishapes$cluster)
ARI_matrix[1,3] <- ARI(multi_data$shape, dbscan_multishapes$cluster)
colnames(ARI_matrix) <- c("Spectral", "K-means", "DBSCAN")

# Creating a matrix of JAC indices 
JAC_matrix <- matrix(0,1,3)
JAC_matrix[1,1] <- extCriteria(as.integer(multi_data$shape), as.integer(specc_multishapes), "Jaccard")$jaccard
JAC_matrix[1,2] <- extCriteria(as.integer(multi_data$shape), kmean_multishapes$cluster, "Jaccard")$jaccard
JAC_matrix[1,3] <- extCriteria(as.integer(multi_data$shape), dbscan_multishapes$cluster, "Jaccard")$jaccard
colnames(JAC_matrix) <- c("Spectral", "K-means", "DBSCAN")

# Creating a matrix of FM indices 
FM_matrix <- matrix(0,1,3)
FM_matrix[1,1] <- extCriteria(as.integer(multi_data$shape), as.integer(specc_multishapes), "Folkes")$folkes_mallows
FM_matrix[1,2] <- extCriteria(as.integer(multi_data$shape), kmean_multishapes$cluster, "Folkes")$folkes_mallows
FM_matrix[1,3] <- extCriteria(as.integer(multi_data$shape), dbscan_multishapes$cluster, "Folkes")$folkes_mallows
colnames(FM_matrix) <- c("Spectral", "K-means", "DBSCAN")

# Final table with all three indices
multishapes_results <- round(rbind(ARI_matrix, JAC_matrix, FM_matrix), 4)
rownames(multishapes_results) <- c('ARI', 'JAC', 'FM')


