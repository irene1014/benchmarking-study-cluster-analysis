#####################################################
######## Simulated Dataset 2: Spiral Data ###########
#####################################################

library(mlbench)
library(dbscan)
library(kernlab)
library(matrixStats)

set.seed(120)
spiralData <- mlbench.spirals(n = 400, cycles = 2.5, sd = 0.025)

# Finding optimal eps value for dbscan
knn_matrix <- kNN(spiralData$x,4)
knn_matrix_new <- sort(knn_matrix$dist[,4])
plot(order(knn_matrix_new),knn_matrix_new,main="Optimized Eps using KNNS", xaxt = 'n')
axis(1, at = seq(0, 400, by = 10))

# There is no obvious elbow point but we can approximate our optimal eps = 0.22
knn_matrix_new[255]

# We are calculating the run time of each algorithm on spiral data.
start_time_spec <- Sys.time()
spec_spirals <- specc(spiralData$x, centers = 2)
end_time_spec <- Sys.time()

start_time_kmeans <- Sys.time()
kmean_spirals <- kmeans(spiralData$x, centers = 2, nstart = 50)
end_time_kmeans <- Sys.time()

start_time_dbscan <- Sys.time()
dbscan_spirals <- dbscan::dbscan(spiralData$x, eps = 0.22, minPts = 4)
end_time_dbscan <- Sys.time()

# Creating a table with run time results
spiral_timed_matrix <- matrix(0,1,3)
spiral_timed_matrix[1,1] <- round(end_time_spec - start_time_spec, digits = 4)
spiral_timed_matrix[1,2] <- round(end_time_kmeans - start_time_kmeans, digits = 4)
spiral_timed_matrix[1,3] <- round(end_time_dbscan - start_time_dbscan, digits = 4)
colnames(spiral_timed_matrix) <- c("Spectral", "K-means", "DBSCAN")


# We can repeat the simulation 20 times to find the means and standard deviations
# of the three indices.

ARI_matrix <- matrix(0,20,3)
JAC_matrix <- matrix(0,20,3)
FM_matrix <- matrix(0,20,3)

colnames(ARI_matrix) <- c("spectral", "kmeans", "dbscan")
colnames(JAC_matrix) <- c("spectral", "kmeans", "dbscan")
colnames(FM_matrix) <- c("spectral", "kmeans", "dbscan")

set.seed(120)
for(i in 1:20)
{
  spiralData <- mlbench.spirals(n = 400, cycles = 2.5, sd = 0.025)
  spec_spirals <- specc(spiralData$x, centers = 2)
  kmean_spirals <- kmeans(spiralData$x, centers = 2, nstart = 50)
  dbscan_spirals <- dbscan::dbscan(spiralData$x, eps = 0.22, minPts = 4)
  
  ARI_matrix[i,1] <- ARI(spiralData$classes, spec_spirals)
  ARI_matrix[i,2] <- ARI(spiralData$classes, kmean_spirals$cluster)
  ARI_matrix[i,3] <- ARI(spiralData$classes, dbscan_spirals$cluster)
  
  JAC_matrix[i,1] <- extCriteria(as.integer(spiralData$classes), as.integer(spec_spirals), "Jaccard")$jaccard
  JAC_matrix[i,2] <- extCriteria(as.integer(spiralData$classes), kmean_spirals$cluster, "Jaccard")$jaccard
  JAC_matrix[i,3] <- extCriteria(as.integer(spiralData$classes), dbscan_spirals$cluster, "Jaccard")$jaccard
  
  FM_matrix[i,1] <- extCriteria(as.integer(spiralData$classes), as.integer(spec_spirals), "Folkes")$folkes_mallows
  FM_matrix[i,2] <- extCriteria(as.integer(spiralData$classes), kmean_spirals$cluster, "Folkes")$folkes_mallows
  FM_matrix[i,3] <- extCriteria(as.integer(spiralData$classes), dbscan_spirals$cluster, "Folkes")$folkes_mallows
  
  print(i)
}

spiral_results <- round(rbind(colMeans(ARI_matrix), colSds(ARI_matrix),
                              colMeans(JAC_matrix),colSds(JAC_matrix),
                              colMeans(FM_matrix),colSds(FM_matrix)), 4)

rownames(spiral_results) <- c('ARI Mean', 'ARI Std Dev', 
                              'JAC Mean', 'JAC Std Dev', 
                              'FM Mean', 'FM Std Dev')

# This is the final table with the mean and std deviations of each index for all 
# three clustering algorithms.
spiral_results


