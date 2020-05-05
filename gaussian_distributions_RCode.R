#####################################################
#### Simulated Dataset 3: Gaussian Distributions ####
#####################################################

library(mvtnorm)
library(dbscan)
library(kernlab)
library(matrixStats)

set.seed(120)
# Simulating two overlapping clusters from two Gaussian distributions.
meanVec1 <- c(5,7)
sigmaMatrix1 <- matrix(c(7, 2.5, 2.5, 5), nrow=2)
gaussianSimulation1 <- rmvnorm(150, mean = meanVec1, sigma = sigmaMatrix1)

meanVec2 <- c(-1,0)
sigmaMatrix2 <- matrix(c(6.5, -4.5, -4.5, 8), nrow=2)
gaussianSimulation2 <- rmvnorm(150, mean = meanVec2, sigma = sigmaMatrix2) 

combinedData <- as.data.frame(rbind(gaussianSimulation1, gaussianSimulation2))
combinedData[,3] <- c(rep(1,150), rep(2,150))
colnames(combinedData) <- c('x', 'y', 'cluster')

# Plot of simulated data
plot(combinedData[,1:2], col = combinedData[,3],  xlim = c(-15, 15), ylim = c(-10,20), pch = 20)

# Finding optimal eps value for dbscan
knn_matrix <- kNN(combinedData[,1:2],4)
knn_matrix_new <- sort(knn_matrix$dist[,4])
plot(order(knn_matrix_new),knn_matrix_new, main="Optimized Eps using KNNS", xaxt = 'n')
axis(1, at = seq(0, 300, by = 10))

# The optimal eps is approximately 1.3
knn_matrix_new[260]


# We are calculating the run time of each algorithm on overlapping gaussian clusters.
start_time_spec <- Sys.time()
spec_sim2 <- specc(as.matrix(combinedData)[,1:2], centers = 2)
end_time_spec <- Sys.time()

start_time_kmeans <- Sys.time()
kmean_sim2 <- kmeans(combinedData[,1:2], centers = 2, nstart = 50)
end_time_kmeans <- Sys.time()

start_time_dbscan <- Sys.time()
dbscan_sim2 <- dbscan::dbscan(combinedData[,1:2], eps = 1.3, minPts = 4)
end_time_dbscan <- Sys.time()

sim2_timed_matrix <- matrix(0,1,3)
sim2_timed_matrix[1,1] <- round(end_time_spec - start_time_spec, digits = 4)
sim2_timed_matrix[1,2] <- round(end_time_kmeans - start_time_kmeans, digits = 4)
sim2_timed_matrix[1,3] <- round(end_time_dbscan - start_time_dbscan, digits = 4)
colnames(sim2_timed_matrix) <- c("spectral", 'kmeans', 'dbscan')

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
  meanVec1 <- c(5,7)
  sigmaMatrix1 <- matrix(c(7, 2.5, 2.5, 5), nrow=2)
  gaussianSimulation1 <- rmvnorm(150, mean = meanVec1, sigma = sigmaMatrix1)
  
  meanVec2 <- c(-1,0)
  sigmaMatrix2 <- matrix(c(6.5, -4.5, -4.5, 8), nrow=2)
  gaussianSimulation2 <- rmvnorm(150, mean = meanVec2, sigma = sigmaMatrix2) 
  
  combinedData <- as.data.frame(rbind(gaussianSimulation1, gaussianSimulation2))
  combinedData[,3] <- c(rep(1,150), rep(2,150))
  colnames(combinedData) <- c('x', 'y', 'cluster')
  
  spec_sim2 <- specc(as.matrix(combinedData)[,1:2], centers = 2)
  kmean_sim2 <- kmeans(combinedData[,1:2], centers = 2, nstart = 50)
  dbscan_sim2 <- dbscan::dbscan(combinedData[,1:2], eps = 1.3, minPts = 4)
  
  ARI_matrix[i,1] <- ARI(combinedData$cluster, spec_sim2)
  ARI_matrix[i,2] <- ARI(combinedData$cluster, kmean_sim2$cluster)
  ARI_matrix[i,3] <- ARI(combinedData$cluster, dbscan_sim2$cluster)
  
  JAC_matrix[i,1] <- extCriteria(as.integer(combinedData$cluster), as.integer(spec_sim2), "Jaccard")$jaccard
  JAC_matrix[i,2] <- extCriteria(as.integer(combinedData$cluster), kmean_sim2$cluster, "Jaccard")$jaccard
  JAC_matrix[i,3] <- extCriteria(as.integer(combinedData$cluster), dbscan_sim2$cluster, "Jaccard")$jaccard
  
  FM_matrix[i,1] <- extCriteria(as.integer(combinedData$cluster), as.integer(spec_sim2), "Folkes")$folkes_mallows
  FM_matrix[i,2] <- extCriteria(as.integer(combinedData$cluster), kmean_sim2$cluster, "Folkes")$folkes_mallows
  FM_matrix[i,3] <- extCriteria(as.integer(combinedData$cluster), dbscan_sim2$cluster, "Folkes")$folkes_mallows
  
  print(i)
}

gaussian_results <- round(rbind(colMeans(ARI_matrix), colSds(ARI_matrix),
                                   colMeans(JAC_matrix),colSds(JAC_matrix),
                                   colMeans(FM_matrix),colSds(FM_matrix)), 4)
rownames(gaussian_results) <- c('ARI Mean', 'ARI Std Dev', 
                                   'JAC Mean', 'JAC Std Dev', 
                                   'FM Mean', 'FM Std Dev')


# This is the final table with the mean and std deviations of each index for all 
# three clustering algorithms.
gaussian_results



