#####################################################
#### Simulated Dataset 4: Mixed Distributions 1 #####
#####################################################

library(MixGHD)
library(ContaminatedMixt)
library(mvtnorm)
library(dbscan)
library(kernlab)
library(matrixStats)

set.seed(120)
# Simulating clusters from several mixed distributions
data1 = rGHD(n = 200, p = 2, mu = c(30,36), alpha = c(-3,2), sigma = matrix(c(1,2,2,1),nrow = 2),
             omega = 0.2, lambda = 1.2)
data2 = rMSGHD(n = 200, p= 2, mu = c(40,32), alpha = c(-3,-2), sigma = matrix(c(1,2,2,3),nrow = 2),
               omega = c(0.4,0.3), lambda = c(0.5,3))
data3 = rCN(n = 200, mu = c(80,60), Sigma = matrix(c(20,8,8,20), ncol = 2), alpha = 0.2, eta = 12)
data4 = rmvnorm(200, mean = c(-30,-15), sigma = matrix(c(100, -10, -10, 100), nrow=2)) + runif(200, min= -25, max = 25)

combinedData <- as.data.frame(rbind(data1, data2, data3, data4))
combinedData[,3] <- c(rep(1,200),  rep(2,200), rep(3,200), rep(4,200))
colnames(combinedData) <- c('x', 'y', 'cluster')

# Plot of simulated data
plot(combinedData[,1:2], col = combinedData[,3],  xlim = c(-190,130), ylim = c(-100,220), pch = 20)


# Finding optimal eps value for dbscan
knn_matrix <- kNN(combinedData[,1:2],4)
knn_matrix_new <- sort(knn_matrix$dist[,4])
plot(order(knn_matrix_new),knn_matrix_new, main="Optimized Eps using KNNS", xaxt="n")
axis(1, at = seq(0, 800, by = 10))

# The optimal eps is approximately 15.0
knn_matrix_new[770]


# We are calculating the run time of each algorithm on overlapping mixed clusters.
start_time_spec <- Sys.time()
spec_sim1 <- specc(as.matrix(combinedData)[,1:2], centers = 4)
end_time_spec <- Sys.time()

start_time_kmeans <- Sys.time()
kmean_sim1 <- kmeans(combinedData[,1:2], centers = 4, nstart = 50)
end_time_kmeans <- Sys.time()

start_time_dbscan <- Sys.time()
dbscan_sim1 <- dbscan::dbscan(combinedData[,1:2], eps = 15.0, minPts = 4)
end_time_dbscan <- Sys.time()

sim1_timed_matrix <- matrix(0,1,3)
sim1_timed_matrix[1,1] <- round(end_time_spec - start_time_spec, digits = 4)
sim1_timed_matrix[1,2] <- round(end_time_kmeans - start_time_kmeans, digits = 4)
sim1_timed_matrix[1,3] <- round(end_time_dbscan - start_time_dbscan, digits = 4)
colnames(sim1_timed_matrix) <- c("spectral", 'kmeans', 'dbscan')

# We can repeat the simulation 20 times to find the means and standard deviations
# of the three indices.

ARI_matrix <- matrix(0,20,3)
JAC_matrix <- matrix(0, 20,3)
FM_matrix <- matrix(0,20,3)

colnames(ARI_matrix) <- c("spectral", "kmeans", "dbscan")
colnames(JAC_matrix) <- c("spectral", "kmeans", "dbscan")
colnames(FM_matrix) <- c("spectral", "kmeans", "dbscan")

set.seed(120)
for(i in 1:20)
{
  data1 = rGHD(n = 200, p = 2, mu = c(30,36), alpha = c(-3,2), sigma = matrix(c(1,2,2,1),nrow = 2),
               omega = 0.2, lambda = 1.2)
  data2 = rMSGHD(n = 200, p= 2, mu = c(40,32), alpha = c(-3,-2), sigma = matrix(c(1,2,2,3),nrow = 2),
                 omega = c(0.4,0.3), lambda = c(0.5,3))
  data3 = rCN(n = 200, mu = c(80,60), Sigma = matrix(c(20,8,8,20), ncol = 2), alpha = 0.2, eta = 12)
  data4 = rmvnorm(200, mean = c(-30,-15), sigma = matrix(c(100, -10, -10, 100), nrow=2)) + runif(200, min= -25, max = 25)
  
  combinedData <- as.data.frame(rbind(data1, data2, data3, data4))
  combinedData[,3] <- c(rep(1,200),  rep(2,200), rep(3,200), rep(4,200))
  colnames(combinedData) <- c('x', 'y', 'cluster')
  
  spec_sim1 <- specc(as.matrix(combinedData)[,1:2], centers = 4)
  kmean_sim1 <- kmeans(combinedData[,1:2], centers = 4, nstart = 50)
  dbscan_sim1 <- dbscan::dbscan(combinedData[,1:2], eps = 15.0, minPts = 4)
  
  ARI_matrix[i,1] <- ARI(combinedData$cluster, spec_sim1)
  ARI_matrix[i,2] <- ARI(combinedData$cluster, kmean_sim1$cluster)
  ARI_matrix[i,3] <- ARI(combinedData$cluster, dbscan_sim1$cluster)
  
  JAC_matrix[i,1] <- extCriteria(as.integer(combinedData$cluster), as.integer(spec_sim1), "Jaccard")$jaccard
  JAC_matrix[i,2] <- extCriteria(as.integer(combinedData$cluster), kmean_sim1$cluster, "Jaccard")$jaccard
  JAC_matrix[i,3] <- extCriteria(as.integer(combinedData$cluster), dbscan_sim1$cluster, "Jaccard")$jaccard
  
  FM_matrix[i,1] <- extCriteria(as.integer(combinedData$cluster), as.integer(spec_sim1), "Folkes")$folkes_mallows
  FM_matrix[i,2] <- extCriteria(as.integer(combinedData$cluster), kmean_sim1$cluster, "Folkes")$folkes_mallows
  FM_matrix[i,3] <- extCriteria(as.integer(combinedData$cluster), dbscan_sim1$cluster, "Folkes")$folkes_mallows
  
  print(i)
}

mixed_results_1 <- round(rbind(colMeans(ARI_matrix), colSds(ARI_matrix),
                                   colMeans(JAC_matrix),colSds(JAC_matrix),
                                   colMeans(FM_matrix),colSds(FM_matrix)), 4)

rownames(mixed_results_1) <- c('ARI Mean', 'ARI Std Dev', 
                                   'JAC Mean', 'JAC Std Dev', 
                                   'FM Mean', 'FM Std Dev')

# This is the final table with the mean and std deviations of each index for all 
# three clustering algorithms.
mixed_results_1

