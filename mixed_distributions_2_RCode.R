#####################################################
#### Simulated Dataset 5: Mixed Distributions 2 #####
#####################################################

library(movMF)
library(dbscan)
library(MixGHD)
library(mvtnorm)
library(dbscan)
library(kernlab)
library(matrixStats)

set.seed(120)

c1 = rmvnorm(200,c(-1,6),diag(2))
c2 = rmvnorm(200,c(6,6),diag(2))
c3 = cbind(runif(40,0,4),runif(40,5.5,6))
c4 =rmovMF(200,c(3, 6))*3 + cbind(rep(-4.2,200),runif(200,0.8,1.2))
c5 = rmovMF(200,c(-3,-10))*4 + cbind(rep(4,200),runif(200,12,12.5))
c6 = rGHD(n = 200, p=2, mu = c(7.5,2),  alpha = c(0.1,-0.1))
c7 = rGHD(n =200, p =2, mu = c(0,1.5), alpha = c(2,0.2), omega = 12, lambda = -3)

data=as.data.frame(rbind(c1,c2,c3,c4, c5, c6, c7))
data[,3] = c(rep(1,200), rep(2, 240), rep(3,200), rep(4, 200), rep(5,200), rep(6,200))


# Plot of simulated data
plot(data[,1:2],col=data[,3], pch = 20)
colnames(data) <- c('x', 'y', 'cluster')

# Finding optimal eps value for dbscan
knn_matrix <- kNN(data[,1:2],4)
knn_matrix_new <- sort(knn_matrix$dist[,4])
plot(order(knn_matrix_new),knn_matrix_new,main="Optimized Eps using KNNS", xaxt = 'n')
axis(1, at = seq(0, 1240, by = 20))

# The optimal eps is approximately 0.6
knn_matrix_new[1160]

# We are calculating the run time of each algorithm on overlapping mixed clusters.
start_time_spec <- Sys.time()
spec_sim3 <- specc(as.matrix(data)[,1:2], centers = 6)
end_time_spec <- Sys.time()

start_time_kmeans <- Sys.time()
kmean_sim3 <- kmeans(data[,1:2], centers = 6, nstart = 50)
end_time_kmeans <- Sys.time()

start_time_dbscan <- Sys.time()
dbscan_sim3 <- dbscan::dbscan(data[,1:2], eps = 0.6, minPts = 4)
end_time_dbscan <- Sys.time()

sim3_timed_matrix <- matrix(0,1,3)
sim3_timed_matrix[1,1] <- round(end_time_spec - start_time_spec, digits = 4)
sim3_timed_matrix[1,2] <- round(end_time_kmeans - start_time_kmeans, digits = 4)
sim3_timed_matrix[1,3] <- round(end_time_dbscan - start_time_dbscan, digits = 4)
colnames(sim3_timed_matrix) <- c("spectral", 'kmeans', 'dbscan')

# We can repeat the simulation 20 times to find the means and standard deviations
# of the three indices.

ARI_matrix <- matrix(0,20,3)
JAC_matrix <- matrix(0, 20,3)
FM_matrix <- matrix(0,20,3)

colnames(ARI_matrix) <- c("spectral", "kmeans", "dbscan")
colnames(JAC_matrix) <- c("spectral", "kmeans", "dbscan")
colnames(FM_matrix) <- c("spectral", "kmeans", "dbscan")

set.seed(120)
include <- c(1:200, 240:1240)        # we do not include the noise observations in our index calculations
for(i in 1:20)
{
  c1=rmvnorm(200,c(-1,6),diag(2))
  c2=rmvnorm(200,c(6,6),diag(2))
  c3=cbind(runif(40,0,4),runif(40,5.5,6))
  c4 =rmovMF(200,c(3, 6))*3 +cbind(rep(-4.2,200),runif(200,0.8,1.2))
  c5 = rmovMF(200,c(-3,-10))*4 +cbind(rep(4,200),runif(200,12,12.5))
  c6 = rGHD(n = 200, p=2, mu = c(7.5,2),  alpha = c(0.1,-0.1))
  c7 = rGHD(n =200, p =2, mu = c(0,1.5), alpha = c(2,0.2), omega = 12, lambda = -3)
  
  data=as.data.frame(rbind(c1,c2,c3,c4, c5, c6, c7))
  data[,3] = c(rep(1,200), rep(2, 240), rep(3,200), rep(4, 200), rep(5,200), rep(6,200))
  colnames(data) <- c('x', 'y', 'cluster')
  
  spec_sim3 <- specc(as.matrix(data)[,1:2], centers = 6)
  kmean_sim3 <- kmeans(data[,1:2], centers = 6, nstart = 50)
  dbscan_sim3 <- dbscan::dbscan(data[,1:2], eps = 0.5, minPts = 3)
  
  ARI_matrix[i,1] <- ARI(data$cluster[include], spec_sim3[include])
  ARI_matrix[i,2] <- ARI(data$cluster[include], kmean_sim3$cluster[include])
  ARI_matrix[i,3] <-  ARI(data$cluster[include], dbscan_sim3$cluster[include])
  
  JAC_matrix[i,1] <- extCriteria(as.integer(data$cluster[include]), as.integer(spec_sim3[include]), "Jaccard")$jaccard
  JAC_matrix[i,2] <- extCriteria(as.integer(data$cluster[include]), kmean_sim3$cluster[include], "Jaccard")$jaccard
  JAC_matrix[i,3] <- extCriteria(as.integer(data$cluster[include]), dbscan_sim3$cluster[include], "Jaccard")$jaccard
  
  FM_matrix[i,1] <- extCriteria(as.integer(data$cluster[include]), as.integer(spec_sim3[include]), "Folkes")$folkes_mallows
  FM_matrix[i,2] <- extCriteria(as.integer(data$cluster[include]), kmean_sim3$cluster[include], "Folkes")$folkes_mallows
  FM_matrix[i,3] <- extCriteria(as.integer(data$cluster[include]), dbscan_sim3$cluster[include], "Folkes")$folkes_mallows
  
  print(i)
}

mixed_results_2 <- round(rbind(colMeans(ARI_matrix), colSds(ARI_matrix),
                                   colMeans(JAC_matrix),colSds(JAC_matrix),
                                   colMeans(FM_matrix),colSds(FM_matrix)), 4)
rownames(mixed_results_2) <- c('ARI Mean', 'ARI Std Dev', 
                                   'JAC Mean', 'JAC Std Dev', 
                                   'FM Mean', 'FM Std Dev')

# This is the final table with the mean and std deviations of each index for all 
# three clustering algorithms.
mixed_results_2
