#####################################################
###### Empirical Dataset 2: Forest Fire Data ########
#####################################################

library(fpc)
library(dbscan)
library(cluster)
library(kernlab)

# We are working with four variables: temp, RH (relative humidity), wind, rain

forestFire <- as.matrix(forestfire[,c(9:12)])

set.seed(120)

#################################### K-means 

# We try 2-5 clusters and select the best partition based on silhouette widths and S-dbw indices.

kmeansIndex <- c()
par(mfrow=c(2,2))
for (i in 2:5) 
{
  kmeans.forest <- kmeans(forestFire, centers = i, nstart = 50)
  kmeansIndex <- c(kmeansIndex,(intCriteria(forestFire, kmeans.forest$cluster, 'S_dbw'))$s_dbw)
  plot(silhouette(x=kmeans.forest$cluster,dist=dist(forestFire)),col="black", main="Silhouette Plot")
}
round(kmeansIndex, 2)
par(mfrow = c(1,1))

# Based on silhouette plots and our internal indices we select G = 3. The corresponding partition is visualized below.

set.seed(120)

kmeans.forest <- kmeans(x=forestFire, centers=3, nstart = 50) 
obs.group <- kmeans.forest$cluster 
plot(forestFire, col=obs.group, pch = 20)  # plotting partition for G = 3

# Grouping together appropriate observations in each cluster
cluster1 <- forestFire[kmeans.forest$cluster==1,]   
cluster2 <- forestFire[kmeans.forest$cluster==2,]  
cluster3 <- forestFire[kmeans.forest$cluster==3,]  

# This gives us an idea of how the clusters were grouped together.
colMeans(cluster1)
colMeans(cluster2)
colMeans(cluster3)





#################################### DBSCAN

set.seed(120)

# We use minPts = 2*numberOfDimensions = 2*4 = 8
knn <- kNN(forestFire,8)                
knn_matrix <- sort(knn$dist[,8])

plot(order(knn_matrix), knn_matrix, main="Optimized Eps using kNN Function", xaxt = "n")
axis(1, at = seq(0, 510, by = 10))

# We want to choose the epsilon at the 'elbow' point, approximately about 460.
epsilon.value <- knn_matrix[460]

dbscan.forest <- fpc::dbscan(forestFire, eps = epsilon.value, MinPts = 8)
dbscan.forest$cluster  # there are only two separate clusters

# Our internal indices for the current dbscan partition.
intCriteria(forestFire, as.integer(dbscan.forest$cluster), 'S_dbw')$s_dbw
plot(silhouette(x=dbscan.forest$cluster,dist=dist(forestFire)),main="DBSCAN silhouette")


# We are comparing different values of epsilon to show that we have the same clustering issue 
# regardless of our epsilon value. 
par(mfrow=c(1,2))
forestFireSubset <- dt[,c(9,10)]

# when epsilon is smaller, there are too many small clusters
db <- fpc::dbscan(forestFire, eps = 2, MinPts = 8)   
plot(forestFireSubset$temp, forestFireSubset$RH, col = db$cluster+2, main="eps = 2", 
     xlab = 'temp', ylab = 'relative humidity', cex.lab=1.3, cex.main=1.4, cex.axis=1.3, pch = 20)

# when epsilon is larger, almost all points are clustered together
db <- fpc::dbscan(forestFire, eps = 8, MinPts = 8)   
plot(forestFireSubset$temp, forestFireSubset$RH, col = db$cluster+1, main="eps = 8", 
     xlab = 'temp', ylab = 'relative humidity',  cex.lab=1.3, cex.main=1.4, cex.axis=1.3, pch = 20)






#################################### Spectral 

# We try 2-5 clusters and select best parition based on silhouette widths, S-dbw indices, and partition results.

set.seed(120)

spectralIndex <- c()
par(mfrow=c(2,2))
for (i in 2:5) 
{
  spectral.forest<- specc(forestFire,centers=i)  
  spectralIndex <- c(spectralIndex,(intCriteria(forestFire, as.integer(spectral.forest), 'S_dbw'))$s_dbw)
  plot(silhouette(x=spectral.forest,dist=dist(forestFire)),col="black", main="Silhouette Plot")
}
round(spectralIndex, 2)
par(mfrow=c(1,1))


spectral.forest<- specc(forestFire,centers=3)  

# Grouping together appropriate observations in each cluster
cluster1 <- forestFire[spectral.forest==1,]   
cluster2 <- forestFire[spectral.forest==2,]  
cluster3 <- forestFire[spectral.forest==3,]  

# This gives us an idea of how the clusters were grouped together.
colMeans(cluster1)
colMeans(cluster2)
colMeans(cluster3)

plot(forestFire, col = spectral.forest, pch = 20)


#################################### Comparison between methods 

# ARI
ARI(kmeans.forest$cluster,spectral.forest)

#JAC 
extCriteria(as.integer(kmeans.forest$cluster), as.integer(spectral.forest), "Jaccard")$jaccard



