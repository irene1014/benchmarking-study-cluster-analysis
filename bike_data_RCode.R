#####################################################
####### Empirical Dataset 1: Bike Shop Data #########
#####################################################

library(cluster)
library(dbscan)
library(kernlab)
library(MixGHD)

bikeDataOriginal <- customerTrends[,-(1:6)]     # Extract only customer columns
bikeData <- t(bikeDataOriginal)                 # Get customers in rows and products in columns

set.seed(120)

#################################### K-means 

# We try 2-5 clusters and select the best partition based on silhouette widths.

kmeansIndex <- c()
par(mfrow=c(2,2))
for (i in 2:5) 
{
  kmeans.bike <- kmeans(bikeData, centers = i, nstart = 50)
  kmeansIndex <- c(kmeansIndex,(intCriteria(bikeData, kmeans.bike$cluster, 'Calinski_Harabasz'))$calinski_harabasz)
  plot(silhouette(x=kmeans.bike$cluster,dist=dist(bikeData)),col="blue", main="Silhouette Plot")
}
round(kmeansIndex, 2)
par(mfrow=c(1,1))


# We want to maximize both Calinski-Harabasz and silhouette indices. CH index is highest for G = 2, and silhouette is highest
# for G = 2. We select G = 4 because for G = 2, and G = 3, about two-thirds of the observations are grouped together in 1 cluster. 
# We want to spread out the observations to get a better picture of the commonalities between the bike shops. When G = 4, 
# the observations are well spread and our silhouette width/CH index is still relatively high.
 
# We select 4 clusters 
set.seed(120)
kmeans.bike <- kmeans(bikeData, centers = 4, nstart = 50)
cluster1 <- names(kmeans.bike$cluster[kmeans.bike$cluster==1])   # 13 shops in cluster 1
cluster2 <- names(kmeans.bike$cluster[kmeans.bike$cluster==2])   # 6 shops in cluster 2
cluster3 <- names(kmeans.bike$cluster[kmeans.bike$cluster==3])   # 8 shops in cluster 3
cluster4 <- names(kmeans.bike$cluster[kmeans.bike$cluster==4])   # 3 shops in cluster 4

# Grouping the corresponding bike shops from each cluster together
centers <-t(kmeans.bike$centers)
colnames(centers) <- make.names(colnames(centers))
organizedData <- cbind(customerTrends[,1:6], centers)

group1 <- head(organizedData[order(-X1), c(1:6, 7)],10)
group2 <- head(organizedData[order(-X2), c(1:6, 8)],10)
group3 <- head(organizedData[order(-X3), c(1:6, 9)],10)
group4 <- head(organizedData[order(-X4), c(1:6, 10)],10)

# We can look at the bike model/category/price that is most common in each cluster





#################################### DBSCAN

set.seed(120)
# Use minPts = 4 because we only have 30 observations. 
knn <- kNN(as.matrix(bikeData), 4)    
knn_matrix <- sort(knn$dist[,4])

plot(order(knn_matrix), knn_matrix, main="Optimized Eps using kNN Function", xaxt = "n")
axis(1, at = seq(0, 30, by = 2))

# We want to choose the epsilon at the 'elbow' point. There is no obvious one, so we approximate around 22.
epsilon.value <- knn_matrix[22]

dbscan.bike <- dbscan::dbscan(bikeData, eps = epsilon.value, minPts = 4)
dbscan.bike$cluster

intCriteria(bikeData, as.integer(dbscan.bike$cluster), 'Calinski_Harabasz')$calinski_harabasz
plot(silhouette(x=dbscan.bike$cluster,dist=dist(bikeData)),main="DBSCAN silhouette")

# Even for the optimal epsilon value, about 70% of the observations are clustered in one group.
# DBSCAN does not seem appropriate for this dataset.
names(bikeDataOriginal[dbscan.bike$cluster==0])
names(bikeDataOriginal[dbscan.bike$cluster==1])
names(bikeDataOriginal[dbscan.bike$cluster==2])




#################################### Spectral 

# Once again, we try 2-5 clusters and select the best partition based on silhouette widths. Since we only have
# 30 observations, we can afford to run spectral clustering multiple times for different values of G.

set.seed(120)

spectralIndex <- c()
par(mfrow=c(2,2))
for (i in 2:5) 
{
  spectral.bike <- specc(as.matrix(bikeData), centers=i)  
  spectralIndex <- c(spectralIndex,(intCriteria(bikeData, as.integer(spectral.bike), 'Calinski_Harabasz'))$calinski_harabasz)
  plot(silhouette(x = spectral.bike, dist = dist(bikeData)), col="blue", main="Silhouette Plot")
}
round(spectralIndex, 2)
par(mfrow=c(1,1))

# The silhouette and CH indices are best for G = 2 and G = 4. For G = 2, a majority of observations are once again
# clustered in the same group. For better spread, we choose G = 4 for spectral clustering.

set.seed(120)
spectral.bike <- specc(as.matrix(bikeData),centers=4)  

# We have the same number of observations per cluster as we obtained using k-means. We can check how close
# the partitions are using ARI.

ARI(kmeans.bike$cluster, spectral.bike)

# Since ARI is 1, we know the partitions are exactly the same. Hence, both methods are equally appropriate for this data.




#################################### Comparison between methods 

# ARI Index
ARI(kmeans.bike$cluster, dbscan.bike$cluster)

#JAC 
extCriteria(as.integer(kmeans.bike$cluster), as.integer(dbscan.bike$cluster), "Jaccard")$jaccard

#CH Index
intCriteria(bikeData, kmeans.bike$cluster, 'Calinski_Harabasz')
intCriteria(bikeData, as.integer(dbscan.bike$cluster), 'Calinski_Harabasz')
intCriteria(bikeData, as.integer(spectral.bike), 'Calinski_Harabasz')

