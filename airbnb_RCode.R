#####################################################
####### Empirical Dataset 3: Airbnb Data #########
#####################################################

library(fpc)
library(dbscan)
library(kernlab)
library(MixGHD)

# data cleaning
airbnbOriginal <- santaClaraAirbnb
airbnbOriginal$longitude <-  as.numeric(as.character(airbnbOriginal$longitude))

# We sample 500 random observations from the original dataset and consider the eight continuous variables.
set.seed(120)
airbnbSample <- airbnbOriginal[sample(nrow(airbnb),500),]
airbnb <- airbnbSample[,c(4:5,7:12)]
airbnb <- as.matrix(cbind(airbnb[,1:2], prop.table(as.matrix(airbnb[,3:8]),margin=2)))


set.seed(120)
#################################### K-means 

# We try 2-5 clusters and select the best partition based on silhouette widths and S-dbw indices.

kmeansIndex <- c()
par(mfrow=c(2,2))
for (i in 2:5) 
{
  kmeans.airbnb <- kmeans(airbnb, centers = i, nstart = 50)
  kmeansIndex <- c(kmeansIndex,(intCriteria(airbnb, kmeans.airbnb$cluster, 'S_dbw'))$s_dbw)
  plot(silhouette(x = kmeans.airbnb$cluster, dist = dist(airbnb)),col="black", main="Silhouette Plot")
}
round(kmeansIndex, 2)
par(mfrow = c(1,1))

# We want to minimize S-dbw index and maximize average silhouette width. The average silhouette width 
# is approximately same for all values of G. The S-dbw index increases as the value of G increases, so 
# G = 2 or G = 3 may be the optimal choice. For G = 3, the observations are well spread out so we will 
# choose G = 3. 

# We can take a closer look at the clusters and the mean/mode of each variable.
set.seed(120)
kmeans.airbnb <- kmeans(airbnb, centers = 3, nstart = 50)

cluster1 <- airbnbSample[kmeans.airbnb$cluster==1, -c(1:2,4:5)]
cluster2 <- airbnbSample[kmeans.airbnb$cluster==2, -c(1:2,4:5)]
cluster3 <- airbnbSample[kmeans.airbnb$cluster==3, -c(1:2,4:5)]

# Neighborhood
table(cluster1$neighbourhood)
table(cluster2$neighbourhood)
table(cluster3$neighbourhood)

# Room Type
table(cluster1$room_type)
table(cluster2$room_type)
table(cluster3$room_type)

# Price
mean(cluster1$price)
mean(cluster2$price)
mean(cluster3$price)

# Nights 
mean(cluster1$minimum_nights)
mean(cluster2$minimum_nights)
mean(cluster3$minimum_nights)





#################################### DBSCAN

set.seed(120)

# We use default value of minPts = 4.
knn <- kNN(airbnb,4)                
knn_matrix <- sort(knn$dist[,4])

plot(order(knn_matrix), knn_matrix, main="Optimized Eps using kNN Function", xaxt = "n")
axis(1, at = seq(0, 500, by = 10))

# There is no obvious elbow point so we choose the value right before epsilon starts increasing quicker.
epsilon.value <- knn_matrix[480]

dbscan.airbnb <- fpc::dbscan(airbnb, eps = epsilon.value, MinPts = 4)
dbscan.airbnb$cluster

plot(silhouette(x = dbscan.airbnb$cluster,dist = dist(airbnb)),main="DBSCAN Silhouette, eps=0.03",col="Blue")
table(dbscan.airbnb$cluster)

# We can take a closer look at the clusters and the mean/mode of each variable.
cluster1 <- airbnbSample[dbscan.airbnb$cluster==0, -c(1:2,4:5)]
cluster2 <- airbnbSample[dbscan.airbnb$cluster==1, -c(1:2,4:5)]
cluster3 <- airbnbSample[dbscan.airbnb$cluster==2, -c(1:2,4:5)]

# Neighborhood
table(cluster1$neighbourhood)
table(cluster2$neighbourhood)
table(cluster3$neighbourhood)

# Room Type
table(cluster1$room_type)
table(cluster2$room_type)
table(cluster3$room_type)

# Price
mean(cluster1$price)
mean(cluster2$price)
mean(cluster3$price)

# Nights 
mean(cluster1$minimum_nights)
mean(cluster2$minimum_nights)
mean(cluster3$minimum_nights)







#################################### Spectral 

# We try 2-5 clusters and select best parition based on silhouette widths, S-dbw indices, and partition results.

set.seed(120)

spectralIndex <- c()
par(mfrow=c(2,2))
for (i in 2:5) 
{
  spectral.airbnb<- specc(airbnb,centers=i)  
  spectralIndex <- c(spectralIndex,(intCriteria(airbnb, as.integer(spectral.airbnb), 'S_dbw'))$s_dbw)
  plot(silhouette(x=spectral.airbnb,dist=dist(airbnb)),col="black", main="Silhouette Plot")
}
round(spectralIndex, 2)
par(mfrow=c(1,1))

# We select G = 3 because the indices are relatively optimal and the observations are well spread out for 
# this value of G. 

spectral.airbnb<- specc(airbnb, centers=3)  





###### Geographical Plots AND Comparison of Methods

# kmeans result
ggplot(data=airbnbSample, aes(x=longitude, y=latitude), xlab="longitude",ylab="latitude") + 
  geom_point(color=kmeans.airbnb$cluster)+theme_classic() + 
  theme(axis.title=element_text(size=14,face="bold"), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

# dbscan result
ggplot(data=airbnbSample, aes(x=longitude, y=latitude), xlab="longitude",ylab="latitude") + 
  geom_point(color=dbscan.airbnb$cluster+1)+theme_classic() + 
  theme(axis.title=element_text(size=14,face="bold"), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

# spectral result
ggplot(data=airbnbSample, aes(x=longitude, y=latitude), xlab="longitude",ylab="latitude") + 
  geom_point(color=spectral.airbnb)+theme_classic() + 
  theme(axis.title=element_text(size=14,face="bold"), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

#ARI
ARI(kmeans.airbnb$cluster,spectral.airbnb)
ARI(kmeans.airbnb$cluster,dbscan.airbnb$cluster)
ARI(spectral.airbnb,dbscan.airbnb$cluster)

#JAC 
extCriteria(as.integer(kmeans.airbnb$cluster), as.integer(spectral.airbnb), "Jaccard")$jaccard

