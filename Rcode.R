#####################################################
#### A Benchmarking Study in Clustering - R Code ####
#####################################################


#####################################################
########### Part 1. Multishapes Data ################
#####################################################
install.packages("grid")
library(gridExtra)
library(grid)
library(factoextra)
library(kernlab)
library(dbscan)
library(ggplot2)
library(MixGHD)
library(MASS)

multi_data <- multishapes
multi_data_obs <- as.matrix(multishapes[,1:2])
multi_dataFrame <- data.frame(multi_data)

# plotting original multishapes data
p <- ggplot(multi_dataFrame, aes(x = x, y = y)) + ggtitle("MultiShapes Data") +
  xlab("X") + ylab("Y")
p + geom_point(col = multi_dataFrame$shape, size = 1.3) + theme(plot.title = element_text(hjust = 0.5, color="black", size=24))

# deciding eps and minpts
knn_matrix <- kNN(multi_data_obs,4)
knn_matrix_new <- sort(knn_matrix$dist[,4])
plot(order(knn_matrix_new),knn_matrix_new,main="Optimized Eps using KNNS")

# running the algorithms on multishapes while timing them
start_time_spec <- Sys.time()
specc_multishapes <- specc(as.matrix(multi_data_obs), centers = 6)
end_time_spec <- Sys.time()

start_time_kmeans <- Sys.time()
kmean_multishapes <- kmeans(multi_data_obs, centers = 6)
end_time_kmeans <- Sys.time()

start_time_dbscan <- Sys.time()
dbscan_multishapes <- dbscan(multi_data_obs, eps = 0.15, MinPts = 4)
end_time_dbscan <- Sys.time()

multishapes_timed_matrix <- matrix(0,1,3)
multishapes_timed_matrix[1,1] <- paste(round(end_time_spec - start_time_spec, digits = 4), "sec")
multishapes_timed_matrix[1,2] <- paste(round(end_time_kmeans - start_time_kmeans, digits = 4), "sec")
multishapes_timed_matrix[1,3] <- paste(round(end_time_dbscan - start_time_dbscan, digits = 4), "sec")

colnames(multishapes_timed_matrix) <- c("Spectral", "K-means", "DBSCAN")
grid.table(multishapes_timed_matrix)

# plotting spectral clus results
p <- ggplot(multi_dataFrame,aes(x = x, y = y)) + ggtitle("Spectral Clustering Results") +
  xlab("X") + ylab("Y")+title()
p + geom_point(col = specc_multishapes, size = 1.3) + theme(plot.title = element_text(hjust = 0.5, color="black", size=24))

# plotting kmeans results
p <- ggplot(multi_dataFrame,aes(x = x, y = y)) + ggtitle("K-Means Results") +
  xlab("X") + ylab("Y")
p + geom_point(col = kmean_multishapes$cluster, size = 1.3) + theme(plot.title = element_text(hjust = 0.5, color="black", size=24))

# plotting dbscan results
# changing 0 to 6 for color
p <- ggplot(multi_dataFrame,aes(x = x, y = y)) + ggtitle("DBSCAN Results") +
  xlab("X") + ylab("Y")
dbscan_multishapes$cluster[dbscan_multishapes$cluster == 0] <- 6
p + geom_point(col = dbscan_multishapes$cluster, size = 1.3) + theme(plot.title = element_text(hjust = 0.5, color="black", size=24))


# ARI results 
ARI_matrix <- matrix(0,1,3)
ARI_matrix[1,1] <- ARI(multi_data$shape, specc_multishapes)
ARI_matrix[1,2] <- ARI(multi_data$shape, kmean_multishapes$cluster)
ARI_matrix[1,3] <- ARI(multi_data$shape, dbscan_multishapes$cluster)

colnames(ARI_matrix) <- c("Spectral", "K-means", "DBSCAN")
grid.table(round(ARI_matrix, digits = 4))






#####################################################
############## Part 2. Spiral Data ##################
#####################################################
library(mlbench)

# Simulated spiral data - run 20 times and compare mean ARI values 

set.seed(120)
spiralData <- mlbench.spirals(n = 400, cycles = 2.5, sd = 0.025)

spirals_dataFrame <- data.frame(spiralData)

# plotting original data with color coding
p <- ggplot(spirals_dataFrame, aes(x = x.1, y = x.2)) + ggtitle("Spirals Data") +
  xlab("X") + ylab("Y")
p + geom_point(col = spirals_dataFrame$classes, size = 1.3) + theme(plot.title = element_text(hjust = 0.5, color="black", size=24))

# deciding epsilon for dbscan

knn_matrix <- kNN(spiralData$x,2)
knn_matrix_new <- sort(knn_matrix$dist[,2])
plot(order(knn_matrix_new),knn_matrix_new,main="Optimized Eps using KNNS")

start_time_spec <- Sys.time()
spec_spirals <- specc(spiralData$x, centers = 2)
end_time_spec <- Sys.time()

start_time_kmeans <- Sys.time()
kmean_spirals <- kmeans(spiralData$x, centers = 2)
end_time_kmeans <- Sys.time()

start_time_dbscan <- Sys.time()
dbscan_spirals <- dbscan(spiralData$x, eps = 0.17, MinPts = 2)
end_time_dbscan <- Sys.time()

spiral_timed_matrix <- matrix(0,1,3)
spiral_timed_matrix[1,1] <- paste(round(end_time_spec - start_time_spec, digits = 4), "sec")
spiral_timed_matrix[1,2] <- paste(round(end_time_kmeans - start_time_kmeans, digits = 4), "sec")
spiral_timed_matrix[1,3] <- paste(round(end_time_dbscan - start_time_dbscan, digits = 4), "sec")

colnames(spiral_timed_matrix) <- c("Spectral", "K-means", "DBSCAN")

grid.table(spiral_timed_matrix)

# plotting spectral clus results
p <- ggplot(spirals_dataFrame,aes(x = x.1, y = x.2)) + ggtitle("Spectral Clustering Results") +
  xlab("X") + ylab("Y")
p + geom_point(col = spec_spirals, size = 1.3) + theme(plot.title = element_text(hjust = 0.5, color="black", size=24))

# plotting kmeans results
p <- ggplot(spirals_dataFrame,aes(x = x.1, y = x.2)) + ggtitle("K-Means Results") +
  xlab("X") + ylab("Y")
p + geom_point(col = kmean_spirals$cluster, size = 1.3) + theme(plot.title = element_text(hjust = 0.5, color="black", size=24))

# plotting dbscan results
p <- ggplot(spirals_dataFrame, aes(x = x.1, y = x.2)) + ggtitle("DBSCAN Results") +
  xlab("X") + ylab("Y")
p + geom_point(col = dbscan_spirals$cluster, size = 1.3) + theme(plot.title = element_text(hjust = 0.5, color="black", size=24))



# repeated simulation
ARI_matrix <- matrix(0,20,3)
colnames(ARI_matrix) <- c("spectral", "kmeans", "dbscan")
set.seed(120)
for(i in 1:20)
{
  spiralData <- mlbench.spirals(n = 400, cycles = 2.5, sd = 0.025)
  spec_spirals <- specc(spiralData$x, centers = 2)
  kmean_spirals <- kmeans(spiralData$x, centers = 2)
  dbscan_spirals <- dbscan(spiralData$x, eps = 0.17, MinPts = 2)
  
  ARI_matrix[i,1] <- ARI(spiralData$classes, spec_spirals)
  ARI_matrix[i,2] <- ARI(spiralData$classes, kmean_spirals$cluster)
  ARI_matrix[i,3] <- ARI(spiralData$classes, dbscan_spirals$cluster)
  
  print(i)
}
# the ARI matrix of 20 simulations will tell us which one is the best for spiral data with minimum noise
spirals_ARI_matrix <- matrix(0,1,3)
spirals_ARI_matrix[1,1] <- mean(ARI_matrix[,1])
spirals_ARI_matrix[1,2] <- mean(ARI_matrix[,2])
spirals_ARI_matrix[1,3] <- mean(ARI_matrix[,3])

colnames(spirals_ARI_matrix) <- c("Spectral", "K-means", "DBSCAN")
rownames(ARI_matrix) <- seq(1:20)
grid.table(round(spirals_ARI_matrix, digits = 4))







#####################################################
################ Part 3. Bike Data ##################
#####################################################

# 1. Kmeans
set.seed(123)
kmeansDat
kmeansDat <- customerTrends[,-(1:6)]  # Extract only customer columns
kmeansDat.t <- t(kmeansDat)  # Get customers in rows and products in columns

#Decide number of clusters by silhouette
library(cluster)
par(mfrow=c(2,2))

for (i in 2:5) {
  center= i
  kmeans.out <- kmeans(kmeansDat.t, centers = i, nstart = 50)
  plot(silhouette(x=kmeans.out$cluster,dist=dist(kmeansDat.t)),col="black", main="Silhouette Plot")
}

par(mfrow=c(1,1))

kmeans.out <- kmeans(kmeansDat.t, centers = 4, nstart = 50)
plot(silhouette(x=kmeans.out$cluster,dist=dist(kmeansDat.t)),col="black", main="Bike-K-Means Silhouette Plot")


kmeans.out <- kmeans(kmeansDat.t, centers = 4, nstart = 50)
plot(silhouette(x=kmeans.out$cluster,dist=dist(kmeansDat.t)),col="black", main="Silhouette Plot")
# 4 clusters 

kmeans.out <- kmeans(kmeansDat.t, centers = 4, nstart = 10)
summary(kmeans.out$cluster)
names(kmeans.out$cluster[kmeans.out$cluster==1])
names(kmeans.out$cluster[kmeans.out$cluster==2])
names(kmeans.out$cluster[kmeans.out$cluster==3])
names(kmeans.out$cluster[kmeans.out$cluster==4])
names(kmeans.out$cluster[kmeans.out$cluster==5])

t <-t(kmeans.out$centers)
colnames(t) <- make.names(colnames(t))
dtc <- cbind(customerTrends[,1:5],t)
head(dtc)
attach(dtc)
k1 <- head(dtc[order(-X1), c(1:5, 6)],10)
k2 <- head(dtc[order(-X2), c(1:5, 7)],10)
k3 <- head(dtc[order(-X3), c(1:5, 8)],10)
k4 <- head(dtc[order(-X4), c(1:5, 9)],10)
k1
k2
k3
k4


# 2. DBSCN 

library(fpc)
library(dbscan)

df <- kmeansDat.t
dt <- kmeansDat

a <- kNN(as.matrix(df),4)
a1 <- sort(a$dist[,4])
par(mfrow=c(1,1))
plot(order(a1),a1,main="Optimized Eps using KNNS")

db <- fpc::dbscan(df, eps = 0.07, MinPts = 2)
db$cluster

plot(silhouette(x=db$cluster,dist=dist(df)),main="Bike-DBSCAN Silhouette Plot",col='black',cex.axis=1.5,cex.title=10)

names(dt[db$cluster==0])
names(dt[db$cluster==1])
names(dt[db$cluster==2])
names(dt[db$cluster==3])

# 3.Spectral

library(kernlab)
sp <- specc(as.matrix(df),centers=4)  
plot(silhouette(x=sp,dist=dist(df)),main="Bike-Spectral Silhouette Plot",col="black",cex.axis=1.5)

t1 <-t(centers(sp))
colnames(t1) <- c('X1','X2','X3','X4')
dtc1 <- cbind(customerTrends[,1:5],t1)

k1 <- head(dtc1[order(-dtc1$X1), c(1:5, 6)],10)
k2;
k3;
k4

#ARI

library(MixGHD)
ARI(kmeans.out$cluster,db$cluster)
table(kmeans.out$cluster,db$cluster)
ARI(kmeans.out$cluster,sp)







#####################################################
############### Part 4. Forest Fire #################
#####################################################

dt <- forestfire
dt1 <- dt[,c(9:12)]

# 1.K-means

dt11 <- t(dt1)
par(mfrow=c(1,1))
library(cluster)
for (i in 2:5) {
  center= i
  kmeans.out <- kmeans(dt1, centers = i, nstart = 50)
  plot(silhouette(x=kmeans.out$cluster,dist=dist(dt1)),col="blue", main="Silhouette Plot")
}
#Average Silhouetthe widths by centers: 0.55, 0.43, 0.4,0.39 
kmeans.out <- kmeans(dt1, centers = 4, nstart = 50)
plot(silhouette(x=kmeans.out$cluster,dist=dist(dt1)),col=1, main="Forest-K-Means Silhouette Plot",border=NA,cex.axis=1.5)
  theme(axis.text=element_text(size=12))

#cluster4

par(mfrow=c(1,1))
kmeans.out <- kmeans(x=dt1,centers=4) 
obs.group <- kmeans.out$cluster 
grp.means <- kmeans.out$center 
plot(dt1,pch=obs.group,cex.axis=1.5)

# 2.DBSCAN

library(fpc)
library(dbscan)

dt1 <- as.matrix(dt1)
a <- kNN(dt1,4)
a1 <- sort(a$dist[,4])
plot(order(a1),a1,main="Optimized Eps using KNNS",cex.lab=1.7,cex.main=1.7,cex.axis=1.5)

db <- fpc::dbscan(dt1, eps = 4, MinPts = 4)
summary(as.factor(db$cluster))
db <- fpc::dbscan(dt1, eps = 5, MinPts = 4)
summary(as.factor(db$cluster))

df <- kmeansDat.t
plot(silhouette(x=db$cluster,dist=dist(dt1)),main="Forest-DBSCAN Silhouette Plot",col='black',border=NA,cex.axis=1.5)


par(mfrow=c(1,2))
dt1 <- dt[,c(9,11)]
db <- fpc::dbscan(dt1, eps = 1, MinPts = 4)
db
dt1
plot(dt1,main="DBSCAN, eps=1",
     cex.lab=1.6,cex.main=1.7,
     cex.axis=1.5,pch=db$cluster,lwd=2)

db <- fpc::dbscan(dt1, eps = 1.5, MinPts = 4)
plot(dt1,main="DBSCAN, eps=1.5",cex.main=1.7,
     cex.axis=1.5,cex.lab=1.6,pch=db$cluster,lwd=2)

# 3.Spectral
dt1 <- dt[,c(9:12)]
library(kernlab)
library(cluster)
sp <- specc(as.matrix(dt1),centers=4)  
kmeans.out <- kmeans(x=dt1,centers=4) 
ARI(kmeans.out$cluster,sp)

par(mfrow=c(1,1))
sp <- specc(as.matrix(dt1),centers=4)  
plot(silhouette(x=sp,dist=dist(dt1)),main="Forest-Spectral Silhouette Plot",col="Black",border=NA,cex.axis=1.5)

# ARI
ARI(kmeans.out$cluster,sp)
table(kmeans.out$cluster,sp)

plot(silhouette(x=kmeans.out$cluster,dist=dist(dt1)),col=1, main="K-Means Silhouette",border=NA,cex.axis=1.5)
plot(silhouette(x=db$cluster,dist=dist(dt1)),main="DBSCAN silhouette",col='black',border=NA,cex.axis=1.5)
plot(silhouette(x=sp,dist=dist(dt1)),main="Spectral Silhouette",col="Black",border=NA,cex.axis=1.5)





#####################################################
################ Part 5. AirBnB #####################
#####################################################

# Data Processing - removing outliers
dt <- santaClaraAirbnb
summary(dt)
dt$longitude <-  as.numeric(as.character(dt$longitude))

set.seed(123)
dts <- dt[sample(nrow(dt),500),]
head(dt)
dt1 <- dts[,c(4:5,7:12)]
head(dt1)
str(dt1)
dt3 <- cbind(dt1[,1:2], prop.table(as.matrix(dt1[,3:8]),margin=2))


# 1.K-means

head(dt3)
par(mfrow=c(1,1))

for (i in 2:5) {
  center= i
  kmeans.out <- kmeans(dt3, centers = i, nstart = 50)
  plot(silhouette(x=kmeans.out$cluster,dist=dist(dt3)),col="blue", main="Silhouette Plot")
}

#cluster4
set.seed(123)
kmeans.out <- kmeans(x=dt3,centers=4) 
kmeans.out <- kmeans(dt3, centers = 4, nstart = 50)
plot(silhouette(x=kmeans.out$cluster,dist=dist(dt3)),col="black", main="Airbnb-K-Means Silhouette Plot",border=NA,cex.axis=1.5)
rev(dt3$latitude)
clusters=kmeans.out$cluster
ggplot(data=dt3, aes(x=latitude, y=longitude),
       xlab="latitude",ylab="longtitude") + geom_point(shape=clusters,size=4,col=kmeans.out$cluster)+
  theme(axis.title=element_text(size=24,face="bold"),
        axis.text=element_text(size=20))+scale_y_continuous(trans = "reverse")+
  scale_x_continuous(trans = "reverse")+ggtitle("K-means")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

k1 <- dts[kmeans.out$cluster==1,-c(1:2)]
k2 <- dts[kmeans.out$cluster==2,-c(1:2,4:5)]
k3 <- dts[kmeans.out$cluster==3,-c(1:2,4:5)]
k4 <- dts[kmeans.out$cluster==4,-c(1:2,4:5)]
summary(k1)
summary(k2)
summary(k3)
summary(k4)

# 2.DBSCAN
library(fpc)
library(dbscan)

a <- kNN(dt3,4)
a1 <- sort(a$dist[,4])
plot(order(a1),a1,main="Optimized Eps using KNNS",cex.lab=1.7,cex.main=1.7,cex.axis=1.5)

par(mfrow=c(1,1))
db <- fpc::dbscan(dt3, eps = 0.028, MinPts = 4)
plot(silhouette(x=db$cluster,dist=dist(dt3)),main="DBSCAN Silhouette, eps=0.028",col="Black",cex.axis=1.5,border=NA)
a <- kNN(dt3,4)
a1 <- sort(a$dist[,4])
db <- fpc::dbscan(dt3, eps = 0.029, MinPts = 4)
plot(silhouette(x=db$cluster,dist=dist(dt3)),main="Airbnb-DBSCAN Silhouette Plot, eps=0.029",col="Black",cex.axis=1.5,border=NA)
table(db$cluster)

k11 <- dts[db$cluster==0,-c(1:2)]
summary(k11)
k22 <- dts[db$cluster==1,-c(1:2)]
summary(k22)
k33 <- dts[db$cluster==2,-c(1:2)]
summary(k33)
clusters=kmeans.out$cluster
ggplot(data=dts, aes(x=latitude, y=longitude),
       xlab="latitude",ylab="longtitude") + geom_point(shape=db$cluster+1,size=4,col=db$cluster+1)+
  theme(axis.title=element_text(size=24,face="bold"),
        axis.text=element_text(size=20))+scale_y_continuous(trans = "reverse")+
  scale_x_continuous(trans = "reverse")+ggtitle("DBSCAN")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# 3. Spectral

library(kernlab)
par(mfrow=c(1,1))
sp <- specc(as.matrix(dt3),centers=3)  
plot(silhouette(x=sp,dist=dist(dt3)),main="Spectral Silhouette",col="Blue")
sp <- specc(as.matrix(dt3),centers=4)  
plot(silhouette(x=sp,dist=dist(dt3)),main="Airbnb-Spectral Silhouette Plot",col="Black",border=NA,cex.axis=1.5)

par(mfrow=c(1,1))
ggplot(data=dt3, aes(x=latitude, y=longitude),
       xlab="latitude",ylab="longtitude") + geom_point(shape=sp,size=4,col=sp)+
  theme(axis.title=element_text(size=24,face="bold"),
        axis.text=element_text(size=20))+scale_y_continuous(trans = "reverse")+
  scale_x_continuous(trans = "reverse")+ggtitle("Spectral")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#ARI
library(MixGHD)
ARI(kmeans.out$cluster,sp)
kmeans.out <- kmeans(dt3, centers = 4, nstart = 50)
plot(silhouette(x=kmeans.out$cluster,dist=dist(dt3)),col="black", main="Airbnb-K-Means Silhouette Plot",border=NA,cex.axis=1.5)
sp <- specc(as.matrix(dt3),centers=4)  
ARI(kmeans.out$cluster,db$cluster)
ARI(sp,db$cluster)
table(kmeans.out$cluster,sp)

kmeans.out <- kmeans(dt3, centers = 4, nstart = 50)
db <- fpc::dbscan(dt3, eps = 0.028, MinPts = 4)
kmeans.out <- kmeans(dt3, centers = 4, nstart = 50)
plot(silhouette(x=kmeans.out$cluster,dist=dist(dt3)),col="black", main="K-Means Silhouette",border=NA,cex.axis=1.5)
sp <- specc(as.matrix(dt3),centers=4)  
plot(silhouette(x=sp,dist=dist(dt3)),main="Airbnb-Spectral Silhouette Plot",col="Black",border=NA,cex.axis=1.5)
ARI(kmeans.out$cluster,sp)

plot(silhouette(x=kmeans.out$cluster,dist=dist(dt3)),col="black", main="K-Means Silhouette",border=NA,cex.axis=1.5)
plot(silhouette(x=db$cluster,dist=dist(dt3)),main="DBSCAN Silhouette, eps=0.028",col="Black",cex.axis=1.5,border=NA)
plot(silhouette(x=sp,dist=dist(dt3)),main="Spectral Silhouette",col="Black",border=NA,cex.axis=1.5)
ARI(kmeans.out$cluster,sp)
ARI(sp,db$cluster)

