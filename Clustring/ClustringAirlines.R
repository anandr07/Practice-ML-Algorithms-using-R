# Heirarcial clustring

library(openxlsx)
EWairlines=read.xlsx("C://Users//ANANDI//Desktop//Assignment//Clustring//EastWestAirlines.xlsx",sheet = "data")
summary(EWairlines)
str(EWairlines)
EastWestairlines=EWairlines[-1]
View(EastWestairlines)
library(cluster)
EW_air=daisy(EWairlines,metric = "gower")
EW_air_matrix=as.matrix(EW_air)
hcluster=hclust(EW_air,method = "complete")
plot(hcluster)
hcluster_1=cutree(hcluster,k=5)
rect.hclust(hcluster,k=5,border = "red")
hcluster_1_matrix=as.matrix(hcluster_1)
hcluster_1
DF=data.frame(EWairlines,hcluster_1)
View(DF)
aggregate(DF,list(hcluster_1),mean)
library(dplyr)
hclustring=DF %>% select(hcluster_1,everything())#bring cluster to 1st column
View(hclustring)


# K means clustring

library(ggplot2)
Kmeanscluster=kmeans(EW_air_matrix,6)
Kmeanscluster
Kmeanscluster$cluster
wss=(nrow(EW_air_matrix)-1)*sum(apply(EW_air_matrix, 2,var))
for (i in 2:13) wss[i]=sum(kmeans(EW_air,centers = i)$withinss)
plot(1:13,wss, type="b",xlab="Number of clusters",ylab="Within sum of squares")  
kmeanscluster3=kmeans(EW_air_matrix,5)
wss=(nrow(EW_air_matrix)-1)*sum(apply(EW_air_matrix, 2,var))
for (i in 2:13) wss[i]=sum(kmeans(EW_air,centers = i)$withinss)
plot(1:13,wss, type="b",xlab="Number of clusters",ylab="Within sum of squares")  
kmeanscluster3$cluster
kmeans_matrix=kmeanscluster3$cluster
kmeans_matrix
kmeans_DF=data.frame(EWairlines,kmeans_matrix)
aggregate(kmeans_DF[,2:13],by=list(kmeans_matrix),FUN=mean)
View(kmeans_DF)
kmeansclustering=kmeans_DF %>% select(kmeans_matrix,everything())
View(kmeansclustering)
PLOT=clara(EW_air_matrix,5)
clusplot(PLOT)


# The clusters comes out to be 5