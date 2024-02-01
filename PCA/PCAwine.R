attach(wine)
summary(wine)
str(wine)
DF=data.frame(wine[,2:14])
View(DF)
PC_wine=princomp(DF,cor=TRUE,scores = TRUE,covmat = NULL)
summary(PC_wine)
loadings(PC_wine)
plot(PC_wine)

# Heirarcial clustring

PC_scores=PC_wine$scores
PC_dist=dist(PC_scores[,1:3],method = "euclidean")
PC_hclust=hclust(PC_dist,method = "complete")
plot(PC_hclust)
PC_Dendo=cutree(PC_hclust,k=4)
rect.hclust(PC_hclust,k=4,border = "red")
PC_hcluster=as.matrix(PC_Dendo)
PC_DF=data.frame(wine,PC_hcluster)
aggregate(PC_DF,list(PC_hcluster),mean)
library(dplyr)
final=PC_DF %>% select(PC_hcluster,everything())
View(final)


# Kmeans clustring

PC_kMclust=kmeans(PC_dist,4)
PC_kmeans=as.matrix(PC_kMclust$cluster)
PC_kmclust_DF=data.frame(final,PC_kmeans)
wss=(nrow(as.matrix(PC_dist))-1)*sum(apply(as.matrix(PC_dist), 2,var))
for (i in 1:13) wss[i]=sum(kmeans(PC_dist,centers = i)$withinss)
plot(1:13,wss, type="b",xlab="Number of clusters",ylab="Within sum of squares") 
FINAL=PC_kmclust_DF %>% select(PC_kmeans,everything())
aggregate(FINAL[,4:16],by=list(PC_kmeans),FUN = mean)
library(cluster)
PLOT=clara(as.matrix(PC_dist),4)
clusplot(PLOT)
View(FINAL)

#Hierarchical clustring

norm_data=scale(DF)
norm_data
dist=dist(norm_data,method = "euclidean")
dist_matrix=as.matrix(dist)
hclust_wine=hclust(dist,method = "complete")
plot(hclust_wine)
plot(hclust_wine,hang = -1)
hclust_wine1=cutree(hclust_wine,k=4)
rect.hclust(hclust_wine,k=4,border = "red")
cluster=as.matrix(hclust_wine1)
New_data=data.frame(DF,cluster)
aggregate(New_data,list(New_data$cluster),mean)
library(dplyr)
New_crimedata=New_data[,c(ncol(New_data),1:(ncol(New_data)-1))]
library(factoextra)
fviz_dend(hclust_wine,k=4,rect = TRUE,rect_border = "red",rect_fill = TRUE,ggtheme = theme_gray())

# K means clustring

library(ggplot2)
Kmeanscluster=kmeans(dist_matrix,6)
Kmeanscluster
Kmeanscluster$cluster
wss=(nrow(dist_matrix)-1)*sum(apply(dist_matrix, 2,var))
for (i in 2:13) wss[i]=sum(kmeans(dist,centers = i)$withinss)
plot(1:13,wss, type="b",xlab="Number of clusters",ylab="Within sum of squares")  
kmeanscluster3=kmeans(dist_matrix,3)
wss=(nrow(dist_matrix)-1)*sum(apply(dist_matrix, 2,var))
for (i in 2:13) wss[i]=sum(kmeans(dist,centers = i)$withinss)
plot(1:13,wss, type="b",xlab="Number of clusters",ylab="Within sum of squares")  
kmeanscluster3$cluster
kmeans_matrix=kmeanscluster3$cluster
kmeans_matrix
kmeans_DF=data.frame(wine,kmeans_matrix)
aggregate(kmeans_DF[,2:13],by=list(kmeans_matrix),FUN=mean)
View(kmeans_DF)
kmeansclustering=kmeans_DF %>% select(kmeans_matrix,everything())
View(kmeansclustering)
PLOT=clara(dist_matrix,5)
clusplot(PLOT)




