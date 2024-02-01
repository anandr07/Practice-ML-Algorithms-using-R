#Hierarchical clustring
Crime_data=read.csv(file.choose())
View(Crime_data)
str(Crime_data)
library(cluster)
norm_data=scale(Crime_data[2:5])
norm_data
dist=dist(norm_data,method = "euclidean")
hclust_crime=hclust(dist,method = "complete")
plot(hclust_crime)
plot(hclust_crime,hang = -1)
hclust_crime1=cutree(hclust_crime,k=4)
rect.hclust(hclust_crime,k=4,border = "red")
cluster=as.matrix(hclust_crime1)
New_data=data.frame(Crime_data,cluster)
aggregate(New_data,list(New_data$cluster),mean)
library(dplyr)
New_crimedata=New_data[,c(ncol(New_data),1:(ncol(New_data)-1))]
library(factoextra)
fviz_dend(hclust_crime,k=4,rect = TRUE,rect_border = "red",rect_fill = TRUE,ggtheme = theme_gray())



