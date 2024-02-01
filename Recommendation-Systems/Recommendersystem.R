Book=read.csv(file.choose())
library(recommenderlab)
library(caTools)
View(Book)
book=Book[,-1]
View(book)
book_DF=as(book,"realRatingMatrix")
book_DF

#Based on Popularity

book_popularity=Recommender(book_DF,method="POPULAR")
bookpopularity1=predict(book_popularity,book_DF,n=1)
list=as(bookpopularity1,"list")
list
#Function for mean
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
#Recomended book is :
getmode(list)

#Using User based collaborative filtering

book_UBCF=Recommender(book_DF,method="UBCF")
bookUBCF=predict(book_UBCF,book_DF,n=1)
list_2=as(bookUBCF,"list")
list_2
#Recomended book is :
getmode(list_2)

#Using Item based collaborative filtering

book_IBCF=Recommender(book_DF,method="IBCF")
bookIBCF=predict(book_UBCF,book_DF,n=1)
list_3=as(bookIBCF,"list")
list_3
#Recomended book is :
getmode(list_3)

#Using SVD 

book_SVD=Recommender(book_DF,method="SVD")
bookSVD=predict(book_UBCF,book_DF,n=1)
list_3=as(bookSVD,"list")
list_3
#Recomended book is:
getmode(list_3)

#Using Alternating least square method

book_ALS=Recommender(book_DF,method="ALS")
bookALS=predict(book_ALS,book_DF,n=1)
list_4=as(bookALS,"list")
list_4
#Recomended book is:
getmode(list_4)

