Movies=read.csv(file.choose())
str(Movies)
movies=Movies[6:15]
View(movies)
str(movies)
library(arules)
library(arulesViz)
rules=apriori(as.matrix(movies),parameter = list(support=0.02,confidence=0.8,minlen=3))
rules
inspect(rules)
inspect(head(rules))
plot(rules)
plot(rules,measure = "confidence")
plot(rules,method = "grouped")
#Mostly watched movies are Patriot and gladiator
#Most watched movies with LOTR is gladiator and green.mile
plot(rules,method = "graph")
