groceries=read.csv(file.choose())
attach(groceries)
summary(groceries)
library(arules)
library(arulesViz)
rules=apriori(groceries,parameter = list(support=0.002,confidence=0.75,minlen=2))
inspect(rules)
inspect(head(rules))
inspect(head(sort(rules,by="confidence")))
head(quality(rules))
plot(rules,measure = "confidence")
plot(rules,method = "grouped",measure = "confidence")
#semi.finished.bread=tropical.fruit is sold with margarin=pip.fruit and citrusfruit=citrusfruit
plot(rules,method = "graph",shading = "confidence")
rules1=apriori(groceries,parameter = list(support=0.002,confidence=0.5,minlen=3))
inspect(rules1)
inspect(head(rules1))
inspect(head(sort(rules1,by="lift")))
head(quality(rules1))
plot(rules1,measure = "lift")
plot(rules1,method = "grouped",measure = "lift")
#semi.finished.bread=tropical.fruit is sold with margarin=pip.fruit and citrusfruit=citrusfruit
plot(rules1,method = "graph",shading = "lift")
rules2=apriori(groceries,parameter = list(support=0.005,confidence=0.25,minlen=2))
inspect(rules2)
inspect(head(rules2))
inspect(head(sort(rules2,by="lift")))
head(quality(rules2))
plot(rules2,measure = "lift")
plot(rules2,method = "grouped",measure = "lift")
#IT is seen mostly citrusfruit is sold with semi.finished.bread
#Citrusfruit=Frankfruter is sold with semi.finished.bread=sausage,citrus.fruit=abarsivecleaner
plot(rules2,method = "graph",shading = "lift")
