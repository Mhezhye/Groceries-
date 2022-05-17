setwd("C:/Users/Mhezhye/Desktop/UoS/ASDM/COURSEWORK/Task 2")
Groc<- read.csv("Groceries_datasetR.csv", header=TRUE)


names(Groc)
head(Groc)
tail(Groc)
summary(Groc)
str(Groc)

dim(Groc)

library(reshape)


Groc<- reshape(Groc, idvar= "Member_number", timevar = "itemDescription" ,direction="wide")
Groc<- replace(Groc,is.na(Groc),0)


write.csv(Groc, "C:/Users/Mhezhye/Desktop/UoS/ASDM/COURSEWORK/Task 2/Groceries_1.csv",row.names = FALSE)

Groc<-read.csv("Groceries_1.csv", header=TRUE, colClasses = "factor")

names(Groc)
head(Groc)
tail(Groc)
summary(Groc)
str(Groc)

dim(Groc)

Groc$Member_number<-NULL

Yes<-colSums(Groc=="1")
Yes

No<-colSums(Groc=="0")
No

Achete<-rbind(Yes,No)
Achete

barplot(Achete,legend=rownames(Achete))
barplot(Achete, beside=T, legend=rownames(Achete))

install.packages("arules")
library(arules)

ApRules<-apriori(Groc)

summary(ApRules)
inspect(ApRules)

ApRules1<-apriori(Groc, parameter=list(minlen=2, maxlen=4, conf=0.95))

summary(ApRules1)
inspect(ApRules1)

summary(Groc)
Aprules2<-apriori(Groc, parameter=list(minlen=2, maxlen= 3, conf=0.50), appearance=list(rhs=c("Purchased.whole.milk=1"),default="lhs"))

summary(Aprules2)
inspect(Aprules2)

install.packages("arulesViz")
library(arulesViz)

plot(Aprules2)
plot(Aprules2, method="grouped")

plot(Aprules2@quality)

Aprules3<-apriori(Groc, parameter=list(minlen=2, maxlen=3, conf= 0.50), appearance=list(rhs=c("Purchased.whole.milk=1"),
                                                                                          lhs=c("Purchased.tropical.fruit=1","Purchased.bottled.water=1","Purchased.citrus.fruit=1",
                                                                                                "Purchased.pip.fruit=1","Purchased.sausage=1", "Purchased.root.vegetables=1", "Purchased.pastry=1",
                                                                                                "Purchased.other.vegetables=1", "Purchased.canned.beer=1","Purchased.pork=1", "Purchased.brown.bread=1",
                                                                                                "Purchased.rolls.buns=1","Purchased.frankfurter=1",
                                                                                                "Purchased.yogurt=1", "Purchased.soda=1")))
summary(Aprules3)
inspect(Aprules3)

plot(Aprules3)
plot(Aprules3, method="grouped")

plot(Aprules3@quality)

ruleExplorer(Aprules3)


 