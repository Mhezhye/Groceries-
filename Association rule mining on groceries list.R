#import dataset
setwd("C:/Users/Mhezhye/Desktop/UoS/ASDM/COURSEWORK/Task 2")
Groc<- read.csv("Groceries_datasetR.csv", header=TRUE)

#Inspect dataset
names(Groc)
head(Groc)
tail(Groc)
summary(Groc)
str(Groc)

#check dimensions
dim(Groc)

library(reshape)

#import reshape to change data from long to wide format
Groc<- reshape(Groc, idvar= "Member_number", timevar = "itemDescription" ,direction="wide")
Groc<- replace(Groc,is.na(Groc),0)

#export as csv to desktop
write.csv(Groc, "C:/Users/Mhezhye/Desktop/UoS/ASDM/COURSEWORK/Task 2/Groceries_1.csv",row.names = FALSE)

#read the new wide format data in
Groc<-read.csv("Groceries_1.csv", header=TRUE, colClasses = "factor")

#Inspect dataset
names(Groc)
head(Groc)
tail(Groc)
summary(Groc)
str(Groc)

#check dimension
dim(Groc)

#set member_number column to null
Groc$Member_number<-NULL

#Compute sum of columns
Yes<-colSums(Groc=="1")
Yes

No<-colSums(Groc=="0")
No

Achete<-rbind(Yes,No)
Achete

#plot and explore with barplot() function
barplot(Achete,legend=rownames(Achete))
barplot(Achete, beside=T, legend=rownames(Achete))

#Install arules for basket analysis
install.packages("arules")
library(arules)

#use apriori() function to create frequent itemsets
ApRules<-apriori(Groc)

#get summary and inspect rules
summary(ApRules)
inspect(ApRules)

#get minimum rules with the code below
ApRules1<-apriori(Groc, parameter=list(minlen=2, maxlen=4, conf=0.95))

#get summary and inspect 
summary(ApRules1)
inspect(ApRules1)

#find most popular item
summary(Groc)
Aprules2<-apriori(Groc, parameter=list(minlen=2, maxlen= 3, conf=0.50), appearance=list(rhs=c("Purchased.whole.milk=1"),default="lhs"))

#get summary and inspect Aprules2
summary(Aprules2)
inspect(Aprules2)

#install arulesviz for visualization of itemsets and association rules
install.packages("arulesViz")
library(arulesViz)

#plot Aprules2 in groups
plot(Aprules2)
plot(Aprules2, method="grouped")

#plot scatterplot matrix 
plot(Aprules2@quality)

#I want to create rules with only purchase items
Aprules3<-apriori(Groc, parameter=list(minlen=2, maxlen=3, conf= 0.50), appearance=list(rhs=c("Purchased.whole.milk=1"),
                                                                                          lhs=c("Purchased.tropical.fruit=1","Purchased.bottled.water=1","Purchased.citrus.fruit=1",
                                                                                                "Purchased.pip.fruit=1","Purchased.sausage=1", "Purchased.root.vegetables=1", "Purchased.pastry=1",
                                                                                                "Purchased.other.vegetables=1", "Purchased.canned.beer=1","Purchased.pork=1", "Purchased.brown.bread=1",
                                                                                                "Purchased.rolls.buns=1","Purchased.frankfurter=1",
#get summary and inspect rules                                                                                                "Purchased.yogurt=1", "Purchased.soda=1")))
summary(Aprules3)
inspect(Aprules3)

# Plot rules in group                                                                                               
plot(Aprules3)
plot(Aprules3, method="grouped")

#plot scatterplot matrix 
plot(Aprules3@quality)

#you can use ruleExplorer() function to explore association rules interactively                                                                                              
ruleExplorer(Aprules3)


 
