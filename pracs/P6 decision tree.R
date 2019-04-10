#classification tree
library(MASS)
library(rpart)
data(biopsy)
biopsy=biopsy[,-1] #delete ID
names(biopsy)=c("thick","u.size","u.shape","adhsn","s.size","nucl","chrom","n.nuc","mit","class") #change the feature names
biopsy.v2=na.omit(biopsy)#delete the observations with missing values
set.seed(123)#random number generator
ind=sample(2,nrow(biopsy.v2),replace = TRUE,prob =c(0.7,0.3))
biop.train=biopsy.v2[ind==1,] #the training data set
biop.test=biopsy.v2[ind==2,] #the test data set
str(biop.test[,10])
set.seed(123)
tree.biop=rpart(class~.,method="class",data=biop.train)
print(tree.biop$cptable)
cp=min(tree.biop$cptable[2,])
prune.tree.biop=prune(tree.biop,cp=cp)

library(partykit)
plot(as.party(tree.biop))
plot(as.party(prune.tree.biop))
rparty.test=predict(prune.tree.biop,newdata=biop.test,type="class")
table(rparty.test,biop.test$class)

#regression tree
library(rpart) #classification and regression trees
library(partykit) #treeplots
library(MASS) #breast and pima indian data
#library(randomForest) #random forests
#library(gbm) #gradient boosting
library(caret)
data(biopsy)
biopsy = biopsy[,-1] #delete ID
names(biopsy)=c("thick","u.size","u.shape","adhsn","s.size","nucl","chrom","n.nuc","mit","class") #change the feature names
biopsy.v2=na.omit(biopsy)#delete the observations with missing values
set.seed(123)#random number generator
ind=sample(2,nrow(biopsy.v2),replace = TRUE,prob =c(0.7,0.3))
biop.train=biopsy.v2[ind==1,] #the training data set
biop.test=biopsy.v2[ind==2,] #the test data set
str(biop.test[,10])
set.seed(123)
tree.biop=rpart(adhsn~.,data=biop.train)
print(tree.biop$cptable)
plotcp(tree.biop)
cp=min(tree.biop$cptable[7,])
prune.tree.biop=prune(tree.biop,cp=cp)
plot(as.party(tree.biop))
plot(as.party(prune.tree.biop))
party.biop.test=predict(prune.tree.biop,newdata=biop.test)
rpart.resid=party.biop.test-biop.test$adhsn #calculate residuals
mean(rpart.resid^2) #calculate MSE



