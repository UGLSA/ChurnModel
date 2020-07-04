#Load libraries
library(class)
library(gmodels)
library(lattice)
library(caret)

#Load the data set
churn_dataset <- read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv', stringsAsFactors = TRUE)

#check structure of the dataset
str(churn_dataset)

#removes the first variable(customerID) from the data set.
churn_dataset <- churn_dataset[-1] 

#check structure of the dataset
str(churn_dataset)

#check for missing values
sapply(churn_dataset, function(x) sum(is.na(x)))

#remove all rows with missing values
churn_dataset <- churn_dataset[complete.cases(churn_dataset), ] 

#check values are removed
sapply(churn_dataset, function(x) sum(is.na(x)))

# logical vector telling if a variable needs to be displayed as numeric
must_convert<-sapply(churn_dataset,is.factor)      
# data.frame of all categorical variables now displayed as numeric
M2<-sapply(churn_dataset[,must_convert],unclass)    
churn<-cbind(churn_dataset[,!must_convert],M2)


#check structure of the dataset
str(churn)

#Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

#normalized data set in the 'loan.subset.n' variable and also we're removing the 'Churn' variable
churn_normalized <- as.data.frame(lapply(churn[,1:19], normalize))

str(churn_normalized)

set.seed(1234)
#Split dataset 2/3 & 1/3
indx = sample(2, nrow(churn_normalized), replace=TRUE, prob=c(0.67, 0.33)) 
churn_normalized.train = churn_normalized[indx==1,1:19] #Where sample index is 1
churn_normalized.test = churn_normalized[indx==2,1:19] #Where sample index is 19


churn_normalized.trainLabels = churn[indx==1,20]
churn_normalized.testLabels = churn[indx==2, 20]


churn_normalized.pred3 = knn(train = churn_normalized.train, test = churn_normalized.test, cl = churn_normalized.trainLabels, k=83)
churn_normalized.pred3


CrossTable(x = churn_normalized.testLabels, y = churn_normalized.pred3, prop.chisq=FALSE)
confusionMatrix(table(churn_normalized.pred3 ,churn_normalized.testLabels))