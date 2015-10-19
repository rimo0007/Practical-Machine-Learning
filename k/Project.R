library(caret)
library(ggplot2)
library(knitr)
library(randomForest)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

#Get the URL of the DATASET
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"


training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

set.seed(12345)

inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]
myTesting <- training[-inTrain, ]
dim(myTraining)
dim(myTesting)

#Cleaning the data 
#Remove NearZeroVariance variables from the training and test set both
nsv <- nearZeroVar(myTraining, saveMetrics=TRUE)
myTraining <- myTraining[,nsv$nzv==FALSE]

nsv<- nearZeroVar(myTesting,saveMetrics=TRUE)
#myTesting <- myTesting[,nsv$nzv==FALSE]

#Remove the first column of the myTraining data set because it is a  ID variable so that it does not interfer with ML MODEL.
myTraining <- myTraining[c(-1)]


#Cleaning Variables with 80% or more NAs. For Variables that have more than a 80% threshold of NA's I'm going to remove them.
temp <- myTraining #creating another subset to iterate in loop
for(i in 1:length(myTraining)) { #for every column in the training dataset
  if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .8 ) { #if n?? NAs > 80% of total observations
    for(j in 1:length(temp)) {
      if( length( grep(names(myTraining[i]), names(temp)[j]) ) ==1)  { #if the columns are the same:
        temp <- temp[ , -j] #Remove that column
      }   
    } 
  }
}
#To check the new N?? of observations
dim(temp)
myTraining <- temp
rm(temp)

# I will take only those columns that I have taken for My Training set
myTesting <- myTesting[colnames(myTraining)]

col<- colnames(myTraining[, -58]) #Column name without class name
testing <- testing[col] 


#Coerce the data into the same type so that Algorithm works properly and gives a good accuracy so, change all the column types of test  data
#into the type of training data.

for (i in 1:length(testing) ) {
  for(j in 1:length(myTraining)) {
    if( length( grep(names(myTraining[i]), names(testing)[j]) ) == 1)  {
      class(testing[j]) <- class(myTraining[i])
    }      
  }      
}

#And to make sure Coertion really worked just add a row of the training set to it and then delete it
testing <- rbind(myTraining[2, -58] , testing)
testing <- testing[-1,]


#Prediction with Decision Trees : Model 1

modFitA1 <- rpart(classe ~ ., data=myTraining, method="class")
fancyRpartPlot(modFitA1)
predictionsA1 <- predict(modFitA1, myTesting, type = "class")
cmtree <- confusionMatrix(predictionsA1, myTesting$classe)



#Prediction with Random Forests
set.seed(12345)
modFitB1 <- randomForest(classe ~ ., data=myTraining)
predictionB1 <- predict(modFitB1, myTesting, type = "class")
cmrf <- confusionMatrix(predictionB1, myTesting$classe)
cmrf

#Prediction with Generalized Boosted Regression
set.seed(12345)
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 1)

gbmFit1 <- train(classe ~ ., data=myTraining, method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)


gbmFinMod1 <- gbmFit1$finalModel

gbmPredTest <- predict(gbmFit1, newdata=myTesting)
gbmAccuracyTest <- confusionMatrix(gbmPredTest, myTesting$classe)
gbmAccuracyTest


#Predicting Results on the Test Data with Random Forest as Random Forest is giving more accuracy

predictionB2 <- predict(modFitB1, testing, type = "class")
predictionB2

#This Function will write the file for submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionB2)


