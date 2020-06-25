# quiz 2

## Q1
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
library(caret)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-trainIndex,]
testing = adData[trainIndex,]

## Q2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain <- createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

library(GGally)
library(Hmisc)
t2 <- training
t2$CompressiveStrength <-cut2(t2$CompressiveStrength, g=4)
ggpairs(data=t2, columns = c("FlyAsh","Age","CompressiveStrength"),
        mapping = ggplot2::aes(colour=CompressiveStrength))


## Q3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(training$Superplasticizer)
hist(log(training$Superplasticizer+1))


# Q4. 
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]
# select out columes start with "IL"
b <- training[,grep("^IL", colnames((training)))]

preProcess(b, method="pca", thresh = 0.9)
# below are additional code for exploring the data
c <- as.vector(names(b))
d <- as.formula(paste("diagnosis ~ ", paste(c, collapse="+")))
model <- train(d, method="glm",
               preProcess="pca", data=training)
confusionMatrix(testing$diagnosis, predict(model, testing))

# Q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# select all columns start with "IL" in the training and testing set, and add diagnosis
trainingIL <- training[,grep("^IL|diagnosis", colnames((training)))]
testingIL <- testing[,grep("^IL|diagnosis", colnames((testing)))]

## model1 -- using all variables
model1 <- train(diagnosis~., method="glm",
               data=trainingIL)
predict_model1 <- predict(model1, testingIL)
confusionMatrix(predict_model1, testingIL$diagnosis)$overall[1]

## model2 --- using PCA 80%

model2 <- train(diagnosis ~., method="glm",
                data=trainingIL, preProcess="pca", 
                trControl=trainControl(preProcOptions=list(thresh=0.8)))
confusionMatrix(testingIL$diagnosis, predict(model2, testingIL))$overall[1]


