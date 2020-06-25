packageVersion(c("AppliedPredictiveModeling","caret","pgmm","rport"))


# Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
## 1. Subset the data to a training set and testing set based on the Case variable in the data set.
trainIndex3 <- createDataPartition(segmentationOriginal$Case, list=FALSE)
training3 <- adData[-trainIndex,]
testing3 = adData[trainIndex,]

## 2. Set the seed to 125 and fit a CART model to predict Class with the rpart method using all predictor variables and default caret settings.
set.seed(125)
modelfit <- train(Class ~., method=rpart, data= training)

## 3. In the final model what would be the final model prediction for cases with the following variable values:
newdata <- data.frame(TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2)
predict(modelfit,newdata)







