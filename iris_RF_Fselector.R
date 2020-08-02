rm(list=ls())
library(FSelector)
library(randomForest)

data(iris)

weights <- information.gain(Species~., iris)

print(weights)

subset <- cutoff.k(weights, 2)

f <- as.simple.formula(subset, "Species")

print(f)

selection_iris <- random.forest.importance(Species ~ ., iris)
print(selection_iris)

################ Modelo RandomForest
prob <- sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
(sum(prob==1)*100) / nrow(iris)
(sum(prob==2)*100) / nrow(iris)
trainData <- iris[prob==1,]
testData <- iris[prob==2,]


iris_rf_petal_len <- randomForest(Species ~ Petal.Length,data=trainData,ntree=100)
table(predict(iris_rf_petal_len),trainData$Species)

iris_rf_var_select <- randomForest(Species ~ etal.Length + Petal.Width,data=trainData,ntree=100)
table(predict(iris_rf_var_select),trainData$Species)

iris_rf_all <- randomForest(Species~.,data=trainData,ntree=100)
table(predict(iris_rf_all),trainData$Species)


#### Post Interesante
#### https://philjette.wordpress.com/2015/05/31/feature-selection-using-information-gain-in-r/

