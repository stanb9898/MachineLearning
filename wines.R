#1. Prepare problem
# a) Load libraries
library(ggplot2)
library(mlbench)
library(caret)
library(lattice)
library(e1071)
library(corrplot)
library(correlation)
library(randomForest)
library(rpart)
library(rpart.plot)

# b) Load data

filename <- "wines.csv"
data <- read.csv(filename)
data1 <- data[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides","free.sulfur.dioxide","density","pH","sulphates","alcohol","quality") ]

# split into train
train_index <- sample(x=1:nrow(data), size=0.8*nrow(data))
train = data[train_index,]
test = data[-train_index,]


# 2. Summarize Data
# a) Descriptive statistics
# peek at data
head(data)

View(data)

tail(data)

summary(data)

str(data)

sapply(data, class)

#if any data is unavailable

anyNA(data)

#omit rows with NA variables
data <- na.omit(data)
data1 <- na.omit(data1)

# data dimension
dim(data)


# b) Data visualizations
# Univariate Visualization

#bar plot
ggplot(data, aes(fixed.acidity)) + geom_bar(fill="black")
ggplot(data, aes(type)) + geom_bar(fill="black")
ggplot(data = data) + geom_bar(mapping = aes(x = quality), fill="gray")
ggplot(data, aes(pH)) + geom_bar(fill="black")


# 3. Prepare data

ggplot(data = data) + geom_bar(mapping = aes(x = quality), fill="gray")

#clasify the wine by taste

data$taste <- ifelse(data$quality < 6, 'bad', 'good')
data$taste[data$quality == 6] <- 'normal'
data$taste <- as.factor(data$taste)
table(data$taste)

# split into train
train_index <- sample(x=1:nrow(data), size=0.8*nrow(data))
train = data[train_index,]
test = data[-train_index,]


# 4. Evaluate Algorithms
# a)compare algorithms


#Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


#GLMNET
set.seed(7)
fit.glmnet <- train(taste~.-quality, data=train, method="glmnet", metric=metric, trControl=control)


#knn
set.seed(7)
fit.knn <- train(taste~.-quality, data=train, method="knn", metric=metric, trControl=control)

#Naive Bayes 
set.seed(7)
fit.nb <- train(taste~.-quality,data=train,method="nb",metric=metric, trControl=control)


#randomForest 
set.seed(7)
fit.rf <- train(taste~.-quality, data=train, method="rf", metric=metric, trControl=control)


#Compare algorithms 
transform_results <- resamples(list(GLMNET=fit.glmnet, KNN=fit.knn,NVM=fit.nb, RF =fit.rf))
summary(transform_results)
dotplot(transform_results)


# look at parameters used for Random forest--best model
print(fit.rf)
x <- test[,1:13]
y <- test[,14]

predictions <- predict(fit.rf, newdata=x)
print(predictions)

#for classification problem - view accuracy
confusionMatrix(finalPredictions, y)


# save the model to disk
saveRDS(fit.rf, "randomForest.rds")


#use the model for prediction
model <- readRDS("randomForest.rds")


# make a predictions on "new data" using the final model
finalPredictions <- predict(model, x)
print(finalPredictions)
model <- randomForest(taste ~ . - quality, data = train)
model
prediction <- predict(model, newdata = test)





