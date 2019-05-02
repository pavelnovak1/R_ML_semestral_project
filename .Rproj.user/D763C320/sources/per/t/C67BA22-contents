library(dplyr)
library(RWeka)
library(rpart)
library(rpart.plot)

setwd("C:\\Users\\pavel\\Documents\\Škola\\Projects\\R_projects\\datasets")

titanic_train <- read.csv("titanic_train.csv")
titanic_test <- read.csv("titanic_test.csv")

titanic_train$Survived <- as.factor(titanic_train$Survived)
titanic_test$Survived <- as.factor(titanic_test$Survived)
titanic_train$Embarked <- as.factor(titanic_train$Embarked)
titanic_test$Embarked <- as.factor(titanic_test$Embarked)
titanic_train$Age[is.na(titanic_train$Age)] <- mean(titanic_train$Age, na.rm = T)
titanic_test$Age[is.na(titanic_test$Age)] <- mean(titanic_test$Age, na.rm = T)

titanic_train <- select(titanic_train, Survived, Pclass, Sex, Age)
titanic_test <- select(titanic_test, Survived, Pclass, Sex, Age)

model.J48 <- J48(Survived ~ ., titanic_train)
prediction.J48 <- predict(model.J48, newdata = titanic_test)

conf_mat.J48 <- table(prediction.J48, titanic_test$Survived)
conf_mat.J48

accuracy.J48 <- sum(diag(conf_mat.J48)) / sum(conf_mat.J48)
accuracy.J48

model.rpart <- rpart(Survived ~ ., data = titanic_train, method = "class")
rpart.plot(model.rpart)

prediction.rpart <- predict(model.rpart, newdata = titanic_test)

conf_mat.rpart <- table(prediction.rpart, titanic_test$Survived)
conf_mat.rpart

accuracy.rpart <- sum(diag(conf_mat.rpart)) / sum(conf_mat.rpart)
accuracy.rpart