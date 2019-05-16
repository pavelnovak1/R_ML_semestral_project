##### loading data #######
library(RWeka)
library(plyr)
library(dplyr)
library(caret)
library(e1071)
library(ipred)

zoo.all <- read.csv("zoo.csv")

head(zoo.all)
summary(zoo.all)

####### preprocessing #######

zoo.all$class_type <- as.factor(zoo.all$class_type)
zoo.all <- zoo.all[sample(nrow(zoo.all)),]
zoo.train <- zoo.all[1:50, ]
zoo.test <- zoo.all[51:101, ]

head(zoo.train)
head(zoo.test)

###### model train ######

model.J48 <- J48(class_type ~ ., data = zoo.train, control = Weka_control(R = T) )
model <- train(class_type ~ ., data = zoo.train, method = "adaboost")

###### model test ######

prediction.J48 <- predict(model.J48, newdata = zoo.test)
prediction <- predict(model, newdata = zoo.test)

###### model evaluation #####

references <- zoo.test$class_type

confmat.J48 <- table(prediction.J48, references)
confmat.J48

confmat <- table(prediction, references)
confmat

accuracy.J48 <- sum(diag(confmat.J48)) / sum(confmat.J48)
accuracy.J48

accuracy <- sum(diag(confmat)) / sum(confmat)
accuracy

##### shufling data #####

acc.plot.J48 <- c()
for(i in 1:100){
  zoo.all <- read.csv("zoo.csv")
  zoo.all$class_type <- as.factor(zoo.all$class_type)
  zoo.all <- zoo.all[sample(nrow(zoo.all)),]
  zoo.train <- zoo.all[1:50, ]
  zoo.test <- zoo.all[51:101, ]
  references <- zoo.test$class_type
  
  model.J48 <- J48(class_type ~ ., data = zoo.train, control = Weka_control(R = F, M = 1, A = T))
  
  prediction.J48 <- predict(model.J48, newdata = zoo.test)
  
  confmat.J48 <- table(prediction.J48, references)
  
  accuracy.J48 <- sum(diag(confmat.J48)) / sum(confmat.J48)
  
  acc.plot.J48 <- c(acc.plot.J48, accuracy.J48)
  
}
summary(acc.plot.J48)
sd(acc.plot.J48)
