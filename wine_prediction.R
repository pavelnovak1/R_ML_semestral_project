library(dplyr)
library(RWeka)
library(caret)
library(rpart.plot)
###### loading data #######

wine.all <- read.csv("winequalityN.csv")

###### analysis ######

head(wine.all)
summary(wine.all)

##### preprocessing #####

wine.all <- wine.all[sample(nrow(wine.all)), ]

wine.all[is.na(wine.all$fixed.acidity), "fixed.acidity"] <- mean(wine.all$fixed.acidity, na.rm = T)
wine.all[is.na(wine.all$volatile.acidity), "volatile.acidity"] <- mean(wine.all$volatile.acidity, na.rm = T)
wine.all[is.na(wine.all$citric.acid), "citric.acid"] <- mean(wine.all$citric.acid, na.rm = T)
wine.all[is.na(wine.all$residual.sugar), "residual.sugar"] <- mean(wine.all$residual.sugar, na.rm = T)
wine.all[is.na(wine.all$chlorides), "chlorides"] <- mean(wine.all$chlorides, na.rm = T)
wine.all[is.na(wine.all$pH), "pH"] <- mean(wine.all$pH, na.rm = T)
wine.all[is.na(wine.all$sulphates), "sulphates"] <- mean(wine.all$sulphates, na.rm = T)

wine.all[(wine.all$quality == 3), "quality"] <- 4
wine.all[(wine.all$quality == 9), "quality"] <- 8
wine.all$quality <- as.factor(wine.all$quality)
plot(wine.all$quality)


summary(wine.all)
plot(wine.all$type)
plot(wine.all$quality)

wine.train <- wine.all[1:2100, ]
wine.test <- wine.all[2101:6497, ]


###### model ######

#model.wine <- J48(quality ~ ., data = wine.train, control = Weka_control(R = T, M = 200))
model.wine <- train(quality ~ ., data = wine.train, method = "J48", preProcess = c("center", "scale"), trControl = trainControl("cv"))
###### evaluation ######

prediction.wine <- predict(model.wine, wine.test)

references.wine <- wine.test$quality

confmat.wine <- table(prediction.wine, references.wine)
confmat.wine

accuracy.wine.2 <- confmat.wine[1:1] + confmat.wine[1,2]
for(i in 2:4){
  for(j in (i-1):(i+1)){
    accuracy.wine.2 <- accuracy.wine.2 + confmat.wine[i,j]
  }
}
accuracy.wine.2 <- accuracy.wine.2 + confmat.wine[5,4] + confmat.wine[5,5]
accuracy.wine.2 <- accuracy.wine.2 / sum(confmat.wine)
accuracy.wine.2

accuracy.wine <- sum(diag(confmat.wine)) / sum(confmat.wine)
accuracy.wine

