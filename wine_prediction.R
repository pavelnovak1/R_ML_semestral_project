###### loading data #######

wine.all <- read.csv("winequalityN.csv")

###### analysis ######

head(wine.all)
summary(wine.all)

##### preprocessing #####

wine.all <- wine.all[select(nrow(wine.all)), ]
wine.all$quality <- as.factor(wine.all$quality)

wine.all[is.na(wine.all$fixed.acidity), "fixed.acidity"] <- mean(wine.all$fixed.acidity, na.rm = T)
wine.all[is.na(wine.all$volatile.acidity), "volatile.acidity"] <- mean(wine.all$volatile.acidity, na.rm = T)
wine.all[is.na(wine.all$citric.acid), "citric.acid"] <- mean(wine.all$citric.acid, na.rm = T)
wine.all[is.na(wine.all$residual.sugar), "residual.sugar"] <- mean(wine.all$residual.sugar, na.rm = T)
wine.all[is.na(wine.all$chlorides), "chlorides"] <- mean(wine.all$chlorides, na.rm = T)
wine.all[is.na(wine.all$pH), "pH"] <- mean(wine.all$pH, na.rm = T)
wine.all[is.na(wine.all$sulphates), "sulphates"] <- mean(wine.all$sulphates, na.rm = T)

summary(wine.all)
plot(wine.all$type)
plot(wine.all$quality)

wine.train <- wine.all[1:3250, ]
wine.test <- wine.all[3251:6497, ]


###### model ######

model <- train(type ~ ., data = wine.train, method = "J48")
prediction <- predict(model, wine.test)

confmat <- table(prediction, wine.test$quality)
confmat

accuracy <- sum(diag(confmat)) / sum(confmat)
accuracy

