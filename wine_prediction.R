library(dplyr)
library(RWeka)
###### loading data #######

wine.all <- read.csv("winequalityN.csv")

###### analysis ######

head(wine.all)
summary(wine.all)

##### preprocessing #####

wine.all <- wine.all[sample(nrow(wine.all)), ]
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

model <- J48(quality ~ ., data = wine.train, control = Weka_control(R = F, M = 200, A = F))
prediction <- predict(model, wine.test)

references <- wine.test$quality

confmat <- table(prediction, references)
confmat

accuracy2 <- confmat[1:1] + confmat[1,2]
for(i in 2:6){
  for(j in (i-1):(i+1)){
    accuracy2 <- accuracy2 + confmat[i,j]
  }
}
accuracy2 <- accuracy2 + confmat[7,6] + confmat[7,7]
accuracy2 <- accuracy2 / sum(confmat)
accuracy2

accuracy <- sum(diag(confmat)) / sum(confmat)
accuracy

