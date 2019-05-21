library(RWeka)
library(caret)

###### loading data #######

wine.all <- read.csv("winequalityN.csv")

###### analysis ######

head(wine.all)
summary(wine.all)


### baseline model ####

wine.all$quality <- as.factor(wine.all$quality)
wine.train <- wine.all[1:3248, ]
wine.test <- wine.all[3249:6497, ]

### raw model without any preprocessing and tuning
model.wine.raw <- J48(quality ~ ., data = wine.train)
prediction.wine.raw <- predict(model.wine.raw, newdata = wine.test)
references.wine.raw <- wine.test$quality

confmat.wine.raw <- table(prediction.wine.raw, references.wine.raw)
accuracy.wine.raw <- sum(diag(confmat.wine.raw)) / sum(confmat.wine.raw)
accuracy.wine.raw

##### preprocessing #####

wine.all <- wine.all[sample(nrow(wine.all)), ] #data shufling

## removing missing values

wine.all[is.na(wine.all$fixed.acidity), "fixed.acidity"] <- mean(wine.all$fixed.acidity, na.rm = T)
wine.all[is.na(wine.all$volatile.acidity), "volatile.acidity"] <- mean(wine.all$volatile.acidity, na.rm = T)
wine.all[is.na(wine.all$citric.acid), "citric.acid"] <- mean(wine.all$citric.acid, na.rm = T)
wine.all[is.na(wine.all$residual.sugar), "residual.sugar"] <- mean(wine.all$residual.sugar, na.rm = T)
wine.all[is.na(wine.all$chlorides), "chlorides"] <- mean(wine.all$chlorides, na.rm = T)
wine.all[is.na(wine.all$pH), "pH"] <- mean(wine.all$pH, na.rm = T)
wine.all[is.na(wine.all$sulphates), "sulphates"] <- mean(wine.all$sulphates, na.rm = T)

## merging category no.3 to no.4 and no.9 to no.8
wine.all[(wine.all$quality == 3), "quality"] <- 4
wine.all[(wine.all$quality == 9), "quality"] <- 8
wine.all$quality <- droplevels(wine.all$quality, exclude = c(3,9))
wine.all$quality <- as.factor(wine.all$quality)

wine.train <- wine.all[1:3248, ]
wine.test <- wine.all[3249:6497, ]


###### model ######

model.wine <- J48(quality ~ ., data = wine.train, control = Weka_control(R = F, M = 105, S = F))
###### evaluation ######

prediction.wine <- predict(model.wine, wine.test)

references.wine <- wine.test$quality

confmat.wine <- table(prediction.wine, references.wine)
confmat.wine


accuracy.wine <- sum(diag(confmat.wine)) / sum(confmat.wine)
accuracy.wine

### evaluation with toleration +- 1 class
accuracy.wine.2 <- confmat.wine[1:1] + confmat.wine[1,2]
for(i in 2:4){
  for(j in (i-1):(i+1)){
    accuracy.wine.2 <- accuracy.wine.2 + confmat.wine[i,j]
  }
}
accuracy.wine.2 <- accuracy.wine.2 + confmat.wine[5,4] + confmat.wine[5,5]
accuracy.wine.2 <- accuracy.wine.2 / sum(confmat.wine)
accuracy.wine.2


