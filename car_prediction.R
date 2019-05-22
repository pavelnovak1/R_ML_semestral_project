######## loading data #######

library(RWeka)
library(dplyr)

cars.all <- read.csv("car.data")
names(cars.all) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "class")

head(cars.all)
summary(cars.all)

cars.all <- cars.all[sample(nrow(cars.all)), ]
cars.train <- cars.all[1:(nrow(cars.all)*0.7), ]
cars.test <- cars.all[(nrow(cars.all)*0.7):nrow(cars.all), ]
model.cars <- J48(class ~ ., cars.train, control = Weka_control(R = T, M = 1))
prediction.cars <- predict(model.cars, cars.test)
references.cars <- cars.test$class

confmat.cars <- table(prediction.cars, references.cars)
confmat.cars

accuracy.cars <- sum(diag(confmat.cars)) / sum(confmat.cars)
accuracy.cars