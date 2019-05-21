######## loading data #######

library(RWeka)
library(dplyr)

cars.all <- read.csv("car.data")
names(cars.all) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "class")

head(cars.all)
summary(cars.all)

cars.train <- cars.all[1:863, ]
cars.test <- cars.all[864:1727, ]
model.cars <- J48(class ~ ., cars.train, control = Weka_control(R = F, M =70, A = F))

prediction.cars <- predict(model.cars, cars.test)
references.cars <- cars.test$class

confmat.cars <- table(prediction.cars, references.cars)
confmat.cars

accuracy.cars <- sum(diag(confmat.cars)) / sum(confmat.cars)
accuracy.cars