library(RWeka)
library(dplyr)

zoo.all <- read.csv("zoo.csv")

head(zoo.all)
summary(zoo.all)
plot(zoo.all)

####### preprocessing #######

zoo.all$class_type <- as.factor(zoo.all$class_type)
zoo.train <- zoo.all[1:50, ]
zoo.test <- zoo.all[51:101, ]

head(zoo.train)
head(zoo.test)

###### model train ######

model <- J48(class_type ~ ., data = zoo.train, control = Weka_control(R = T, M = 3) )
plot(model)

###### model test ######

prediction <- predict(model, newdata = zoo.test)

###### model evaluation #####

references <- zoo.test$class_type

confmat <- table(prediction, references)
confmat

accuracy <- sum(diag(confmat)) / sum(confmat)
accuracy