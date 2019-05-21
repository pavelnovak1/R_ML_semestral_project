####### loading data #######
library(RWeka)
mushrooms.all <- read.csv("mushrooms.csv")

######## analysis ########

head(mushrooms.all)
summary(mushrooms.all)

###### preprocessing ########

mushrooms.all <- mushrooms.all[sample(nrow(mushrooms.all)), ]
mushrooms.train <- mushrooms.all[1:4062, ]
mushrooms.test <- mushrooms.all[4063:8124, ]


##### model #####
model.mushrooms <- J48(class ~ ., mushrooms.train, control = Weka_control(R = T))

prediction.mushrooms <- predict(model.mushrooms, mushrooms.test)

references.mushrooms <- mushrooms.test$class

confmat.mushrooms <- table(prediction.mushrooms, references.mushrooms)
confmat.mushrooms

accuracy.mushrooms <- sum(diag(confmat.mushrooms)) / sum(confmat.mushrooms)
accuracy.mushrooms