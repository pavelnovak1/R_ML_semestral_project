########## loading data ########

students.all <- read.csv("StudentsPerformance.csv")

######### analysis ##########

head(students.all)
summary(students.all)

######### preprocessing #########

students.train <- students.all[1:500, ]
students.test <- students.all[501:1000, ]


######### model #########

model.students <- J48(gender ~ ., data = students.train, control = Weka_control(R = F, M = 10, A = T))

######## model evaluation ########

prediction.students <- predict(model.students, students.test)
references.students <- students.test$gender

confmat.students <- table(prediction.students, references.students)
confmat.students

accuracy.students <- sum(diag(confmat.students)) / sum(confmat.students)
accuracy.students
