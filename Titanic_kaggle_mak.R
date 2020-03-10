titanic_train=read.csv("D://train_tt.csv")
head(titanic_train)
tail(titanic_train)
str(titanic_train)

summary(titanic_train$Age)
#Replace NA in age
mean.Age <- mean(titanic_train$Age, na.rm=TRUE)
titanic_train$Age[is.na(titanic_train$Age)] = mean.Age
mean.Age
mean(titanic_train$Age)
summary(titanic_train)

titanic_test=read.csv("D://test_tt.csv")
head(titanic_test)
tail(titanic_test)
str(titanic_test)

summary(titanic_test$Age)
#Replace NA in age
mean.Age_test <- mean(titanic_test$Age, na.rm=TRUE)
titanic_test$Age[is.na(titanic_test$Age)] = mean.Age_test
mean.Age_test
mean(titanic_test$Age)
summary(titanic_test)





#Drop variables  name, ticket, cabin,  home.dest
titanic_1 <- titanic_train[,c(1,2,3,5,6,7,8)]
str(titanic_1)

titanic_train
titanic_test
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
install.packages("tree")
library(tree)

dtree=rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = titanic_train, method = "class")
plot(dtree)
text(dtree)
fancyRpartPlot(dtree)
prediction <- predict(dtree, titanic_test, type = "class")
solution <- data.frame(PassengerID = titanic_test$PassengerId, Survived = prediction)
write.csv(solution, file = 'dtree_solution.csv', row.names = F)


p=predict(dtree, titanic_test, type = "class")
table(titanic_test$Survived,prediction)
prop.table(table(titanic_test$Survived,p))
table_conf<- table(test$survived,p)
table_conf
#Accuracy of the model
Accuracy <-  sum(diag(table_conf)) / sum(table_conf)
Accuracy
dtree_1 <- rpart(survived ~ pclass + sex + age + sibsp + parch + fare + embarked, data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))
fancyRpartPlot(dtree_1)
p1=predict(dtree_1, test, type = "class")
table_conf_1 <-  table(test$survived,p1)
table_conf_1
Accuracy_1 <-  sum(diag(table_conf_1)) / sum(table_conf_1)
Accuracy_1
prediction <- predict(dtree_1, test, type = "class")
solution <- data.frame(PassengerID = test$srno, Survived = prediction)
write.csv(solution, file = 'dtree_1_solution.csv', row.names = F)
