#install.packages("caret")
#install.packages("readr")
#install.packages("rpart")
#install.packages("Metrics")
library(Metrics)
library(caret)
library(readr)
library(rpart)


#train_data <- read_csv("C:\\Users\\Junhao Li\\Dropbox\\CSYE\\Midturn_Project\\result_train.csv")
#test_data <- read_csv("C:\\Users\\Junhao Li\\Dropbox\\CSYE\\Midturn_Project\\result_test.csv")
train_data <- read_csv("~/Dropbox/CSYE/Midturn_Project/result_train.csv")
test_data <- read_csv("~/Dropbox/CSYE/Midturn_Project/result_test.csv")

train_Id <- train_data$Id
test_Id <- test_data$Id
train_data[["Id"]] = NULL
test_data[["Id"]] = NULL


fit <- rpart(train_data$Response ~ ., data = train_data, method = "anova", control=rpart.control(minsplit=30, cp=0.0001) )

printcp(fit)
plotcp(fit) # visualize cross-validation results 
print(fit)
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow = c(1,2)) # two plots on one page
rsq.rpart(fit) # visualize cross-validation results

# plot tree 
plot(fit, uniform = T, main = "Regression Tree")
text(fit, use.n = T, all = T, cex = .8)

# prune the tree
pfit <- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=T, main="Pruned Regression Tree")
text(pfit, use.n=T, all=T, cex=.8)

predict <- predict(pfit, newdata = test_data)

submit <- data.frame (Id = test_Id, Response = predict)
write_csv(submit, "~/Dropbox/CSYE/Midturn_Project/RegeressionTreeSubmit.csv", append = F)

ScoreQuadraticWeightedKappa(test_data$Response,predict)
