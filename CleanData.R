library(readr)
library(caret)

#data <- read_csv("C:\\Users\\Junhao Li\\Dropbox\\CSYE\\Midturn_Project\\train.csv")
data <- read_csv("~/Dropbox/CSYE/Midturn_Project/train.csv")

sum(is.na(data))

feature.names <- names(data)[2:(ncol(data)-1)]

##Missing values:
#Employment_Info_1/Employment_Info_4/Employment_Info_6/
#Insurance_History_5/Family_Hist_2/Family_Hist_3/Family_Hist_4/Family_Hist_5
#Medical_History_1/Medical_History_10/Medical_History_15/Medical_History_24/Medical_History_32

# Fix NA's
for (f in feature.names) {
  if (class(data[[f]])=="integer" || class(data[[f]])=="numeric") {
    mean <- mean(data[[f]], na.rm = T)
    data[[f]][is.na(data[[f]])] <- mean
  }
}

# Convert character columns to ids
for (f in feature.names) {
  if (class(data[[f]])=="character") {
    levels <- unique(data[[f]])
    data[[f]] <- as.integer(factor(data[[f]], levels=levels))
  }
}

#Create Dummy Variables in a Data Frame
catVars <- c('Product_Info_1', 'Product_Info_2', 'Product_Info_3',
             'Product_Info_5', 'Product_Info_6', 'Product_Info_7',
             'Employment_Info_2', 'Employment_Info_3', 'Employment_Info_5',
             'InsuredInfo_1', 'InsuredInfo_2', 'InsuredInfo_3', 'InsuredInfo_4',
             'InsuredInfo_5', 'InsuredInfo_6', 'InsuredInfo_7',
             'Insurance_History_1', 'Insurance_History_2',
             'Insurance_History_3', 'Insurance_History_4',
             'Insurance_History_7', 'Insurance_History_8',
             'Insurance_History_9', 'Family_Hist_1', 'Medical_History_2',
             'Medical_History_3', 'Medical_History_4', 'Medical_History_5',
             'Medical_History_6', 'Medical_History_7', 'Medical_History_8',
             'Medical_History_9', 'Medical_History_11',
             'Medical_History_12', 'Medical_History_13', 'Medical_History_14',
             'Medical_History_16', 'Medical_History_17', 'Medical_History_18',
             'Medical_History_19', 'Medical_History_20', 'Medical_History_21',
             'Medical_History_22', 'Medical_History_23', 'Medical_History_25',
             'Medical_History_26', 'Medical_History_27', 'Medical_History_28',
             'Medical_History_29', 'Medical_History_30', 'Medical_History_31',
             'Medical_History_33', 'Medical_History_34', 'Medical_History_35',
             'Medical_History_36', 'Medical_History_37', 'Medical_History_38',
             'Medical_History_39', 'Medical_History_40', 'Medical_History_41')

#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data

for(var in catVars){
  for(level in unique(data[[var]])){
    data[paste(var, level, sep = "_")] <- ifelse(data[[var]] == level, 1, 0)
  }
  data[[var]] = NULL
}

#Reduce demension 

model <- lm(data$Response ~ ., data = data)
# estimate variable importance
importance <- varImp(model, scale=FALSE)

sum(importance > 1)

for(var in names(data)[1:(ncol(data))]){
  if(var == 'Response' || var == 'Id'){
    print(var)
  }else if(is.na(importance[var,])){
    data[[var]] = NULL;
  } else if(importance[var,] <= 10){  
    data[[var]] = NULL;
  }
}

#Divide top 10% as test
test_size <- floor(0.1 * nrow(data))
test_index <- c(1:test_size)

test <- data[test_index,]
train <- data[-test_index,]

write_csv(data, "~/Dropbox/CSYE/Midturn_Project/result.csv", append = F)
write_csv(train, "~/Dropbox/CSYE/Midturn_Project/result_train1.csv", append = F)
write_csv(test, "~/Dropbox/CSYE/Midturn_Project/result_test1.csv", append = F)

#write_csv(data, "C:\\Users\\Junhao Li\\Dropbox\\CSYE\\Midturn_Project\\result.csv", append = F)
#write_csv(train, "C:\\Users\\Junhao Li\\Dropbox\\CSYE\\Midturn_Project\\result_train.csv", append = F)
#write_csv(test, "C:\\Users\\Junhao Li\\Dropbox\\CSYE\\Midturn_Project\\result_test.csv", append = F)













#--------------------------------------------------------------------------------------------


for(var in row.names(importance)){
  if(importance[var,] <= 5){  
    data[[var]] = NULL;
  }
}


attach(data)

sum(is.na(Id))
sum(is.na(Product_Info_7))
sum(is.na(Ins_Age))
sum(is.na(Ht))
sum(is.na(Wt))
sum(is.na(BMI))
sum(is.na(Employment_Info_6))
sum(is.na(InsuredInfo_6))
sum(is.na(Insurance_History_9))
sum(is.na(Family_Hist_5))
sum(is.na(Medical_History_41))
sum(is.na(Medical_Keyword_48))
sum(is.na(Response))


##Missing values:
#Employment_Info_1/Employment_Info_4/Employment_Info_6/
#Insurance_History_5/Family_Hist_2/Family_Hist_3/Family_Hist_4/Family_Hist_5
#Medical_History_1/Medical_History_10/Medical_History_15/Medical_History_24/Medical_History_32

##Numarical: 
#Employment_Info_1/Employment_Info_4/Employment_Info_6/
#Insurance_History_5/Family_Hist_2/Family_Hist_3/Family_Hist_4/Family_Hist_5
#Medical_History_1/Medical_History_10/Medical_History_15/Medical_History_24/Medical_History_32

##Categorical:
#NA

sum(is.na(Employment_Info_1),is.na(Employment_Info_4),
    is.na(Employment_Info_6),is.na(Insurance_History_5),
    is.na(Family_Hist_2),is.na(Family_Hist_3),
    is.na(Family_Hist_4),is.na(Family_Hist_5),
    is.na(Medical_History_1),is.na(Medical_History_10),
    is.na(Medical_History_15),is.na(Medical_History_24),
    is.na(Medical_History_32))


data$Employment_Info_1[is.na(data$Employment_Info_1)] = mean(data$Employment_Info_1,na.rm = T)
data$Employment_Info_4[is.na(data$Employment_Info_4)] = mean(data$Employment_Info_4,na.rm = T)
data$Employment_Info_6[is.na(data$Employment_Info_6)] = mean(data$Employment_Info_6,na.rm = T)
data$Insurance_History_5[is.na(data$Insurance_History_5)] = mean(data$Insurance_History_5,na.rm = T)
data$Family_Hist_2[is.na(data$Family_Hist_2)] = mean(data$Family_Hist_2,na.rm = T)
data$Family_Hist_3[is.na(data$Family_Hist_3)] = mean(data$Family_Hist_3,na.rm = T)
data$Family_Hist_4[is.na(data$Family_Hist_4)] = mean(data$Family_Hist_4,na.rm = T)
data$Family_Hist_5[is.na(data$Family_Hist_5)] = mean(data$Family_Hist_5,na.rm = T)
data$Medical_History_1[is.na(data$Medical_History_1)] = mean(data$Medical_History_1,na.rm = T)
data$Medical_History_10[is.na(data$Medical_History_10)] = mean(data$Medical_History_10,na.rm = T)
data$Medical_History_15[is.na(data$Medical_History_15)] = mean(data$Medical_History_15,na.rm = T)
data$Medical_History_24[is.na(data$Medical_History_24)] = mean(data$Medical_History_24,na.rm = T)
data$Medical_History_32[is.na(data$Medical_History_32)] = mean(data$Medical_History_32,na.rm = T)

sum(is.na(data))


# Load Dependeicies
install.packages("xgboost")
install.packages("readr")
library(caret)
library(xgboost)
library(readr)

# Set seed
set.seed(1337)

# Load data
train <- read_csv("C:\\Users\\Junhao Li\\Dropbox\\CSYE\\Midturn_Project\\train.csv")

# Categorical Variables
# catVars <- c('Product_Info_1', 'Product_Info_2', 'Product_Info_3',
#              'Product_Info_5', 'Product_Info_6', 'Product_Info_7',
#              'Employment_Info_2', 'Employment_Info_3', 'Employment_Info_5',
#              'InsuredInfo_1', 'InsuredInfo_2', 'InsuredInfo_3', 'InsuredInfo_4',
#              'InsuredInfo_5', 'InsuredInfo_6', 'InsuredInfo_7',
#              'Insurance_History_1', 'Insurance_History_2',
#              'Insurance_History_3', 'Insurance_History_4',
#              'Insurance_History_7', 'Insurance_History_8',
#              'Insurance_History_9', 'Family_Hist_1', 'Medical_History_2',
#              'Medical_History_3', 'Medical_History_4', 'Medical_History_5',
#              'Medical_History_6', 'Medical_History_7', 'Medical_History_8',
#              'Medical_History_9', 'Medical_History_11',
#              'Medical_History_12', 'Medical_History_13', 'Medical_History_14',
#              'Medical_History_16', 'Medical_History_17', 'Medical_History_18',
#              'Medical_History_19', 'Medical_History_20', 'Medical_History_21',
#              'Medical_History_22', 'Medical_History_23', 'Medical_History_25',
#              'Medical_History_26', 'Medical_History_27', 'Medical_History_28',
#              'Medical_History_29', 'Medical_History_30', 'Medical_History_31',
#              'Medical_History_33', 'Medical_History_34', 'Medical_History_35',
#              'Medical_History_36', 'Medical_History_37', 'Medical_History_38',
#              'Medical_History_39', 'Medical_History_40', 'Medical_History_41')
#
# for (var in catVars) {
#   train[[var]] <- factor(train[[var]])
#   test[[var]] <- factor(test[[var]])
# }


# Dummy Variables
dummyVars <- paste('Medical_Keyword_', 1:48, sep = '')

for (var in dummyVars) {
  train[[var]] <- factor(train[[var]])
}

feature.names <- names(train)[2:(ncol(train)-1)]

# Fix NA's
for (f in feature.names) {
  if (class(train[[f]])=="integer" || class(train[[f]])=="numeric") {
    mean <- mean(train[[f]], na.rm = T)
    train[[f]][is.na(train[[f]])] <- mean
  }
}

# Convert character columns to ids
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
  }
}

tra<-train[,feature.names]
h<-sample(nrow(train),12000)
dval<-xgb.DMatrix(data=data.matrix(tra[h,]),label=train$Response[h], missing = NA)
dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=train$Response[-h], missing = NA)
watchlist<-list(val=dval,train=dtrain)

param <- list(  objective           = "reg:linear",
                eta                 = 0.025,
                depth               = 20,
                subsample           = 0.7,
                colsample_bytree    = 0.7,
                min_child_weight    = 3
)

clf <- xgb.train(   params              = param,
                    data                = dtrain,
                    nrounds             = 1000,
                    verbose             = 0,
                    early.stop.round    = 50,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    eval_metric         = 'rmse'
)

importance_matrix <- xgb.importance(feature.names, model = clf)
xgb.plot.importance(importance_matrix)

clf <- xgboost(data        = data.matrix(train[,feature.names]),
               label       = train$Response,
               eta         = 0.025,
               depth       = 20,
               nrounds     = 4000,
               objective   = "reg:linear",
               eval_metric = "rmse",
               colsample_bytree=0.7,
               min_child_weight=3,
               subsample=0.7)

importance_matrix <- xgb.importance(feature.names, model = clf)
xgb.plot.importance(importance_matrix[1:20,])

submission <- data.frame(Id=test$Id)
submission$Response <- as.integer(round(predict(clf, data.matrix(test[,feature.names]))))

submission[submission$Response < 1, "Response"] <- 1
submission[submission$Response > 8, "Response"] <- 8

write_csv(submission, "../submissions/xgboost_starter_4.csv")


#Remove redundent
# ensure the results are repeatable
set.seed(7)
# load the library
install.packages("mlbench")
install.packages("caret")
library(mlbench)
library(caret)
# calculate correlation matrix
correlationMatrix <- cor(data[,2:(ncol(data)-1)])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)


#Rank feature by importance
# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=1, repeats=3)
# train the model
model <- train(data$Response~., data=data, method="lm", preProcess="scale", trControl=control)

model <- lm(data$Response ~ ., data = data)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

sum(importance > 3)