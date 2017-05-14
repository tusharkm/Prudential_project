setwd("D:/Big Data Analytics/MidTermProject/result")
library(MASS)
library(xlsx)
library(relaimpo)
library(Metrics)
set.seed(30)
result_train<- read.csv("result_train.csv",header=T)
result_test<- read.csv("result_test.csv",header=T)
str(data)
head(data)
corel<-cor(result_train)

par()
pdf('rplot.pdf')
dev.off()


###################Linear Model for Feature selection

linearmodel<-lm(result_train$Response~.,result_train)
predicted_result <-predict(linearmodel,result_test)
misClasificError <- mean(ceiling(predicted_result) != result_test3$Response)
print(paste('Accuracy',1-misClasificError))

################### Feature selection with all variables
summary(linearmodel)
linearmodel2<-lm(result_train2$Response~.,result_train2)


lmkeep<-c("result_test$Response","result_test$Product_Info_4","result_test$Ins_Age","result_test$Ht","result_test$Wt","result_test$BMI","result_test$Family_Hist_2",
               "result_test$Family_Hist_3","result_test$Family_Hist_4","result_test$Family_Hist_5","result_test$Medical_History_1",
             "result_test$Medical_History_10","result_test$Medical_History_15","result_test$Medical_History_32","result_test$Medical_Keyword_2","result_test$Medical_Keyword_3",
             ",result_test$Medical_Keyword_6","result_test$Medical_Keyword_9","result_test$Medical_Keyword_12","result_test$Medical_Keyword_15","result_test$Medical_Keyword_18
             ","result_test$Medical_Keyword_19","result_test$Medical_Keyword_22","result_test$Medical_Keyword_24","result_test$Medical_Keyword_25","result_test$Medical_Keyword_26
             ","result_test$Medical_Keyword_28","result_test$Medical_Keyword_29","result_test$Medical_Keyword_31","result_test$Medical_Keyword_33","result_test$Medical_Keyword_35","result_test$Medical_Keyword_37","result_test$Medical_Keyword_38
             ","result_test$Medical_Keyword_39","result_test$Medical_Keyword_41","result_test$Medical_Keyword_45","result_test$Product_Info_2_8","result_test$Product_Info_2_9","result_test$Product_Info_2_11","result_test$Product_Info_2_17","result_test$Product_Info_3_9","result_test$Product_Info_3_18
             ","result_test$Product_Info_5_3","result_test$Employment_Info_2_37","result_test$Employment_Info_2_6","result_test$Employment_Info_2_23","result_test$InsuredInfo_1_3","result_test$InsuredInfo_4_2
             ","result_test$InsuredInfo_5_3","result_test$InsuredInfo_7_3","result_test$InsuredInfo_3_3","result_test$Medical_History_2_280","result_test$Medical_History_2_548","result_test$Medical_History_2_532","result_test$Medical_History_2_131",
             "result_test$Medical_History_2_293","result_test$Medical_History_2_39","result_test$Medical_History_2_75","result_test$Medical_History_2_419","result_test$Medical_History_2_184","result_test$Medical_History_2_559","result_test$Medical_History_2_99","result_test$Medical_History_2_476","result_test$Medical_History_2_414","result_test$Medical_History_2_399","result_test$Medical_History_2_636","result_test$Medical_History_2_69","result_test$Medical_History_2_618","result_test$Medical_History_2_37","result_test$Medical_History_2_148","result_test$Medical_History_2_516","result_test$Medical_History_2_231",
             "result_test$Medical_History_2_502","result_test$Medical_History_2_45","result_test$Medical_History_2_97","result_test$Medical_History_2_271","result_test$Medical_History_2_611","result_test$Medical_History_2_384","result_test$Medical_History_2_38","result_test$Medical_History_2_623","result_test$Medical_History_2_582","result_test$Medical_History_2_6",
             "result_test$Medical_History_2_133","result_test$Medical_History_2_498","result_test$Medical_History_2_136","result_test$Medical_History_2_207","result_test$Medical_History_2_400","result_test$Medical_History_3_1","result_test$Medical_History_7_2","result_test$Medical_History_9_3","result_test$Medical_History_11_3","result_test$Medical_History_13_2","result_test$Medical_History_21_3","result_test$Medical_History_34_2",
             ",result_test$Medical_History_37_3","result_test$Medical_History_38_2")



lmkeep<-c("Response","Product_Info_4","Ins_Age","Ht","Wt","BMI","Family_Hist_2",
          "Family_Hist_3","Family_Hist_4","Family_Hist_5","Medical_History_1",
          "Medical_History_10","Medical_History_15","Medical_History_32","Medical_Keyword_2",
          "Medical_Keyword_3","Medical_Keyword_6","Medical_Keyword_9","Medical_Keyword_12","Medical_Keyword_15",
          "Medical_Keyword_18","Medical_Keyword_19","Medical_Keyword_22","Medical_Keyword_24","Medical_Keyword_25",
          "Medical_Keyword_26","Medical_Keyword_28","Medical_Keyword_29","Medical_Keyword_31","Medical_Keyword_33",
          "Medical_Keyword_35","Medical_Keyword_37","Medical_Keyword_38","Medical_Keyword_39","Medical_Keyword_41",
          "Medical_Keyword_45","Product_Info_2_8","Product_Info_2_9","Product_Info_2_11","Product_Info_2_17",
          "Product_Info_3_9","Product_Info_3_18","Product_Info_5_3","Employment_Info_2_37","Employment_Info_2_6",
          "Employment_Info_2_23","InsuredInfo_1_3","InsuredInfo_4_2","InsuredInfo_5_3","InsuredInfo_7_3",
          "InsuredInfo_3_3","Medical_History_2_280","Medical_History_2_548","Medical_History_2_532","Medical_History_2_131",
          "Medical_History_2_293","Medical_History_2_39","Medical_History_2_75","Medical_History_2_419",
          "Medical_History_2_184","Medical_History_2_559","Medical_History_2_99","Medical_History_2_476",
          "Medical_History_2_414","Medical_History_2_399","Medical_History_2_636","Medical_History_2_69",
          "Medical_History_2_618","Medical_History_2_37","Medical_History_2_148","Medical_History_2_516",
          "Medical_History_2_231","Medical_History_2_502","Medical_History_2_45","Medical_History_2_97",
          "Medical_History_2_271","Medical_History_2_611","Medical_History_2_384","Medical_History_2_38",
          "Medical_History_2_623","Medical_History_2_582","Medical_History_2_6","Medical_History_2_133",
          "Medical_History_2_498","Medical_History_2_136","Medical_History_2_207","Medical_History_2_400",
          "Medical_History_3_1","Medical_History_7_2","Medical_History_9_3","Medical_History_11_3","Medical_History_13_2",
          "Medical_History_21_3","Medical_History_34_2","Medical_History_37_3","Medical_History_38_2")

result_test2<-result_test[lmkeep]
result_train2<-result_train[lmkeep]

linearmodel2

#####################################
##Predict with reduced value
result_test2<-result_test[lmkeep]
result_train2<-result_train[lmkeep]
predicted_result <-predict(linearmodel2,result_test2)
summary(linearmodel2)
misClasificError <- mean(ceiling(predicted_result) != result_test2$Response)
misClasificError
print(paste('Accuracy',1-misClasificError))
#########################################
lm2
predicted_result <-predict(lm2,result_test2)

RMSE <- sqrt(mean((result_test-predicted_result)^2))
RMSE
predicted_result
summary(lm2)
coefficients(linearmodel)
help("coefficients")
plot(linearmodel)
predicted_result

misClasificError <- mean((predicted_result) != result_test2$Response)
misClasificError
print(paste('Accuracy',1-misClasificError))






#####################################
##Predict with reduced value
result_test2<-result_test[lmkeep]
result_train2<-result_train[lmkeep]
predicted_result <-predict(linearmodel2,result_test2)
summary(linearmodel2)
misClasificError <- mean(ceiling(predicted_result) != result_test2$Response)
misClasificError
print(paste('Accuracy',1-misClasificError))
#########################################
lmkeep2<-c("Response","Product_Info_4","Ins_Age","Ht","Wt","BMI","Family_Hist_2",
          "Family_Hist_3","Family_Hist_4","Family_Hist_5","Medical_History_1",
          "Medical_History_10","Medical_History_15","Medical_History_32","Medical_Keyword_2",
          "Medical_Keyword_3","Medical_Keyword_6","Medical_Keyword_9","Medical_Keyword_12","Medical_Keyword_15",
          "Medical_Keyword_18","Medical_Keyword_19","Medical_Keyword_22","Medical_Keyword_24","Medical_Keyword_25",
          "Medical_Keyword_26","Medical_Keyword_28","Medical_Keyword_29","Medical_Keyword_31","Medical_Keyword_33",
          "Medical_Keyword_35","Medical_Keyword_37","Medical_Keyword_38","Medical_Keyword_39","Medical_Keyword_41",
          "Medical_Keyword_45","Product_Info_2_8","Product_Info_2_9","Product_Info_2_11","Product_Info_2_17",
          "Product_Info_3_9","Product_Info_3_18","Product_Info_5_3","Employment_Info_2_37","Employment_Info_2_6",
          "Employment_Info_2_23","InsuredInfo_1_3","InsuredInfo_4_2","InsuredInfo_5_3","InsuredInfo_7_3",
          "InsuredInfo_3_3","Medical_History_2_280","Medical_History_2_548","Medical_History_2_532","Medical_History_2_131",
          "Medical_History_2_293","Medical_History_2_39","Medical_History_2_75","Medical_History_2_419",
          "Medical_History_2_184","Medical_History_2_559","Medical_History_2_99","Medical_History_2_476",
          "Medical_History_2_414","Medical_History_2_399","Medical_History_2_636","Medical_History_2_69",
          "Medical_History_2_618","Medical_History_2_37","Medical_History_2_148","Medical_History_2_516",
          "Medical_History_2_231","Medical_History_2_502","Medical_History_2_45","Medical_History_2_97",
          "Medical_History_2_271","Medical_History_2_611","Medical_History_2_384","Medical_History_2_38",
          "Medical_History_2_623","Medical_History_2_582","Medical_History_2_6","Medical_History_2_133",
          "Medical_History_2_498","Medical_History_2_136","Medical_History_2_207","Medical_History_2_400",
          "Medical_History_3_1","Medical_History_7_2","Medical_History_9_3","Medical_History_11_3","Medical_History_13_2",
          "Medical_History_21_3","Medical_History_34_2","Medical_History_37_3","Medical_History_38_2","Medical_History_23_3","Medical_History_4_2","Medical_History_39_3","Medical_History_23_3","Medical_History_16_1","Medical_History_13_3","InsuredInfo_6_2","Medical_History_40_3","Insurance_History_2_1","Employment_Info_3_1",
          "Medical_History_30_2","Medical_History_33_3","Employment_Info_2_9")

result_test2<-result_test[lmkeep2]
result_train2<-result_train[lmkeep2]
linearmodel2<-lm(result_train2$Response~.,result_train2)
predicted_result <-predict(linearmodel2,result_test2)
summary(linearmodel2)
misClasificError <- mean(ceiling(predicted_result) != result_test3$Response)
misClasificError
print(paste('Accuracy',1-misClasificError))
result_test3$Response
predicted_result
ScoreQuadraticWeightedKappa(result_test3$Response,predicted_result)
plot(linearmodel3)

#############################3-2* and corelatio values
lmkeep3<-c("Response","Product_Info_4","Ins_Age","Ht","Wt","BMI","Family_Hist_2",
           "Family_Hist_3","Family_Hist_4","Family_Hist_5","Medical_History_1",
           "Medical_History_10","Medical_History_15","Medical_Keyword_2",
           "Medical_Keyword_3","Medical_Keyword_6","Medical_Keyword_9","Medical_Keyword_15",
           "Medical_Keyword_19","Medical_Keyword_22","Medical_Keyword_25",
           "Medical_Keyword_26","Medical_Keyword_33",
           "Medical_Keyword_37","Medical_Keyword_38","Medical_Keyword_39","Medical_Keyword_41",
           "Medical_Keyword_45","Product_Info_2_8","Product_Info_2_9",
           "Product_Info_3_9","Employment_Info_2_37",
           "InsuredInfo_1_3","InsuredInfo_4_2","InsuredInfo_5_3","InsuredInfo_7_3",
           "InsuredInfo_3_3","Medical_History_2_532",
           "Medical_History_2_39","Medical_History_2_75",
           "Medical_History_2_414","Medical_History_2_399",
           "Medical_History_2_37","Medical_History_2_148",
           "Medical_History_2_231","Medical_History_2_502","Medical_History_2_45","Medical_History_2_97",
           "Medical_History_2_271","Medical_History_2_611","Medical_History_2_384","Medical_History_2_38",
           "Medical_History_2_623","Medical_History_2_582","Medical_History_2_6","Medical_History_2_133",
           "Medical_History_2_498","Medical_History_2_136","Medical_History_2_207","Medical_History_2_400",
           "Medical_History_3_1","Medical_History_7_2","Medical_History_9_3","Medical_History_11_3","Medical_History_13_2",
           "Medical_History_21_3","Medical_History_34_2","Medical_History_38_2","Medical_History_23_3","Medical_History_4_2","Medical_History_39_3","Medical_History_23_3","Medical_History_16_1","Medical_History_13_3","InsuredInfo_6_2","Medical_History_40_3","Insurance_History_2_1","Employment_Info_3_1",
           "Medical_History_30_2","Medical_History_33_3","Employment_Info_2_9")


result_test3<-result_test[lmkeep3]
result_train3<-result_train[lmkeep3]
linearmodel3<-lm(result_train3$Response~.,result_train3)
predicted_result3 <-predict(linearmodel3,result_test3)
summary(linearmodel3)
summary(linearmodel)
misClasificError <- mean(ceiling(predicted_result3) != result_test3$Response)
misClasificError
print(paste('Accuracy',1-misClasificError))
result_test3$Response
predicted_result
ScoreQuadraticWeightedKappa(result_test3$Response,predicted_result3)
plot(linearmodel3)


anova(linearmodel,linearmodel3)
###############svm
library(e1071)
model<- svm (result_train3$Response ~ .,scale=FALSE,data=result_train3)
?rpusvm

#######################
test.org<- read.csv("test.csv",header=T)
testpred<-lm()

########################ordered
m<-polr(as.factor(result_train3$Response) ~ .,data=result_train3,Hess = true)
m


################################feature selection test

response=result_train$Response
scores.grid <- expand.grid(columns = 1:ncol(result_test), score = NA)


for (i in 1:ncol(result_train)) {
  feat=names(result_test)[i]
  temp=data.frame(result_train[, feat])
  names(temp)=feat
  #eGrid = expand.grid(cp=0.002)
  nf  <- trainControl(method="cv", number=5, classProbs = FALSE, summaryFunction = defaultSummary)    
  a= train(x=temp, y=response, method = "lm", metric="RMSE",  trControl=nf)
  #p=predict(a, train)
  scores.grid[i, 2]=mean(a$results[,"RMSE"])
  cat(mean(a$results[,"RMSE"]), "") 
  
}

?geom_line

scores.grid$featName=names(result_test)
ggplot(data=scores.grid, aes(x=reorder(featName, -score), y=score)) +
  geom_line(colour="darkblue",  aes(group="name")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  ggtitle("Features by rmse Score") 