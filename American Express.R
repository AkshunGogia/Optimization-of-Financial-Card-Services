library(data.table)
library(caret)
library(ggplot2)
library(mlr)
library(stringr)
library(e1071)
library(plyr)
library(dplyr)

train<-fread("Training_Dataset.csv")
final<-fread("Final_Dataset.csv")
lead<-fread("Leaderboard_Dataset.csv")
dict<-fread("Data_Dictionary (1).csv")

tr<-train
fi<-final
le<-lead

da_store<-rbind(tr[,-c(47:52)],fi,le)
#tr[,sup:=mvar46]
#tr[,elite:=mvar47]
#tr[,credit:=mvar48]

#tr$elite<-ifelse(tr$elite==1,2,tr$elite)
#tr$credit<-ifelse(tr$credit==1,3,tr$credit)

#tr$extension<-0
#tr$extension<-ifelse(tr$sup==1,1,0)
#tr$extension<-ifelse(tr$elite==2,2,tr$extension)
#tr$extension<-ifelse(tr$credit==3,3,tr$extension)

da<-rbind(tr[,-c(47:52)],fi,le)

da$mvar1=NULL
da$mvar12<-as.factor(da$mvar12)
da[,profession:=da$mvar12]

dum<-createDummyFeatures(da$profession)
da<-cbind(da,dum)

da$profession=NULL
lapply(da,class)

da$mvar3<-as.numeric(da$mvar3)

preprocessParams <- preProcess(da[,3], method=c("range"))
da[,3] <- predict(preprocessParams, da[,3])

#da[,6]<-scale(da$mvar6)
#da[,9]<-scale(da$mvar9)

da[,years:=da$mvar5/12]

#da$mvar5=NULL

da$mvar12<-as.numeric(da$mvar12)
da$mvar12<-as.factor(da$mvar12)

da$avg_income<-ifelse(da$mvar2>0,da$mvar9/da$mvar2,da$mvar9)


#da$mvar9=NULL

#preprocessParams2 <- preProcess(da[,66], method=c("range"))
#da[,66]<- predict(preprocessParams2, da[,66])

da[,avg_elctronics:=(mvar16+mvar17+mvar18+mvar19)]
da[,avg_travel:=(mvar20+mvar21+mvar22+mvar23)]
da[,avg_household:=(mvar24+mvar25+mvar26+mvar27)]
da[,avg_car:=(mvar28+mvar29+mvar30+mvar31)]
da[,avg_retail:=(mvar32+mvar33+mvar34+mvar35)]
da[,avg_spend:=(mvar36+mvar37+mvar38+mvar39)]

da2<-da

da2$Has_Family<-ifelse(da2$mvar2>0,1,0)
da2$More_Travel<-ifelse(da2$avg_travel>mean(da2$avg_travel),1,0)
da2$More_Car<-ifelse(da2$avg_car>mean(da2$avg_car),1,0)
da2$Inc_To_Spend<-ifelse(da2$avg_spend>0,da2$avg_income/da2$avg_spend,0)

da2$More_Retail<-ifelse(da2$Has_Family>0 & da2$avg_retail>mean(da2$avg_retail),1,0)
da2$More_Household<-ifelse(da2$Has_Family>0 & da2$avg_household>mean(da2$avg_household),1,0)

da2$Less_Inc_To_Spend<-ifelse(da2$Has_Family>0 & da2$Inc_To_Spend<mean(da2$Inc_To_Spend),1,0)

da2$supp_accepted<-ifelse(da2$mvar40>0 & da2$mvar43>0,da2$mvar40-da2$mvar43,99)
da2$supp_accepted<-as.factor(da2$supp_accepted)
dum2<-createDummyFeatures(da2$supp_accepted)
dum2[,5:7]=NULL
colnames(dum2)<-c("supp_high","supp_medium","supp_avg","supp_low")
da2<-cbind(da2,dum2)

da2$elite_accepted<-ifelse(da2$mvar41>0 & da2$mvar44>0,da2$mvar41-da2$mvar44,99)
da2$elite_accepted<-as.factor(da2$elite_accepted)
dum2<-createDummyFeatures(da2$elite_accepted)
dum2[,4]=NULL
colnames(dum2)<-c("elite_high","elite_medium","elite_avg")
da2<-cbind(da2,dum2)

da2$credit_accepted<-ifelse(da2$mvar42>0 & da2$mvar45>0,da2$mvar42-da2$mvar45,99)
da2$credit_accepted<-as.factor(da2$credit_accepted)
dum2<-createDummyFeatures(da2$elite_accepted)
dum2[,3:4]=NULL
colnames(dum2)<-c("credit_high","credit_medium")
da2<-cbind(da2,dum2)

da2[,c("supp_accepted","elite_accepted","credit_accepted")]=NULL

da2$supp_more_travel<-ifelse(da2$More_Travel==1 & (da2$supp_high==1 | da2$supp_medium==1 | da2$supp_avg==1),1,0)
da2$supp_more_car<-ifelse(da2$More_Car==1 & (da2$supp_high==1 | da2$supp_medium==1 | da2$supp_avg==1),1,0)
da2$supp_more_household<-ifelse(da2$Household==1 & (da2$supp_high==1 | da2$supp_medium==1 | da2$supp_avg==1),1,0)
da2$supp_more_retail<-ifelse(da2$More_Retail==1 & (da2$supp_high==1 | da2$supp_medium==1 | da2$supp_avg==1),1,0)
da2$supp_less_inc_to_spend<-ifelse(da2$Less_Inc_To_Spend==1 & (da2$supp_high==1 | da2$supp_medium==1 | da2$supp_avg==1),1,0)


#da2$supp_low=NULL
#da2$elite_avg=NULL


da2$elite_more_travel<-ifelse(da2$More_Travel==1 & (da2$elite_high==1|da2$elite_medium==1),1,0)
da2$elite_more_car<-ifelse(da2$More_Car==1 & (da2$elite_high==1|da2$elite_medium==1),1,0)
da2$elite_more_household<-ifelse(da2$Household==1 & (da2$elite_high==1|da2$elite_medium==1),1,0)
da2$elite_more_retail<-ifelse(da2$More_Retail==1 & (da2$elite_high==1|da2$elite_medium==1),1,0)
da2$elite_less_inc_to_spend<-ifelse(da2$Less_Inc_To_Spend==1 & (da2$elite_high==1|da2$elite_medium==1),1,0)


da2$credit_more_travel<-ifelse(da2$More_Travel==1 & (da2$credit_high==1|da2$credit_medium==1),1,0)
da2$credit_more_car<-ifelse(da2$More_Car==1 & (da2$credit_high==1|da2$credit_medium==1),1,0)
da2$credit_more_household<-ifelse(da2$Household==1 & (da2$credit_high==1|da2$credit_medium==1),1,0)
da2$credit_more_retail<-ifelse(da2$More_Retail==1 & (da2$credit_high==1|da2$credit_medium==1),1,0)
da2$credit_less_inc_to_spend<-ifelse(da2$Less_Inc_To_Spend==1 & (da2$credit_high==1|da2$credit_medium==1),1,0)


da2$air_miles_elite<-ifelse((da2$elite_high==1|da2$elite_medium==1|da2$elite_avg==1) & da2$mvar15>0,1,0)
da2$air_miles_supp<-ifelse((da2$supp_high==1|da2$supp_medium==1|da2$supp_avg==1) & da2$mvar15>0,1,0)

da2$platnium_elite<-ifelse((da2$elite_high==1|da2$elite_medium==1|da2$elite_avg==1) & da2$mvar10==1,1,0)
da2$platnium_supp<-ifelse((da2$supp_high==1|da2$supp_medium==1|da2$supp_avg==1) & da2$mvar10==1,1,0)


da2$credit_payment<-ifelse((da2$credit_high==1 | da2$credit_medium==1) & da2$mvar13>10,1,0)
da2$supp_payment<-ifelse((da2$supp_high==1|da2$supp_avg==1|da2$supp_medium==1)&da2$mvar13>15,1,0)
da2$elite_payment<-ifelse((da2$elite_high==1|da2$elite_medium==1|da2$elite_avg==1) & da2$mvar13>10,1,0)

ds<-da2

ds$avg_mem_fees=ifelse(ds$mvar14>0,ds$mvar6/ds$mvar14,0)
ds$more_mem<-ifelse(ds$avg_mem_fees>mean(ds$avg_mem_fees),1,0)

ds$supp_more_mem<-ifelse(ds$more_mem==1 & (ds$supp_high==1 | ds$supp_medium==1 | ds$supp_avg==1),1,0)
ds$elite_more_mem<-ifelse(ds$more_mem==1 & (ds$elite_high==1|ds$elite_medium==1|ds$elite_avg==1),1,0)
ds$credit_more_mem<-ifelse(ds$more_mem==1 & (ds$credit_high==1|ds$credit_medium==1),1,0)

ds$mvar3<-da_store$mvar3

ds$check_spend<-ifelse(ds$mvar3>0 & ds$avg_spend>0,ds$mvar3/ds$avg_spend,0)
ds$more_check_spend<-ifelse(ds$check_spend>mean(ds$check_spend),1,0)
ds$supp_check_spend<-ifelse(ds$more_check_spend==1 & (ds$supp_high==1 | ds$supp_medium==1 | ds$supp_avg==1),1,0)
ds$elite_check_spend<-ifelse(ds$more_check_spend==1 & (ds$elite_high==1 | ds$elite_medium==1 | ds$elite_avg==1),1,0)
ds$credit_check_spend<-ifelse(ds$more_check_spend==1 & (ds$credit_high==1 | ds$credit_medium==1),1,0)

ds$platnium_credit<-ifelse((ds$credit_high==1|ds$credit_medium==1) & ds$mvar10==1,1,0)
ds$air_miles_credit<-ifelse((ds$credit_high==1|ds$credit_medium==1) & ds$mvar15>0,1,0)

dss<-ds

dss<-dss[,-c(2:45)]
dss<-dss[,-c(22:28)]
dss<-dss[,-c(21,25,60,65)]
dss[,years:=ceiling(years)]

dss[,c("Unknown","credit_high","credit_medium","credit_more_travel","credit_more_car",
         "credit_more_household","credit_more_retail","credit_less_inc_to_spend",
         "credit_check_spend","platnium_credit","air_miles_credit"):=NULL]

trr<-dss[1:40000,]
ld<-dss[ds$cm_key==50001:60000,]
fd<-dss[ds$cm_key==70001:80000,]

trr[,sup:=train$mvar49]
trr[,elite:=train$mvar50]
trr[,credit:=train$mvar51]

#trr$elite<-ifelse(trr$elite==1,2,trr$elite)
#trr$credit<-ifelse(trr$credit==1,3,trr$credit)

#trr$extension<-0
#trr$extension<-ifelse(trr$sup==1,1,0)
#trr$extension<-ifelse(trr$elite==2,2,trr$extension)
#trr$extension<-ifelse(trr$credit==3,3,trr$extension)

#trr$sup=NULL
#trr$credit=NULL
#trr$elite=NULL
trr$elite_avg=NULL
ld$elite_avg=NULL
fd$elite_avg=NULL

col<-c("Unknown","credit_high","credit_medium","credit_more_travel","credit_more_car",
       "credit_more_household","credit_more_retail","credit_less_inc_to_spend",
       "credit_check_spend","platnium_credit","air_miles_credit")


lapply(trr,class)

model<-glm(sup~.,data=trr[,-c("credit","elite","cm_key")])

pred_train<-predict(model, trr[,-c("cm_key","credit","elite")],type = 'response')
pred<-predict(model, ld[,-c("cm_key")],type = 'response')
fitted.results<-as.data.frame(pred)
fitted.results <- ifelse(fitted.results >= 0.227,1,0)
table(fitted.results)

fitted.results_train<-pred_train
fitted.results_train <- ifelse(fitted.results_train >= 0.23,1,0)
confusionMatrix(fitted.results_train,trr$sup)

model_elite<-glm(elite~.,data=trr[,-c("credit","sup","cm_key")], family = binomial)

pred_train_elite<-predict(model_elite, trr[,-c("cm_key","credit","sup")],type = 'response')
pred_elite<-predict(model_elite, ld[,-c("cm_key")],type = 'response')
fitted.results_elite<-as.data.frame(pred_elite)
fitted.results_elite <- ifelse(fitted.results_elite >= 0.144,1,0)
table(fitted.results_elite)

fitted.results_train_elite<-pred_train_elite
fitted.results_train_elite <- ifelse(fitted.results_train_elite >= 0.144,1,0)
confusionMatrix(fitted.results_train_elite,trr$elite)


model_credit<-glm(credit~.,data=trr[,-c("elite","sup","cm_key")], family = binomial)

pred_train_credit<-predict(model_credit, trr[,-c("cm_key","elite","sup")],type = 'response')
pred_credit<-predict(model_credit, ld[,-c("cm_key")],type = 'response')
fitted.results_credit<-as.data.frame(pred_credit)
fitted.results_credit <- ifelse(fitted.results_credit >= 0.125,1,0)
table(fitted.results_credit)

fitted.results_train_credit<-pred_train_credit
fitted.results_train_credit <- ifelse(fitted.results_train_credit >= 0.125,1,0)
confusionMatrix(fitted.results_train_credit,trr$credit)

sol<-as.data.frame(cbind(ld$cm_key, fitted.results, fitted.results_elite, fitted.results_credit))
anova(model, test = 'Chisq')


sol$sup_elite<-ifelse(sol$pred==1 & sol$pred_elite==1,1,0)
sol$elite_credit<-ifelse(sol$pred_elite==1&sol$pred_credit==1,1,0)
sol$sup_credit<-ifelse(sol$pred==1 & sol$pred_credit==1,1,0)

table(sol$sup_elite)
table(sol$elite_credit)
table(sol$sup_credit)

table(sol$pred)
table(sol$pred_elite)
table(sol$pred_credit)

sol$pred_credit<-ifelse(sol$pred==1 & sol$pred_credit==1,0,sol$pred_credit)
setDT(sol)
sol[,c("sup_elite","elite_credit","sup_credit"):=NULL]
sol$card<-0
sol$card<-ifelse(sol$pred_credit==1,"Credit",sol$card)
sol$card<-ifelse(sol$pred==1,"Supp",sol$card)
sol$card<-ifelse(sol$pred_elite==1,"Elite",sol$card)


final<-sol[,c(1,5)]
final<-final[final$card!=0,]

final$card<-as.factor(final$card)
write.csv(final,"ans3.csv")

library(ROCR)
ROCRpred <- prediction(fitted.results_train,trr$sup)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

library(h2o)
h2o.init(nthreads = -1,max_mem_size = "10G") 
h2o_train1 <- as.h2o(trr[,-c(56,57)])
h2o_test <- as.h2o(ld)

h2o_train1$sup<- h2o.asfactor(h2o_train1$sup)

xd1 <- h2o.splitFrame(h2o_train1,ratios = 0.7)

split_val1 <- xd1[[2]]

y <- "sup"
x <- setdiff(colnames(trr), c(y,"cm_key","elite","credit"))

gbm_clf1 <- h2o.gbm(x = x
                   ,y = y
                   ,training_frame = h2o_train1
                   ,validation_frame = split_val1
                   ,ignore_const_cols = T
                   ,ntrees = 500
                   ,model_id = "gbm_model"
                   ,stopping_metric = "AUC"
                   ,learn_rate = 1)

gbm_clf1
gbm_clf_pred1 <- as.data.table(h2o.predict(gbm_clf1,h2o_test))
sub_pred1 <- data.table(cm_key = ld$cm_key, loan_status = gbm_clf_pred1$p1)
sub_pred1$loan_status<-ifelse(sub_pred1$loan_status>0.32,1,0)
table(sub_pred1$loan_status)
head(gbm_clf_pred,10)

gbm_clf_pred_sup <- as.data.table(h2o.predict(gbm_clf1,h2o_train1))
tr_sup<-ifelse(gbm_clf_pred_sup$p1>0.32,1,0)
confusionMatrix(trr$sup,tr_sup)
table(tr_sup)


h2o_train <- as.h2o(trr[,-c(55,57)])
h2o_test <- as.h2o(ld)

h2o_train$elite<- h2o.asfactor(h2o_train$elite)

xd <- h2o.splitFrame(h2o_train,ratios = 0.7)

split_val <- xd[[2]]

y <- "elite"
x <- setdiff(colnames(trr), c(y,"cm_key","sup","credit"))

gbm_clf <- h2o.gbm(x = x
                   ,y = y
                   ,training_frame = h2o_train
                   ,validation_frame = split_val
                   ,ignore_const_cols = T
                   ,ntrees = 500
                   ,model_id = "gbm_model"
                   ,stopping_metric = "AUC"
                   ,learn_rate = 1)
                   

gbm_clf
gbm_clf_pred <- as.data.table(h2o.predict(gbm_clf,h2o_test))
sub_pred2 <- data.table(cm_key = ld$cm_key, loan_status = gbm_clf_pred$p1)
sub_pred2$loan_status<-ifelse(sub_pred2$loan_status>0.28,1,0)
table(sub_pred2$loan_status)
head(gbm_clf_pred,10)

gbm_clf_pred_tr <- as.data.table(h2o.predict(gbm_clf,h2o_train))
tr_elite<-ifelse(gbm_clf_pred_tr$p1>0.28,1,0)
confusionMatrix(trr$elite,tr_elite)
table(tr_elite)


h2o_train2 <- as.h2o(trr[,-c(55,56)])
h2o_test <- as.h2o(ld)

h2o_train2$credit<- h2o.asfactor(h2o_train2$credit)

xd2 <- h2o.splitFrame(h2o_train2,ratios = 0.7)

split_val2 <- xd2[[2]]

y <- "credit"
x <- setdiff(colnames(trr), c(y,"cm_key","elite","sup"))

gbm_clf2 <- h2o.gbm(x = x
                    ,y = y
                    ,training_frame = h2o_train2
                    ,validation_frame = split_val2
                    ,ignore_const_cols = T
                    ,ntrees = 500
                    ,stopping_rounds = 10
                    ,model_id = "gbm_model"
                    ,stopping_metric = "AUC"
                    ,learn_rate = 1)

gbm_clf2
gbm_clf_pred2 <- as.data.table(h2o.predict(gbm_clf2,h2o_test))
sub_pred3 <- data.table(cm_key = ld$cm_key, loan_status = gbm_clf_pred2$p1)
sub_pred3$loan_status<-ifelse(sub_pred3$loan_status>0.3,1,0)
table(sub_pred3$loan_status)
head(gbm_clf_pred2,10)

gbm_clf_pred_credit <- as.data.table(h2o.predict(gbm_clf2,h2o_train2))
head(gbm_clf_pred_credit,10)
tr_credit<-ifelse(gbm_clf_pred_credit$p1>0.3,1,0)
confusionMatrix(trr$credit,tr_credit)
table(tr_credit)








library(xgboost)

ld$sup<-NA
labels <- trr$sup 
ts_label <-ld$sup
new_tr <- as.matrix(trr[,-c("cm_key","credit","elite")])
new_ts <- as.matrix(ld[,-c("cm_key")])

#preparing matrix 
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts)

#default parameters
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.02, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, maximize = F)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 45, print.every.n = 10, maximize = F)
xgbpred <- predict (xgb1,dtest)
pred<-data.frame(xgbpred)


j<-cbind(sub_pred1,sub_pred2$loan_status,sub_pred3$loan_status)
colnames(j)<-c("cm_key","pred","pred_elite","pred_credit")
sol<-j
sol$sup_elite<-ifelse(sol$pred==1 & sol$pred_elite==1,1,0)
sol$elite_credit<-ifelse(sol$pred_elite==1&sol$pred_credit==1,1,0)
sol$sup_credit<-ifelse(sol$pred==1 & sol$pred_credit==1,1,0)

table(sol$sup_elite)
table(sol$elite_credit)
table(sol$sup_credit)

sol$pred_credit<-ifelse(sol$pred==1 & sol$pred_credit==1,0,sol$pred_credit)
sol$pred_elite<-ifelse(sol$pred==1 & sol$pred_elite==1,0,sol$pred_elite)
setDT(sol)
sol[,c("sup_elite","elite_credit","sup_credit"):=NULL]
sol$card<-0
sol$card<-ifelse(sol$pred==1,"Supp",sol$card)
sol$card<-ifelse(sol$pred_elite==1,"Elite",sol$card)
sol$card<-ifelse(sol$pred_credit==1,"Credit",sol$card)

final2<-sol[,c(1,5)]
final2<-final[final$card!=0,]

final2$card<-as.factor(final2$card)
write.csv(final2,"ans_gbm.csv")
