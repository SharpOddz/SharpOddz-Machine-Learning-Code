library(neuralnet)
library(dpylr)
library(ggplot2)
library(psych)
library(caret)
library(quantreg)
library(e1071)
library(pls)
library(glmnet)
library(nnet)
library(randomForest)
library(randomForestExplainer)
library(rpart)
library(gbm)
library(lightgbm)
setwd("/Users/colinaslett/Desktop/SwimmingCSV/NFL_ML_DATA")
training_data<-read.csv("NFL_TRAINING_DATA.csv", header = TRUE, stringsAsFactors = FALSE)
prediction_data<-read.csv("NFL_PRED_SHEET.csv", header = TRUE, stringsAsFactors = FALSE)

date <- Sys.Date()

#Simple Regression
#model <- lm(SCORE~.,data = training_data)
#save(model,file="NFL_SIMPLE_REGRESSION.RData")
load("NFL_SIMPLE_REGRESSION.RData")
slr_pred <- data.frame(prediction_data$NAME,predict(model,prediction_data))
file_name <- paste(date,"_SLR_NFL_PREDICTIONS.csv",sep="")
write.csv(slr_pred, file = file_name)

#SVR
#modelsvm <- svm(SCORE~.,training_data)
#save(modelsvm,file = "NFL_SVM.RData")
load("NFL_SVM.RData")
#predict(modelsvm,training_data)
svm_pred <- data.frame(prediction_data$NAME,predict(modelsvm,prediction_data))
file_name <- paste(date,"_SVM_NFL_PREDICTIONS.csv",sep="")
write.csv(svm_pred, file = file_name)

#Gradient Boosted Tree
#AdvgradBoost<-gbm(SCORE~.,data=training_data,distribution = "gaussian",n.trees = 10000,
#                  shrinkage = 0.01, interaction.depth = 4)
#save(AdvgradBoost,file = "NFL_GRADIENT_BOOST.RData")
load("NFL_GRADIENT_BOOST.RData")
gbt_pred <- data.frame(prediction_data$NAME,predict(AdvgradBoost,prediction_data))
file_name <- paste(date,"_GBT_NFL_PREDICTIONS.csv",sep="")
write.csv(gbt_pred, file = file_name)


#Random Forest Tree
#rfAdv <- randomForest(SCORE~.,data = training_data,mtry = 5,importance = TRUE,na.action=na.roughfix)
#save(rfAdv,file = "NFL_RandomForest.RData")
load("NFL_RandomForest.RData")
predict(rfAdv,training_data)
rft_pred <- data.frame(prediction_data$NAME,predict(rfAdv,prediction_data))
file_name <- paste(date,"_RFT_NFL_PREDICTIONS.csv",sep="")
write.csv(rft_pred, file = file_name)



#Neural Network
#EQUATION IS normalized value * (max-min)+min which in this case, 0 is min, 10 is max
#normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
#}
#max(training_data$OPP_PEN_YARDS)
#min(training_data$OPP_PEN_YARDS)
# training_data$SCORE <- normalize(training_data$SCORE)
# training_data$HOME <- normalize(training_data$HOME)
# training_data$THIRD_DOWN_SUCC <- normalize(training_data$THIRD_DOWN_SUCC)
# training_data$PTS_PER_DRIVE <- normalize(training_data$PTS_PER_DRIVE)
# training_data$YARDS_PER_PASS_ATT <- normalize(training_data$YARDS_PER_PASS_ATT)
# training_data$PASSING_PCT<- normalize(training_data$PASSING_PCT)
# training_data$PASSING_INTS<- normalize(training_data$PASSING_INTS)
# training_data$YARDS_PER_RUSH_ATT<- normalize(training_data$YARDS_PER_RUSH_ATT)
# training_data$RED_ZONE_PCT<- normalize(training_data$RED_ZONE_PCT)
# training_data$PEN_YARDS<- normalize(training_data$PEN_YARDS)
# training_data$TEMPO<- normalize(training_data$TEMPO)
# training_data$OPP_THIRD_DOWN_SUCC_CONCEDED<- normalize(training_data$OPP_THIRD_DOWN_SUCC_CONCEDED)
# training_data$OPP_POINTS_CONCEDED_PER_DRIVE<- normalize(training_data$OPP_POINTS_CONCEDED_PER_DRIVE)
# training_data$OPP_YARDS_PER_PASS_ATT_CONCEDED<- normalize(training_data$OPP_YARDS_PER_PASS_ATT_CONCEDED)
# training_data$OPP_YARDS_PER_RUSH_ATT_CONCEDED<- normalize(training_data$OPP_YARDS_PER_RUSH_ATT_CONCEDED)
# training_data$OPP_RED_ZONE_SUCCESS_CONCEDED<- normalize(training_data$OPP_RED_ZONE_SUCCESS_CONCEDED)
# training_data$OPP_PEN_YARDS<- normalize(training_data$OPP_PEN_YARDS)

#set.seed(333)
# nn <- neuralnet(SCORE~.,
#                 data = training_data,
#                 hidden = c(7,3),
#                 linear.output = FALSE,
#                 lifesign = 'full',
#                 rep = 1,
#                 algorithm = "rprop+",
#                 stepmax = 120000)
# save(nn,file="NFL_NEURAL_NET.RData")
#Have to Normalize Prediction Values as well
prediction_data$HOME <- (prediction_data$HOME - 0)/(1 - 0)
prediction_data$THIRD_DOWN_SUCC <- (prediction_data$THIRD_DOWN_SUCC - 0.15)/(0.6190476 - 0.15)
prediction_data$PTS_PER_DRIVE <- (prediction_data$PTS_PER_DRIVE - 0.3)/(3.727273 - 0.3)
prediction_data$YARDS_PER_PASS_ATT <- (prediction_data$YARDS_PER_PASS_ATT - 3.466667)/(13.27869 - 3.466667)
prediction_data$PASSING_PCT<- (prediction_data$PASSING_PCT - 0.4393939)/(0.8225806 - 0.4393939)
prediction_data$PASSING_INTS<- (prediction_data$PASSING_INTS - 0)/(0.08450704 - 0)
prediction_data$YARDS_PER_RUSH_ATT<- (prediction_data$YARDS_PER_RUSH_ATT - 1.787879)/(7.567568 - 1.787879)
prediction_data$RED_ZONE_PCT<- (prediction_data$RED_ZONE_PCT - 0)/(1 - 0)
prediction_data$PEN_YARDS<- (prediction_data$PEN_YARDS - 20)/(133.5 - 20)
prediction_data$TEMPO<- (prediction_data$TEMPO - 45)/(78 - 45)
prediction_data$OPP_THIRD_DOWN_SUCC_CONCEDED<- (prediction_data$OPP_THIRD_DOWN_SUCC_CONCEDED - 0.1)/(0.6428571 - 0.1)
prediction_data$OPP_POINTS_CONCEDED_PER_DRIVE<- (prediction_data$OPP_POINTS_CONCEDED_PER_DRIVE - 0.125)/(4.434783 - 0.125)
prediction_data$OPP_YARDS_PER_PASS_ATT_CONCEDED<- (prediction_data$OPP_YARDS_PER_PASS_ATT_CONCEDED - 4.144444)/(11.72222 - 4.144444)
prediction_data$OPP_YARDS_PER_RUSH_ATT_CONCEDED<- (prediction_data$OPP_YARDS_PER_RUSH_ATT_CONCEDED - 1.625)/(6.318841 - 1.625)
prediction_data$OPP_RED_ZONE_SUCCESS_CONCEDED<- (prediction_data$OPP_RED_ZONE_SUCCESS_CONCEDED - 0)/(1 - 0)
prediction_data$OPP_PEN_YARDS<- (prediction_data$OPP_PEN_YARDS - 20)/(133.5 - 20)

load("NFL_NEURAL_NET.RData")
output <- compute(nn, prediction_data)
#output$net.result*57
nn_pred <- data.frame(prediction_data$NAME,output$net.result*57)
file_name <- paste(date,"_NN_NFL_PREDICTIONS.csv",sep="")
write.csv(nn_pred, file = file_name)

#TIMES 57 TO UN-NORMALIZE THE OUTPUT
