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
setwd("/Users/colinaslett/Desktop/SwimmingCSV/CBB_ML_DATA")
training_data<-read.csv("CBB_TRAINING_DATA.csv", header = TRUE, stringsAsFactors = FALSE)
prediction_data<-read.csv("CBB_PRED_SHEET.csv", header = TRUE, stringsAsFactors = FALSE)

date <- Sys.Date()

#Random Forest Tree
rfAdv <- randomForest(SCORE~.,data = training_data,mtry = 5,importance = TRUE,na.action=na.roughfix)
save(rfAdv,file = "CBB_RandomForest.RData")
load("CBB_RandomForest.RData")
rft_pred <- data.frame(prediction_data$NAME,predict(rfAdv,prediction_data))
file_name <- paste(date,"_RFT_CBB_PREDICTIONS.csv",sep="")
write.csv(rft_pred, file = file_name)

#Gradient Boosted Tree
#AdvgradBoost<-gbm(SCORE~.,data=training_data,distribution = "gaussian",n.trees = 10000,
#                  shrinkage = 0.01, interaction.depth = 4)
#save(AdvgradBoost,file = "CBB_GRADIENT_BOOST.RData")
load("CBB_GRADIENT_BOOST.RData")
gbt_pred <- data.frame(prediction_data$NAME,predict(AdvgradBoost,prediction_data))
file_name <- paste(date,"_GBT_CBB_PREDICTIONS.csv",sep="")
write.csv(gbt_pred, file = file_name)

#SVR
#modelsvm <- svm(SCORE~.,training_data)
#save(modelsvm,file = "CBB_SVM.RData")
load("CBB_SVM.RData")
svm_pred <- data.frame(prediction_data$NAME,predict(modelsvm,prediction_data))
file_name <- paste(date,"_SVM_CBB_PREDICTIONS.csv",sep="")
write.csv(svm_pred, file = file_name)


#Simple Regression
#model <- lm(SCORE~.,data = training_data)
#save(model,file="CBB_SIMPLE_REGRESSION.RData")
load("CBB_SIMPLE_REGRESSION.RData")
slr_pred <- data.frame(prediction_data$NAME,predict(model,prediction_data))
file_name <- paste(date,"_SLR_CBB_PREDICTIONS.csv",sep="")
write.csv(slr_pred, file = file_name)


#Neural Network
#EQUATION IS normalized value * (max-min)+min which in this case, 0 is min, 10 is max
# normalize <- function(x) {
#    return ((x - min(x)) / (max(x) - min(x)))
# }
# max(training_data$OPP_TURNOVERS)
# min(training_data$OPP_TURNOVERS)
# training_data$SCORE <- normalize(training_data$SCORE)
# training_data$TWO_PT_PERC <- normalize(training_data$TWO_PT_PERC)
# training_data$THREE_PT_PERC <- normalize(training_data$THREE_PT_PERC)
# training_data$FG_PERC <- normalize(training_data$FG_PERC)
# training_data$TEMPO<- normalize(training_data$TEMPO)
# training_data$OFF_REBOUND_PERC<- normalize(training_data$OFF_REBOUND_PERC)
# training_data$DEF_REBOUND_PERC<- normalize(training_data$DEF_REBOUND_PERC)
# training_data$STEALS<- normalize(training_data$STEALS)
# training_data$STEALS_CONCEDED<- normalize(training_data$STEALS_CONCEDED)
# training_data$TURNOVERS<- normalize(training_data$TURNOVERS)
# training_data$TURNOVERS_CONCEDED<- normalize(training_data$TURNOVERS_CONCEDED)
# training_data$OPP_OFF_REBOUND_PERC<- normalize(training_data$OPP_OFF_REBOUND_PERC)
# training_data$OPP_DEF_REBOUND_PERC<- normalize(training_data$OPP_DEF_REBOUND_PERC)
# training_data$OPP_STEALS<- normalize(training_data$OPP_STEALS)
# training_data$OPP_BLOCKS<- normalize(training_data$OPP_BLOCKS)
# training_data$OPP_TURNOVERS<- normalize(training_data$OPP_TURNOVERS)
# set.seed(333)
# n <- neuralnet(SCORE~.,
#                data = training_data,
#                hidden = c(7,3),
#                linear.output = FALSE,
#                lifesign = 'full',
#                rep = 1,
#                algorithm = "rprop+",
#                stepmax = 250000)
# save(n,file="CBB_NEURAL_NET.RData")
#Have to Normalize Prediction Values as well
prediction_data$TWO_PT_PERC <- (prediction_data$TWO_PT_PERC - 0.2654545)/(0.5828571 - 0.2654545)
prediction_data$THREE_PT_PERC <- (prediction_data$THREE_PT_PERC - 0.1916667)/(0.5073529 - 0.1916667)
prediction_data$FG_PERC <- (prediction_data$FG_PERC - 0.869281)/(0.4777778 - 0.869281)
prediction_data$TEMPO<- (prediction_data$TEMPO - 59.5)/(127.6667 - 59.5)
prediction_data$OFF_REBOUND_PERC<- (prediction_data$OFF_REBOUND_PERC - 0.258427)/(0.7555556 - 0.258427)
prediction_data$DEF_REBOUND_PERC<- (prediction_data$DEF_REBOUND_PERC - 0.5066079)/(0.8562092 - 0.5066079)
prediction_data$STEALS<- (prediction_data$STEALS - 2.8)/(15 - 2.8)
prediction_data$STEALS_CONCEDED<- (prediction_data$STEALS_CONCEDED - 2)/(15.6 - 2)
prediction_data$TURNOVERS<- (prediction_data$TURNOVERS - 0)/(25 - 0)
prediction_data$TURNOVERS_ßCONCEDED<- (prediction_data$TURNOVERS_CONCEDED - 0)/(18.16667 - 0)
prediction_data$OPP_OFF_REBOUND_PERC<- (prediction_data$OPP_OFF_REBOUND_PERC - 0.258427)/(0.7555556 - 0.258427)
prediction_data$OPP_DEF_REBOUND_PERC<- (prediction_data$OPP_DEF_REBOUND_PERC - 0.5066079)/(0.8562092 - 0.5066079)
prediction_data$OPP_STEALS<- (prediction_data$OPP_STEALS - 2.8)/(15 - 2.8)
prediction_data$OPP_BLOCKS<- (prediction_data$OPP_BLOCKS - 0.4)/(8.846154 - 0.4)
prediction_data$OPP_TURNOVERS<- (prediction_data$OPP_TURNOVERS - 0)/(25 - 0)

load("CBB_NEURAL_NET.RData")
output <- compute(n, prediction_data)
nn_pred <- data.frame(prediction_data$NAME,((output$net.result*(139-24)) + 24))
file_name <- paste(date,"_NN_CBB_PREDICTIONS.csv",sep="")
write.csv(nn_pred, file = file_name)
