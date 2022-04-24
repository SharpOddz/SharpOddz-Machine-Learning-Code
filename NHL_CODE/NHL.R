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
setwd("/Users/colinaslett/Desktop/SwimmingCSV/NHL_ML_DATA")
training_data<-read.csv("NHL_TRAINING_DATA.csv", header = TRUE, stringsAsFactors = FALSE)
prediction_data<-read.csv("NHL_PRED_SHEET.csv", header = TRUE, stringsAsFactors = FALSE)

date <- "2022-04-12"

#Simple Regression
#model <- lm(SCORE~.,data = training_data)
#save(model,file="NHL_SIMPLE_REGRESSION.RData")
load("NHL_SIMPLE_REGRESSION.RData")
slr_pred <- data.frame(prediction_data$NAME,predict(model,prediction_data))
file_name <- paste(date,"_SLR_NHL_PREDICTIONS.csv",sep="")
write.csv(slr_pred, file = file_name)
#predict(model,prediction_data)


#SVR
#modelsvm <- svm(SCORE~.,training_data)
#save(modelsvm,file = "NHL_SVM.RData")
load("NHL_SVM.RData")
predict(modelsvm,prediction_data)



#Gradient Boosted Tree
#AdvgradBoost<-gbm(SCORE~.,data=training_data,distribution = "gaussian",n.trees = 10000,
#                  shrinkage = 0.01, interaction.depth = 4)
#save(AdvgradBoost,file = "NHL_GRADIENT_BOOST.RData")
load("NHL_GRADIENT_BOOST.RData")
#plot(AdvgradBoost,i.var=7)
predict(AdvgradBoost,prediction_data)
#summary(AdvgradBoost,las=1)


#Random Forest Tree
#rfAdv <- randomForest(SCORE~.,data = training_data,mtry = 5,importance = TRUE,na.action=na.roughfix)
#save(rfAdv,file = "NHL_RandomForest.RData")
load("NHL_RandomForest.RData")
predict(rfAdv,prediction_data)
#varImpPlot(rfAdv)
#min_depth_frame <- min_depth_distribution(rfAdv)
#plot_min_depth_distribution(min_depth_frame)
#https://www.r-bloggers.com/2019/08/explaining-predictions-random-forest-post-hoc-analysis-randomforestexplainer-package/
#https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html


#varImpPlot(rfAdv)


#Neural Network
#EQUATION IS normalized value * (max-min)+min which in this case, 0 is min, 10 is max
# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# max(training_data$OPP_HITS)
# min(training_data$OPP_HITS)
# training_data$SCORE <- normalize(training_data$SCORE)
# training_data$HOME <- normalize(training_data$HOME)
# training_data$AVG_SHOTS <- normalize(training_data$AVG_SHOTS)
# training_data$AVG_HITS <- normalize(training_data$AVG_HITS)
# training_data$FACEOFF_PERC <- normalize(training_data$FACEOFF_PERC)
# training_data$POWERPLAY_PERC<- normalize(training_data$POWERPLAY_PERC)
# training_data$POWERPLAYC<- normalize(training_data$POWERPLAYC)
# training_data$TAKEAWAYS<- normalize(training_data$TAKEAWAYS)
# training_data$OPP_SAVE_PERC<- normalize(training_data$OPP_SAVE_PERC)
# training_data$OPP_AVG_SHOTS_C<- normalize(training_data$OPP_AVG_SHOTS_C)
# training_data$OPP_TAKEAWAYS<- normalize(training_data$OPP_TAKEAWAYS)
# training_data$OPP_POWERPLAYC<- normalize(training_data$OPP_POWERPLAYC)
# training_data$OPP_BLOCKEDSHOTS<- normalize(training_data$OPP_BLOCKEDSHOTS)
# training_data$OPP_HITS<- normalize(training_data$OPP_HITS)
# set.seed(333)
# n <- neuralnet(SCORE~.,
#                data = training_data,
#                hidden = c(5,2),
#                linear.output = FALSE,
#                lifesign = 'full',
#                rep = 3,
#                algorithm = "rprop+",
#                stepmax = 100000)
#Have to Normalize Prediction Values as well
prediction_data$HOME <- (prediction_data$HOME - 0)/(1 - 0)
prediction_data$AVG_SHOTS <-(prediction_data$AVG_SHOTS - 20.91667)/(42.38462 - 20.91667)
prediction_data$AVG_HITS <-(prediction_data$AVG_HITS - 12)/(40 - 12)
prediction_data$FACEOFF_PERC <-(prediction_data$FACEOFF_PERC - 0.4027778)/(0.6004843 - 0.4027778)
prediction_data$POWERPLAY_PERC <-(prediction_data$POWERPLAY_PERC - 0)/(0.3529412 - 0)
prediction_data$POWERPLAYC <-(prediction_data$POWERPLAYC - 1.6)/(5.5 - 1.6)
prediction_data$TAKEAWAYS <-(prediction_data$TAKEAWAYS - 2.2)/(14.2 - 2.2)
prediction_data$OPP_SAVE_PERC <-(prediction_data$OPP_SAVE_PERC - .8243243)/(.9716981 - .8243243)
prediction_data$OPP_AVG_SHOTS_C <-(prediction_data$OPP_AVG_SHOTS_C - 21.2)/(42 - 21.2)
prediction_data$OPP_TAKEAWAYS <-(prediction_data$OPP_TAKEAWAYS - 2.2)/(14.2 - 2.2)
prediction_data$OPP_POWERPLAYC <-(prediction_data$OPP_POWERPLAYC - 1.6)/(5.5 - 1.6)
prediction_data$OPP_BLOCKEDSHOTS <-(prediction_data$OPP_BLOCKEDSHOTS - 7.6)/(22.4 - 7.6)
prediction_data$OPP_HITS <-(prediction_data$OPP_HITS - 12)/(40 - 12)

#save(n,file="NHL_NEURAL_NET.RData")
load("NHL_NEURAL_NET.RData")
output <- compute(n, prediction_data)
output$net.result*10
#TIMES TEN TO UN-NORMALIZE THE OUTPUT
#hist(training_data$OPP_HITS)
