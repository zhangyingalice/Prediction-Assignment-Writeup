rm(list = ls())
library(caret)

#Load datasets
training= read.csv("C:/YZ_R/Data/pml-training.csv",na.strings=c("NA","#DIV/0!",""))
testing= read.csv("C:/YZ_R/Data/pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

##########################################################################################
#########################         Data preprocessing  & PARTITIONING        #########################
##########################################################################################
training <- training[,colSums(is.na(testing))==0]
testing <- testing[,colSums(is.na(testing))==0]

# Delete irrelevent columns [X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window]
training  <-training[,-c(1:7)]
testing <-testing[,-c(1:7)]

# Divide training data to subtraining and subtesting (70% subtraining, 30% subtesting)
inTrain <- createDataPartition(y=training$classe, p=0.7, list=F,)
subTraining <- training[inTrain,]
subTesting <- training[-inTrain,]

##########################################################################################
#################          Model1  Decision Tree      ####################
##########################################################################################
library(rpart)
model_dt <- rpart(classe ~., data=subTraining, method="class")
pred_dt  <- predict(model_dt, subTesting, type="class")
res_dt <- confusionMatrix(pred_dt,subTesting$classe)
res_dt


##########################################################################################
#################          Model2  Recursive partitioning      ####################
##########################################################################################

library("party")
party_tree <- ctree(formula=classe ~., data=subTraining)
partypredict <- predict(party_tree, newdata=subTesting)
res_party<- confusionMatrix(partypredict ,subTesting$classe)
res_party

##########################################################################################
#################          Model3  Random Forest     ####################
##########################################################################################
library(randomForest)
model_rf <- randomForest(classe ~., data=subTraining, na.action=na.omit)
pred_rf <- predict(model_rf,subTesting, type="class")
# Summarize randomForest results. 
res_rf <- confusionMatrix(pred_rf,subTesting$classe)
res_rf


##########################################################################################
#################          Compare Models    ####################
##########################################################################################
df_res <- data.frame(res_dt$overall, res_party$overall, res_rf$overall)
df_res

##########################################################################################
#################         Predict the Test Data    ####################
##########################################################################################

res <- predict(model_rf, testing, type="class")
res