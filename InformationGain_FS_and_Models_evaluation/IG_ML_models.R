#*******************************************************************************************
#*---------------------------MACHINE LEARNING MODELS (INFORMATION GAIN)----------------------------------------
#********************************************************************************************
install.packages("caret")   #confusionMatrix
library(caret)

#-------------------------------------------------------------------------------------------
#--------------------------START : RANDOM FOREST--------------------------------------------
#-------------------------------------------------------------------------------------------

#Installing library
install.packages("randomForest")
library("randomForest")
set.seed(2500)

RF_model <- randomForest(TARGET ~.,data=ig_traindata,importance = TRUE)
RF_model
plot(RF_model)

ig_testdata$pred <- predict(RF_model, newdata = ig_testdata,type="class")
ig_testdata$pred <- as.factor(ig_testdata$pred)
cf = confusionMatrix(ig_testdata$pred, ig_testdata$TARGET)
print(cf)

RF_model <- randomForest(TARGET ~.,data=ig_traindata,importance = TRUE,trControl = trainControl(method = "cv",number = 10 ))
RF_model
plot(RF_model)
ig_testdata$pred <- predict(RF_model, newdata = ig_testdata,type="class")
ig_testdata$pred <- as.factor(ig_testdata$pred)
cf = confusionMatrix(ig_testdata$pred, ig_testdata$TARGET)
print(cf)
#---------------------------START : Hyperparameter Tuning-------------------------------------

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- caret::train(TARGET ~., data=ig_traindata, method="rf", 
                       metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

tunegrid <- expand.grid(.mtry=7,.ntree=c(100:200))
rf_gridsearch <- caret::train(TARGET ~., data=ig_traindata, method=customRF, 
                              metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

#--------------------------------STOP : Hyperparameter Tuning----------------------------------

#--------------------------------START : FINAL MODEL--------------------------------------------
RF_model_final <- randomForest(TARGET ~.,data=ig_traindata,importance = TRUE,ntree=194,mtry = 7,trControl = trainControl(method = "cv",number = 10))
plot(RF_model_final)
ig_testdata$pred <- predict(RF_model, newdata = ig_testdata,type="class")
ig_testdata$pred <- as.factor(ig_testdata$pred)
cf = confusionMatrix(ig_testdata$pred, ig_testdata$TARGET,mode = "everything")
cf

library(Metrics)
ig_testdata$TARGET <- as.numeric(ig_testdata$TARGET)
ig_testdata$pred <- as.numeric(ig_testdata$pred)
result <- rmse(ig_testdata$TARGET, ig_testdata$pred)
result

install.packages("pROC")
library(pROC)
roc_object <- roc( ig_testdata$TARGET, ig_testdata$pred)
roc_object
auc(roc_object)

#-------------------------------END : FINAL MODEL------------------------------------------

#-------------------------------------------------------------------------------------------------------------
#--------------------------END : RANDOM FOREST----------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------
#--------------------------START : LOGISTIC REGRESSION--------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------

library(readr)
library(tidymodels)
library(glmnet)

ga_lr_traindata$TARGET <- as.factor(ig_traindata$TARGET)
ga_lr_testdata$TARGET <- as.factor(ig_testdata$TARGET)

# Train a logistic regression model
model <- logistic_reg(mixture = double(1), penalty = double(1)) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(TARGET ~ ., data = ig_traindata)

# Model summary
tidy(model)

pred_class <- predict(model,
                      new_data = ig_testdata,
                      type = "class")

# Class Probabilities
pred_proba <- predict(model,
                      new_data = ig_testdata,
                      type = "prob")
results <- ig_testdata %>%
  select(TARGET) %>%
  bind_cols(pred_class, pred_proba)

results$TARGET <- as.factor(results$TARGET)
accuracy(results, truth = TARGET, estimate = .pred_class)
#0.567

#----HP:Start---
# Define the logistic regression model with penalty and mixture hyperparameters
log_reg <- logistic_reg(mixture = tune(), penalty = tune(), engine = "glmnet")

# Define the grid search for the hyperparameters
grid <- grid_regular(mixture(), penalty(), levels = c(mixture = 4, penalty = 3))

# Define the workflow for the model
log_reg_wf <- workflow() %>%
  add_model(log_reg) %>%
  add_formula(TARGET ~ .)

# Define the resampling method for the grid search
folds <- vfold_cv(train, v = 10)

# Tune the hyperparameters using the grid search
log_reg_tuned <- tune_grid(
  log_reg_wf,
  resamples = folds,
  grid = grid,
  control = control_grid(save_pred = TRUE)
)

select_best(log_reg_tuned, metric = "roc_auc")
#----HP:END------------
#------Final model-----
model <- logistic_reg(mixture = 0.333, penalty = 0.0000000001) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(TARGET ~ ., data = ig_traindata)

# Model summary
tidy(model)

pred_class <- predict(model,
                      new_data = ig_testdata,
                      type = "class")

# Class Probabilities
pred_proba <- predict(model,
                      new_data =ig_testdata,
                      type = "prob")
results <- ig_testdata %>%
  select(TARGET) %>%
  bind_cols(pred_class, pred_proba)
results$TARGET <- as.factor(results$TARGET)
accuracy(results, truth = TARGET, estimate = .pred_class)
#0.
#-------------------------------------------------------------------------------------------------------------
#--------------------------END : LOGISTIC REGRESSION----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
#--------------------------START : KNN --------------------------------------------
#-------------------------------------------------------------------------------------------------------------
install.packages("class")
library("class")

ig_data <- data_IG[c("TARGET","CODE_GENDER", "NAME_INCOME_TYPE","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS","NAME_HOUSING_TYPE", "OCCUPATION_TYPE")]
ig_data$TARGET <- as.factor(ig_data$TARGET)
str(ig_data)
train_index <- sample(1:nrow(ig_data), nrow(ig_data)*0.5)
ig_knn_traindata <- ig_data[train_index, ]
ig_knn_testdata <- ig_data[-train_index, ]

knn_train_data <- ig_knn_traindata[,c(-1)]
knn_test_data <- ig_knn_testdata[,c(-1)]
y_train <- ig_knn_traindata$TARGET
y_test <- ig_knn_testdata$TARGET

knn_test_pred <- knn(train=knn_train_data,test=knn_test_data,cl=y_train)
confusionMatrix(knn_test_pred,y_test,mode="everything")

#-------Choosing K value------------

#Using Caret
# Run algorithms using 10-fold cross validation
trainControl <- trainControl(method="repeatedcv", number=10)
metric <- "Accuracy"

set.seed(7)
grid <- expand.grid(.k=seq(1,25,by=1))
fit.knn <- caret::train(TARGET ~., data=ig_traindata, method="knn", 
                 metric=metric, tuneGrid=grid, trControl=trainControl)
knn.k2 <- fit.knn$bestTune 
print(fit.knn)
plot(fit.knn)

#Using the fit model to predict class for our test set, and print out the confusion matrix:
set.seed(1)
knn_test_data$pred <- predict(fit.knn$finalModel, newdata = knn_test_data,type="class")
knn_test_data$pred<- as.factor(knn_test_data$pred)
cf <- confusionMatrix(knn_test_data$pred, y_test,mode="everything")
print(cf)

library(Metrics)
y_test <- as.numeric(y_test)
knn_test_data$pred <- as.numeric(knn_test_data$pred)
result <- rmse(y_test, knn_test_data$pred)
result

#install.packages("pROC")
#library(pROC)
roc_object <- roc(y_test, knn_test_data$pred)
roc_object
plot(roc_object,main ="ROC curve -- KNN ")
auc(roc_object)

mcc(knn_test_data$pred, y_test)

knn_test_pred <- knn(train=knn_train_data,test=knn_test_data,cl=y_train,k=1)
confusionMatrix(knn_test_pred,y_test,mode="everything")

#REFERENCE : https://rpubs.com/pmtam/knn

#-------------------------------------------------------------------------------------------------------------
#--------------------------END : KNN --------------------------------------------
#-------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------
#------------------------START : SVM----------------------------------------------
#-------------------------------------------------------------------------------------------------------------

#Reference : https://uc-r.github.io/svm

# Fitting SVM to the Training set
install.packages('e1071')
library(e1071)

classifier = svm(formula = TARGET ~ .,
                 data = ig_traindata,
                 type = 'C-classification',
                 kernel = 'linear')

classifier

y_pred = predict(classifier, newdata = ig_testdata)
y_pred

cf <- confusionMatrix(y_pred, ig_testdata$TARGET)
print(cf)
#Accuracy : 0.5815

#Visualizing the Training set results
#plot decision boundaries and support vectors for the training data
plot(x = classifier, data = ig_traindata)
#plot reference : https://rdrr.io/rforge/e1071/man/plot.svm.html


#Reference : https://www.geeksforgeeks.org/classifying-data-using-support-vector-machinessvms-in-r/

#-------------START : HYPER PARAMETER TUNING ----------



m = tune.svm(x = ig_traindata[,-1],y=as.numeric(ig_traindata$TARGET),
             tunecontrol=tune.control(cross=5),cost=1:3,gamma=seq(0,0.5,by=0.1))

m$performances
m$best.parameters

#kernel type : polynomial
classifier_poly = svm(formula = TARGET ~ .,
                 data = ig_traindata,
                 type = 'C-classification',
                 kernel = 'polynomial')

y_pred_poly = predict(classifier_poly, newdata = ig_testdata)
cf <- confusionMatrix(y_pred_poly, ig_testdata$TARGET)
print(cf)


#kernel type : radial basis
classifier_radial = svm(formula = TARGET ~ .,
                      data = ig_traindata,
                      type = 'C-classification',
                      kernel = 'radial')

y_pred_radial = predict(classifier_radial, newdata = ig_testdata)
cf <- confusionMatrix(y_pred_radial, ig_testdata$TARGET)
print(cf)

#kernel type : sigmoid
classifier_sig = svm(formula = TARGET ~ .,
                        data = ig_traindata,
                        type = 'C-classification',
                        kernel = 'sigmoid')

y_pred_sig = predict(classifier_sig, newdata = ig_testdata)
cf <- confusionMatrix(y_pred_sig, ig_testdata$TARGET)
print(cf)


hyperparameters <- expand.grid(C = c(0.1, 1, 10),sigma = c(0.01, 0.1, 1))
grid_parameters <- expand.grid(C = c(0.1, 1, 10, 100),
                               sigma = c(1, 0.1, 0.01, 0.001),
                               kernel = c("poly", "rbf", "sigmoid", "linear"))

control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation
classifier = caret::train(form = TARGET ~ ., data = ig_traindata, method = 'svmRadial',tuneGrid = grid_parameters,trControl = control)
classifier
print(classifier$bestTune)
print(classifier$results)

#-------------END : HYPER PARAMETER TUNING ----------

#------------ START : FINAL MODEL----------------------

classifier = svm(formula = TARGET ~ .,
                 data = ig_traindata,
                 type = 'C-classification',
                 kernel = 'radial',gamma = 0.5, cost = 3,trControl = trainControl(method = "cv",number = 2))

classifier

y_pred = predict(classifier, newdata = ig_testdata)
y_pred

cf <- confusionMatrix(y_pred, ig_testdata$TARGET,mode = "everything")
print(cf)

ig_testdata$TARGET <- as.numeric(ig_testdata$TARGET)
y_pred <- as.numeric(y_pred)
result <- rmse(ig_testdata$TARGET,y_pred)
result

#install.packages("pROC")
#library(pROC)
roc_object <- roc(ig_testdata$TARGET, y_pred)
roc_object
plot(roc_object ,main ="ROC curve -- SVM ")
auc(roc_object)

#library(mltools)
mcc(y_pred, ig_testdata$TARGET)

#-------------- END : FINAL MODEL ------------------------------
#-------------------------------------------------------------------------------------------------------------
#------------------------END : SVM----------------------------------------------
#-------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------
#------------------------- START : DECISION TREE------------------------------------
#-------------------------------------------------------------------------------------------------------------

dim(ig_traindata)
dim(ig_testdata)

#Build the model
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

fit <- rpart(TARGET~., data = ig_traindata, method = 'class')
rpart.plot(fit, extra = 106)

#Make a prediction
predict_unseen <-predict(fit, ig_testdata, type = 'class')

#Testing the applicant who didn't make it and those who did.
cf <- confusionMatrix(predict_unseen, ig_testdata$TARGET)
print(cf)

#------------- START : HYPER PARAMETER TUNING --------------------------
trainControl <- trainControl(method="repeatedcv", number=10)
metric <- "Accuracy"

set.seed(7)
grid <- expand.grid(.cp=seq(0.01,0.1,by=1))
model_dt <- caret::train(TARGET ~., data=ig_traindata, method="rpart", 
                        metric=metric, tuneGrid=grid, trControl=trainControl)


grid <- expand.grid(maxdepth=seq(1,30,by=1))
model_dt <- caret::train(TARGET ~., data=ig_traindata, method="rpart2", 
                         metric=metric, tuneGrid=grid, trControl=trainControl)
#------------- END : HYPER PARAMETER TUNING --------------------------


#-------------- START : FINAL MODEL ------------------------------

fit <- rpart(TARGET~., data = ig_traindata, method = 'class',control=rpart.control(maxdepth = 12,cp = 0.01))
rpart.plot(fit, extra = 106)

#Make a prediction
predict_unseen <-predict(model_dt$finalModel, ig_testdata, type = 'class')

#Testing the applicant who didn't make it and those who did.
cf <- confusionMatrix(predict_unseen, ig_testdata$TARGET,mode = "everything")
print(cf)

ig_testdata$TARGET <- as.numeric(ig_testdata$TARGET)
pred <- as.numeric(predict_unseen)
result <- rmse(ig_testdata$TARGET,pred)
result

#install.packages("pROC")
library(pROC)
roc_object <- roc(ig_testdata$TARGET, pred)
roc_object
plot(roc_object ,main ="ROC curve -- Decision Tree ")
auc(roc_object)

library(mltools)
mcc(pred, ig_testdata$TARGET)

#-------------- END : FINAL MODEL ------------------------------

#-------------------------------------------------------------------------------------------------------------
#------------------------- END : DECISION TREE------------------------------------
#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
#------------------------- START : ANN------------------------------------
#-------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(neuralnet)

data <- ig_data
data <- data %>% mutate_if(is.character, as.factor)

set.seed(245)
data_rows <- floor(0.70 * nrow(data))
train_indices <- sample(c(1:nrow(data)), data_rows)
train_data <- data[train_indices,]
test_data <- data[-train_indices,]

model = neuralnet(
  TARGET~CODE_GENDER+NAME_INCOME_TYPE+NAME_EDUCATION_TYPE+NAME_FAMILY_STATUS+NAME_HOUSING_TYPE+OCCUPATION_TYPE,
  data=train_data,
  linear.output = FALSE,
  threshold = 0.2
)

plot(model,rep = "best")

pred <- predict(model, test_data)
labels <- c(1,2)
prediction_label <- data.frame(max.col(pred)) %>%     
  mutate(pred=labels[max.col.pred.]) %>%
  select(2) %>%
  unlist()

table(test_data$TARGET, prediction_label)

check = as.numeric(test_data$TARGET) == max.col(pred)
accuracy = (sum(check)/nrow(test_data))*100
print(accuracy)

trainControl <- trainControl(method="repeatedcv", number=10)
metric <- "Accuracy"
hyper_grid <- expand.grid(size= c(5, 10, 20),decay= c(0.01, 0.05, 0.1))
set.seed(123)
tuned_nn <- caret::train(TARGET~CODE_GENDER+NAME_INCOME_TYPE+NAME_EDUCATION_TYPE+NAME_FAMILY_STATUS+NAME_HOUSING_TYPE+OCCUPATION_TYPE, 
                  data = train_data, method = "nnet", tuneGrid = hyper_grid, 
                  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
tuned_nn$finalModel

#----Final Model-----
pred <- predict(tuned_nn$finalModel,test_data,type='class')
confusionMatrix(as.factor(pred),test_data$TARGET,mode = "everything")

library(Metrics)
test_data$TARGET <- as.numeric(ig_testdata$TARGET)
pred <- as.numeric(pred)
result <- rmse(test_data$TARGET,pred)
result

#install.packages("pROC")
library(pROC)
roc_object <- roc(test_data$TARGET, pred)
roc_object
plot(roc_object ,main ="ROC curve -- ANN ")
auc(roc_object)

library(mltools)
mcc(pred, test_data$TARGET)

#-------------------------------------------------------------------------------------------------------------
#------------------------- STOP : ANN------------------------------------
#-------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------
#-------------------------- START : XGBOOST ----------------------------------
#-------------------------------------------------------------------------------------------------------------

#Step 1 - Install the necessary libraries
install.packages('xgboost')     # for fitting the xgboost model
install.packages('caret')       # for general data preparation and model fitting
install.packages('e1071')

library(xgboost)
library(caret)  
library(e1071)           

#Step 2 - Train and Test data
# createDataPartition() function from the caret package to split the original dataset into a training and testing set and split data into training (80%) and testing set (20%)
parts = createDataPartition(ig_data$TARGET, p = 0.7, list = F)
train = ig_data[parts, ]
test = ig_data[-parts, ]

X_train = data.matrix(train[,-1])                  # independent variables for train
y_train = train[,1]                                # dependent variables for train

X_test = data.matrix(test[,-1])                    # independent variables for test
y_test = test[,1]                                   # dependent variables for test

# convert the train and test data into xgboost matrix type.
xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
xgboost_test = xgb.DMatrix(data=X_test, label=y_test)

#Step 3 - Create a xgboost model
# train a model using our training data
model <- xgboost(data = xgboost_train,                    # the data   
                 max.depth=3,                          # max depth 
                 nrounds=50)                              # max number of boosting iterations

summary(model)

#Step 4 - Make predictions on the test dataset
#use model to make predictions on test data
pred_test = predict(model, xgboost_test)
pred_test


#Step 5 - Convert prediction to factor type
pred_test[(pred_test>3)] = 3
pred_y = as.factor((levels(y_test))[round(pred_test)])
print(pred_y)

#Step 6 - Create a confusion matrix
conf_mat = confusionMatrix(y_test, pred_y)
print(conf_mat)

xgb_grid_1 = expand.grid(max_depth = c(3, 5, 7), 
                         nrounds = (1:10)*50,    # number of trees
                         # default values below
                         eta = c(0.01, 0.001, 0.0001),
                         gamma = 0,
                         subsample = 1,
                         min_child_weight = 1,
                         colsample_bytree = 0.6)
control <- trainControl(method = "cv", number = 10)
model = caret::train(TARGET~.,data=ig_traindata,method='xgbTree',trControl=control,tunegrid=xgb_grid_1)
print(model)

#-------------------------------------------------------------------------------------------------------------
#-------------------------- STOP : XGBOOST ----------------------------------
#-------------------------------------------------------------------------------------------------------------

#Plot bar chart for accuracies of each model
library(ggplot2)
library(viridis)
model <- c(rep("RF",2),rep("KNN",2),rep("SVM",2),rep("DT",2),rep("ANN",2),rep("XGB",2),rep("LR",2))
condition <- rep(c("before tuning","after tuning"),7)
accuracy <- c(89.97,91.31,86.37,91.14,78.13,82.21,83.58,85.06,64.07,80.29,91.03,92.23,56.9,57.61)
ig_models_accuracy_data <- data.frame(model,condition,accuracy)

ggplot(ig_models_accuracy_data,aes(fill=condition,y=accuracy,x=model))+
  ylim(0,100)+
  geom_col(position = "dodge",stat = "identity") + 
  theme_gray()+
  scale_fill_manual(values=c("#999999", "#CC79A7"))







