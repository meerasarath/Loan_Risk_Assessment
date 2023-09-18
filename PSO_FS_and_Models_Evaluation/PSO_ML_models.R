#-------------------------------------------------------------------------------------------------------------
#--------------------------START : LOGISTIC REGRESSION--------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------

library(readr)
library(tidymodels)
library(glmnet)

pso_lr_traindata$TARGET <- as.factor(pso_lr_traindata$TARGET)
pso_lr_testdata$TARGET <- as.factor(pso_lr_testdata$TARGET)

# Train a logistic regression model
model <- logistic_reg(mixture = double(1), penalty = double(1)) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(TARGET ~ ., data = pso_lr_traindata)

# Model summary
tidy(model)

pred_class <- predict(model,
                      new_data = pso_lr_testdata,
                      type = "class")

# Class Probabilities
pred_proba <- predict(model,
                      new_data = pso_lr_testdata,
                      type = "prob")
results <- pso_lr_testdata %>%
  select(TARGET) %>%
  bind_cols(pred_class, pred_proba)

accuracy(results, truth = TARGET, estimate = .pred_class)
#0.569

#------Hyperparameter tuning----------
param_grid <- expand.grid(
  parameter = c(0.001, 0.01, 0.1, 1, 10)  # Adjust these values as needed
)

ctrl <- trainControl(
  method = "cv",    # Use k-fold cross-validation
  number = 10,      # Number of folds
  verboseIter = TRUE
)

# Build the logistic regression model with grid search
lr_tuned_model <- caret::train(
  TARGET ~ .,          # Adjust the formula accordingly
  data = pso_lr_traindata,
  method = "glm",
  family = binomial,
  trControl = ctrl,
  tuneGrid = param_grid
)

#-------------------------------------------------------------------------------------------------------------
#------------------------- START : ANN------------------------------------
#-------------------------------------------------------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)
install.packages("neuralnet")
library(neuralnet)

data <- PSO_ANN_data
data <- data %>% mutate_if(is.character, as.factor)

set.seed(245)
data_rows <- floor(0.70 * nrow(data))
train_indices <- sample(c(1:nrow(data)), data_rows)
train_data <- data[train_indices,]
test_data <- data[-train_indices,]

model = neuralnet(
  TARGET~NAME_CONTRACT_TYPE+AMT_CREDIT+AMT_ANNUITY+NAME_INCOME_TYPE+NAME_EDUCATION_TYPE+REGION_POPULATION_RELATIVE+DAYS_EMPLOYED+DAYS_REGISTRATION+OCCUPATION_TYPE+CNT_FAM_MEMBERS+REGION_RATING_CLIENT+HOUR_APPR_PROCESS_START+REG_CITY_NOT_LIVE_CITY+REG_CITY_NOT_WORK_CITY+EXT_SOURCE_2+APARTMENTS_AVG+BASEMENTAREA_AVG+ELEVATORS_AVG+FLOORSMAX_AVG+FLOORSMIN_AVG+LANDAREA_AVG+LIVINGAPARTMENTS_AVG+LIVINGAREA_AVG+NONLIVINGAREA_AVG+YEARS_BEGINEXPLUATATION_MODE+ELEVATORS_MODE+FLOORSMAX_MODE+LANDAREA_MODE+LIVINGAPARTMENTS_MODE+NONLIVINGAPARTMENTS_MODE+NONLIVINGAREA_MODE+YEARS_BUILD_MEDI+ELEVATORS_MEDI+ENTRANCES_MEDI+FLOORSMAX_MEDI+LIVINGAREA_MEDI+NONLIVINGAPARTMENTS_MEDI+NONLIVINGAREA_MEDI+FLAG_DOCUMENT_3,
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
tuned_nn <- caret::train(TARGET~NAME_CONTRACT_TYPE+AMT_CREDIT+AMT_ANNUITY+NAME_INCOME_TYPE+NAME_EDUCATION_TYPE+REGION_POPULATION_RELATIVE+DAYS_EMPLOYED+DAYS_REGISTRATION+OCCUPATION_TYPE+CNT_FAM_MEMBERS+REGION_RATING_CLIENT+HOUR_APPR_PROCESS_START+REG_CITY_NOT_LIVE_CITY+REG_CITY_NOT_WORK_CITY+EXT_SOURCE_2+APARTMENTS_AVG+BASEMENTAREA_AVG+ELEVATORS_AVG+FLOORSMAX_AVG+FLOORSMIN_AVG+LANDAREA_AVG+LIVINGAPARTMENTS_AVG+LIVINGAREA_AVG+NONLIVINGAREA_AVG+YEARS_BEGINEXPLUATATION_MODE+ELEVATORS_MODE+FLOORSMAX_MODE+LANDAREA_MODE+LIVINGAPARTMENTS_MODE+NONLIVINGAPARTMENTS_MODE+NONLIVINGAREA_MODE+YEARS_BUILD_MEDI+ELEVATORS_MEDI+ENTRANCES_MEDI+FLOORSMAX_MEDI+LIVINGAREA_MEDI+NONLIVINGAPARTMENTS_MEDI+NONLIVINGAREA_MEDI+FLAG_DOCUMENT_3,
                         data = train_data, method = "nnet", tuneGrid = hyper_grid, 
                         trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))

#-------------------------------------------------------------------------------------------------------------
#------------------------- START : XGBoost------------------------------------
#-------------------------------------------------------------------------------------------------------------
library(xgboost)
library(caret)  
library(e1071)           

#Step 2 - Train and Test data
# createDataPartition() function from the caret package to split the original dataset into a training and testing set and split data into training (80%) and testing set (20%)
parts = createDataPartition(PSO_XGBoost_data$TARGET, p = 0.7, list = F)
train = PSO_XGBoost_data[parts, ]
test = PSO_XGBoost_data[-parts, ]

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
model = caret::train(TARGET~.,data=pso_xg_traindata,method='xgbTree',trControl=control,tunegrid=xgb_grid_1)
print(model)
xgb_best_model <- model$finalModel

#--final model

param <- list(max_depth=3,eta=0.4,gamma=0,colsample_bytree=0.6,min_child_weight=1,subsample=0.75)
xgb_model <- xgboost(param,data = xgboost_train,                    # the data   
                 nrounds=150) 
pred_test = predict(xgb_model, xgboost_test)
pred_test[(pred_test>3)] = 3
pred_y = as.factor((levels(y_test))[round(pred_test)])
conf_mat = confusionMatrix(y_test, pred_y,mode = "everything")

rmse(y_test,predict(xgb_model,xgboost_test))

install.packages("pROC")
library(pROC)
roc_object <- roc(y_test, pred_y)
roc_object
plot(roc_object ,main ="ROC curve -- XGB ")
auc(roc_object)

library(Metrics)
library(mltools)
mcc(pred_y,y_test)

rmse(as.numeric(y_test),as.numeric(pred_y))
y_test <- as.numeric(y_test)
pred_y <- as.numeric(pred_y)
sqrt(mean((y_test - pred_y)^2, na.rm = TRUE))
#-------------------------------------------------------------------------------------------------------------
#------------------------START : SVM----------------------------------------------
#-------------------------------------------------------------------------------------------------------------

#Reference : https://uc-r.github.io/svm

# Fitting SVM to the Training set
install.packages('e1071')
library(e1071)

classifier = svm(formula = TARGET ~ .,
                 data = pso_svm_traindata,
                 type = 'C-classification',
                 kernel = 'linear')

classifier

y_pred = predict(classifier, newdata = pso_svm_testdata)
y_pred

cf <- confusionMatrix(y_pred, pso_svm_testdata$TARGET)
print(cf)

#Visualizing the Training set results
#plot decision boundaries and support vectors for the training data
plot(x = classifier, data = ga_svm_traindata)
#plot reference : https://rdrr.io/rforge/e1071/man/plot.svm.html


#Reference : https://www.geeksforgeeks.org/classifying-data-using-support-vector-machinessvms-in-r/

#-------------START : HYPER PARAMETER TUNING ----------



m = tune.svm(x = pso_svm_traindata[,-1],y=as.numeric(pso_svm_traindata$TARGET),
             tunecontrol=tune.control(cross=5),cost=1:3,gamma=seq(0,0.5,by=0.1))

m$performances
m$best.parameters


#kernel type : polynomial
classifier_poly = svm(formula = TARGET ~ .,
                      data = pso_svm_traindata,
                      type = 'C-classification',
                      kernel = 'polynomial')

y_pred_poly = predict(classifier_poly, newdata = ga_svm_testdata)
cf <- confusionMatrix(y_pred_poly, ga_svm_testdata$TARGET)
print(cf)

#kernel type : radial basis
classifier_radial = svm(formula = TARGET ~ .,
                        data = pso_svm_traindata,
                        type = 'C-classification',
                        kernel = 'radial')

y_pred_radial = predict(classifier_radial, newdata = pso_svm_testdata)
cf <- confusionMatrix(y_pred_radial, pso_svm_testdata$TARGET)
print(cf)

#kernel type : sigmoid
classifier_sig = svm(formula = TARGET ~ .,
                     data = pso_svm_traindata,
                     type = 'C-classification',
                     kernel = 'sigmoid')

y_pred_sig = predict(classifier_sig, newdata = pso_svm_testdata)
cf <- confusionMatrix(y_pred_sig, pso_svm_testdata$TARGET)
print(cf)

#------------ START : FINAL MODEL----------------------

classifier = svm(formula = TARGET ~ .,
                 data = pso_svm_traindata,
                 type = 'C-classification',
                 kernel = 'radial',gamma = 0.5, cost = 3,trControl = trainControl(method = "cv",number = 10))

classifier

y_pred = predict(classifier, newdata = pso_svm_testdata)
y_pred

cf <- confusionMatrix(y_pred, pso_svm_testdata$TARGET)
print(cf)

#-------------------------------------------------------------------------------------------
#--------------------------START : RANDOM FOREST--------------------------------------------
#-------------------------------------------------------------------------------------------

#Installing library
install.packages("randomForest")
library("randomForest")
set.seed(2500)

RF_model <- randomForest(TARGET ~.,data=pso_rf_traindata,importance = TRUE)
RF_model
plot(RF_model)

pso_rf_testdata$pred <- predict(RF_model, newdata = pso_rf_testdata,type="class")
pso_rf_testdata$pred <- as.factor(pso_rf_testdata$pred)
cf = confusionMatrix(pso_rf_testdata$pred, pso_rf_testdata$TARGET)
print(cf)

#tune mtry
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=seq(20,40,by=2))
rf_gridsearch <- caret::train(TARGET ~., data=pso_rf_traindata, method="rf", 
                              metric=metric, tuneGrid=tunegrid, trControl=control)

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

#tune ntree
tunegrid <- expand.grid(.mtry=20,.ntree=242)
rf_gridsearch <- caret::train(TARGET ~., data=pso_rf_traindata, method=customRF, 
                              metric=metric, tuneGrid=tunegrid, trControl=control)
#--------------------------------START : FINAL MODEL--------------------------------------------
RF_model <- randomForest(TARGET ~.,data=pso_rf_traindata,importance = TRUE,ntree=242,mtry = 20,trControl = trainControl(method = "cv",number = 10))
pso_rf_testdata$pred <- predict(rf_gridsearch$finalModel, newdata = pso_rf_testdata,type="class")
pso_rf_testdata$pred <- as.factor(pso_rf_testdata$pred)
cf = confusionMatrix(pso_rf_testdata$pred, pso_rf_testdata$TARGET,mode="everything")

pso_rf_testdata$TARGET <- as.numeric(pso_rf_testdata$TARGET)
pso_rf_testdata$pred <- as.numeric(pso_rf_testdata$pred)
result <- rmse(pso_rf_testdata$TARGET,pso_rf_testdata$pred)
result

library(pROC)
roc_object <- roc(pso_rf_testdata$TARGET, pso_rf_testdata$pred)
roc_object
plot(roc_object ,main ="ROC curve -- Random Forest ")
auc(roc_object)

library(mltools)
mcc(pso_rf_testdata$pred, pso_rf_testdata$TARGET)

#-------------------------------------------------------------------------------------------------------------
#------------------------- START : DECISION TREE--------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#Build the model
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
#-------------------------------
set.seed(2345)
fit <- rpart(TARGET~., data = pso_dt_traindata, method = 'class')
rpart.plot(fit, extra = 106)

#Make a prediction
predict_unseen <-predict(fit, pso_dt_testdata, type = 'class')

#Testing the applicant who didn't make it and those who did.
cf <- confusionMatrix(predict_unseen, pso_dt_testdata$TARGET)
print(cf)
#------------------------------------
trainControl <- trainControl(method="repeatedcv", number=10)
metric <- "Accuracy"

set.seed(7)
grid <- expand.grid(.cp=seq(0.01,0.1,by=1))
model_dt <- caret::train(TARGET ~., data=pso_dt_traindata, method="rpart", 
                         metric=metric, tuneGrid=grid, trControl=trainControl)

set.seed(2345)
grid <- expand.grid(maxdepth=seq(1,30,by=1))
model_dt <- caret::train(TARGET ~., data=pso_dt_traindata, method="rpart2", 
                         metric=metric, tuneGrid=grid, trControl=trainControl)
#----------------------------------------------
#Final Model
set.seed(1200)
fit <- rpart(TARGET~., data = pso_dt_traindata, method = 'class',control=rpart.control(maxdepth = 10,cp = 0.01))
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, pso_dt_testdata, type = 'class')
cf <- confusionMatrix(predict_unseen, pso_dt_testdata$TARGET)
print(cf)

#-------------------------------------------------------------------------------------------------------------
#------------------------- END : DECISION TREE--------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
#--------------------------START : KNN --------------------------------------------
#-------------------------------------------------------------------------------------------------------------
library("class")

PSO_KNN_data <- data_pso[c("TARGET","NAME_CONTRACT_TYPE","CODE_GENDER","FLAG_OWN_CAR","AMT_CREDIT","AMT_ANNUITY","AMT_GOODS_PRICE","NAME_EDUCATION_TYPE","DAYS_ID_PUBLISH","REGION_RATING_CLIENT_W_CITY","REG_REGION_NOT_LIVE_REGION","REG_CITY_NOT_LIVE_CITY","REG_CITY_NOT_WORK_CITY","LIVE_CITY_NOT_WORK_CITY","EXT_SOURCE_1","EXT_SOURCE_3","APARTMENTS_AVG","ENTRANCES_AVG","FLOORSMAX_AVG","FLOORSMIN_AVG","LIVINGAREA_AVG","NONLIVINGAREA_AVG","YEARS_BUILD_MODE","ELEVATORS_MODE","ENTRANCES_MODE","FLOORSMAX_MODE","LANDAREA_MODE","LIVINGAREA_MODE","NONLIVINGAPARTMENTS_MODE","NONLIVINGAREA_MODE","APARTMENTS_MEDI","COMMONAREA_MEDI","ELEVATORS_MEDI","ENTRANCES_MEDI","FLOORSMAX_MEDI","LANDAREA_MEDI","LIVINGAPARTMENTS_MEDI","TOTALAREA_MODE","FLAG_DOCUMENT_3")]
train_index <- sample(1:nrow(PSO_KNN_data), nrow(PSO_KNN_data)*0.5)
pso_knn_traindata <- PSO_KNN_data[train_index, ]
pso_knn_testdata <- PSO_KNN_data[-train_index, ]

knn_train_data <- pso_knn_traindata[,c(-1)]
knn_test_data <- pso_knn_testdata[,c(-1)]
y_train <- pso_knn_traindata$TARGET
y_test <- pso_knn_testdata$TARGET

set.seed(150)
knn_test_pred <- knn(train=knn_train_data,test=knn_test_data,cl=y_train)
confusionMatrix(knn_test_pred,y_test)

#-------Choosing K value------------

#Using Caret
# Run algorithms using 10-fold cross validation
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

set.seed(7)
grid <- expand.grid(.k=seq(1,3,by=0.25))
fit.knn <- caret::train(TARGET ~., data=pso_knn_traindata, method="knn", metric=metric, tuneGrid=grid, trControl=trainControl)
knn.k2 <- fit.knn$bestTune # keep this optimal k for testing with stand alone knn() function in next section
print(fit.knn)
plot(fit.knn)

#Using the fit model to predict class for our test set, and print out the confusion matrix:
set.seed(1)
prediction <- predict(fit.knn, newdata = pso_knn_testdata)
cf <- confusionMatrix(prediction, pso_knn_testdata$TARGET)
print(cf)

#-------------------------------------------------------------------------------------------------------------
#--------------------------END : KNN --------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#Plot bar chart for accuracy of each model
library(ggplot2)
library(viridis)
model <- c(rep("RF",2),rep("KNN",2),rep("SVM",2),rep("DT",2),rep("ANN",2),rep("XGB",2),rep("LR",2))
condition <- rep(c("before tuning","after tuning"),7)
accuracy <- c(95.83,96.06,79.17,79.21,58.75,59.77,82.15,83.45,52.17,60.15,92.63,96.5,72.7,73.66)
pso_models_accuracy_data <- data.frame(model,condition,accuracy)

ggplot(pso_models_accuracy_data,aes(fill=condition,y=accuracy,x=model))+
  ylim(0,100)+
  geom_col(position = "dodge",stat = "identity") + 
  theme_gray()+
  scale_fill_manual(values=c("#999999", "#CC79A7"))


