#-------------------------------------------------------------------------------------------
#--------------------------START : RANDOM FOREST--------------------------------------------
#-------------------------------------------------------------------------------------------
library("randomForest")
set.seed(1234)
RF_model <- randomForest(TARGET ~.,data=ga_rf_traindata,importance = TRUE,trControl = trainControl(method = "cv",number = 10 ))
RF_model
plot(RF_model)
ga_rf_testdata$pred <- predict(RF_model, newdata = ga_rf_testdata,type="class")
ga_rf_testdata$pred <- as.factor(ga_rf_testdata$pred)
cf = confusionMatrix(ga_rf_testdata$pred, ga_rf_testdata$TARGET)
print(cf)

#tune mtry
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=seq(7,10,by=1))
rf_gridsearch <- caret::train(TARGET ~., data=ga_rf_traindata, method="rf", 
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
tunegrid <- expand.grid(.mtry=8,.ntree=seq(500,510,by=1))
rf_gridsearch <- caret::train(TARGET ~., data=ig_traindata, method=customRF, 
                              metric=metric, tuneGrid=tunegrid, trControl=control)

#--------------------------------START : FINAL MODEL--------------------------------------------
RF_model <- randomForest(TARGET ~.,data=ga_rf_traindata,importance = TRUE,ntree=500,mtry = 8,trControl = trainControl(method = "cv",number = 10))
ga_rf_testdata$pred <- predict(RF_model, newdata = ga_rf_testdata,type="class")
ga_rf_testdata$pred <- as.factor(ga_rf_testdata$pred)
cf = confusionMatrix(ga_rf_testdata$pred, ga_rf_testdata$TARGET,mode="everything")

fourfoldplot(cf$table, color = c("cyan", "pink"),
             conf.level = 0, margin = 1, main = "Confusion Matrix of GA-RF")

library(Metrics)
ga_rf_testdata$TARGET <- as.numeric(ga_rf_testdata$TARGET)
ga_rf_testdata$pred <- as.numeric(ga_rf_testdata$pred)
result <- rmse(ga_rf_testdata$TARGET,ga_rf_testdata$pred)
result

library(pROC)
roc_object <- roc(ga_rf_testdata$TARGET, ga_rf_testdata$pred)
roc_rf <- roc_object
plot(roc_object ,main ="ROC curve -- Random Forest ")
auc(roc_object)

library(mltools)
mcc(ga_rf_testdata$pred, ga_rf_testdata$TARGET)
#-------------------------------------------------------------------------------------------------------------
#--------------------------START : LOGISTIC REGRESSION--------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------

library(readr)
library(tidymodels)
library(glmnet)

ga_lr_traindata$TARGET <- as.factor(ga_lr_traindata$TARGET)
ga_lr_testdata$TARGET <- as.factor(ga_lr_testdata$TARGET)

# Train a logistic regression model
model <- logistic_reg(mixture = double(1), penalty = double(1)) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(TARGET ~ ., data = ga_lr_traindata)

# Model summary
tidy(model)

pred_class <- predict(model,
                      new_data = ga_lr_testdata,
                      type = "class")

# Class Probabilities
pred_proba <- predict(model,
                      new_data = ga_lr_testdata,
                      type = "prob")
results <- ga_lr_testdata %>%
  select(TARGET) %>%
  bind_cols(pred_class, pred_proba)

accuracy(results, truth = TARGET, estimate = .pred_class)
#73.2

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
  fit(TARGET ~ ., data = ga_lr_traindata)

# Model summary
tidy(model)

pred_class <- predict(model,
                      new_data = ga_lr_testdata,
                      type = "class")

# Class Probabilities
pred_proba <- predict(model,
                      new_data = ga_lr_testdata,
                      type = "prob")
results <- ga_lr_testdata %>%
  select(TARGET) %>%
  bind_cols(pred_class, pred_proba)

accuracy(results, truth = TARGET, estimate = .pred_class)
#0.745
precision(results, truth = TARGET, estimate = .pred_class)
#0.762
recall(results, truth = TARGET, estimate = .pred_class)
#0.752
mcc(results, truth = TARGET, estimate = .pred_class)
#0.489
results$TARGET <- as.numeric(results$TARGET)
rmse(results, truth = TARGET, estimate = .pred_class)
#0.505

F1 <- 2 * ( 0.762* 0.752) / (0.762 + 0.752) 

results$.pred_class <- as.numeric(results$.pred_class)
roc_object <- roc(ga_lr_testdata$TARGET,results$.pred_class)
roc_RF <- roc_object
plot(roc_object ,main ="ROC curve -- LR ")
auc(roc_object)
#0.7446

#----Final model:ENd-------

#-------------------------------------------------------------------------------------------------------------
#--------------------------START : KNN --------------------------------------------
#-------------------------------------------------------------------------------------------------------------
install.packages("class")
library("class")

str(data_ga)
GA_KNN_data <- data_ga[c("TARGET","FLAG_OWN_CAR","CNT_CHILDREN","AMT_ANNUITY","NAME_INCOME_TYPE","REGION_POPULATION_RELATIVE","DAYS_BIRTH","DAYS_REGISTRATION","OWN_CAR_AGE","REGION_RATING_CLIENT_W_CITY","REG_REGION_NOT_WORK_REGION","REG_CITY_NOT_WORK_CITY","LIVE_CITY_NOT_WORK_CITY","EXT_SOURCE_2","APARTMENTS_AVG","YEARS_BUILD_AVG","ELEVATORS_AVG","NONLIVINGAPARTMENTS_AVG","NONLIVINGAREA_AVG","APARTMENTS_MODE","YEARS_BUILD_MODE","COMMONAREA_MODE","ELEVATORS_MODE","FLOORSMAX_MODE","NONLIVINGAPARTMENTS_MODE","ELEVATORS_MEDI","ENTRANCES_MEDI","FLOORSMAX_MEDI","LIVINGAREA_MEDI","NONLIVINGAPARTMENTS_MEDI","TOTALAREA_MODE","FLAG_DOCUMENT_3")]
train_index <- sample(1:nrow(GA_KNN_data), nrow(GA_KNN_data)*0.5)
ga_knn_traindata <- GA_KNN_data[train_index, ]
ga_knn_testdata <- GA_KNN_data[-train_index, ]

knn_train_data <- ga_knn_traindata[,c(-1)]
knn_test_data <- ga_knn_testdata[,c(-1)]
y_train <- ga_knn_traindata$TARGET
y_test <- ga_knn_testdata$TARGET

knn_test_pred <- knn(train=knn_train_data,test=knn_test_data,cl=y_train)
confusionMatrix(knn_test_pred,y_test)

#---choosing k-----
trainControl <- trainControl(method="repeatedcv", number=10)
metric <- "Accuracy"

set.seed(7)
grid <- expand.grid(.k=seq(1,25,by=1))
fit.knn <- caret::train(TARGET ~., data=ga_knn_traindata, method="knn", 
                        metric=metric, tuneGrid=grid, trControl=trainControl)
knn.k2 <- fit.knn$bestTune # keep this optimal k for testing with stand alone knn() function in next section
print(fit.knn)

#-------------------------------------------------------------------------------------------------------------
#------------------------START : SVM----------------------------------------------
#-------------------------------------------------------------------------------------------------------------
library(e1071)

classifier = svm(formula = TARGET ~ .,
                 data = ig_traindata,
                 type = 'C-classification',
                 kernel = 'radial')

classifier
y_pred = predict(classifier, newdata = ig_testdata)
y_pred
cf <- confusionMatrix(y_pred, ig_testdata$TARGET)
print(cf)

m = tune.svm(x = ig_traindata[,-1],y=as.numeric(ig_traindata$TARGET),
             tunecontrol=tune.control(cross=5),cost=1:3,gamma=seq(0,0.5,by=0.1))
m$performances
m$best.parameters

#------------ START : FINAL MODEL----------------------

classifier = svm(formula = TARGET ~ .,
                 data = ig_traindata,
                 type = 'C-classification',
                 kernel = 'radial',gamma = 0.5, cost = 3,trControl = trainControl(method = "cv",number = 2))

classifier
y_pred = predict(classifier, newdata = ig_testdata)
y_pred
cf <- confusionMatrix(y_pred, ig_testdata$TARGET)
print(cf)

#------------ END : FINAL MODEL----------------------

#-------------------------------------------------------------------------------------------------------------
#------------------------- START : DECISION TREE------------------------------------
#-------------------------------------------------------------------------------------------------------------

dim(ig_traindata)
dim(ig_testdata)

#Build the model
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

fit <- rpart(TARGET~., data = ga_dt_traindata, method = 'class')
rpart.plot(fit, extra = 106)

#Make a prediction
predict_unseen <-predict(fit, ga_dt_testdata, type = 'class')

#Testing the applicant who didn't make it and those who did.
cf <- confusionMatrix(predict_unseen, ga_dt_testdata$TARGET)
print(cf)

trainControl <- trainControl(method="repeatedcv", number=10)
metric <- "Accuracy"

--------
set.seed(7)
grid <- expand.grid(.cp=seq(0.01,0.1,by=1))
model_dt <- caret::train(TARGET ~., data=ga_dt_traindata, method="rpart", 
                         metric=metric, tuneGrid=grid, trControl=trainControl)

grid <- expand.grid(maxdepth=seq(1,30,by=1))
model_dt <- caret::train(TARGET ~., data=ga_dt_traindata, method="rpart2", 
                         metric=metric, tuneGrid=grid, trControl=trainControl)

#-------------- START : FINAL MODEL ------------------------------

fit <- rpart(TARGET~., data = ga_dt_traindata, method = 'class',control=rpart.control(maxdepth = 12,cp = 0.01))
rpart.plot(fit, extra = 106)

#Make a prediction
predict_unseen <-predict(fit, ga_dt_testdata, type = 'class')

#Testing the applicant who didn't make it and those who did.
cf <- confusionMatrix(predict_unseen, ga_dt_testdata$TARGET)
print(cf)

#----------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
#------------------------- START : ANN------------------------------------
#-------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(neuralnet)

data <- GA_ANN_data
data <- data %>% mutate_if(is.character, as.factor)

set.seed(245)
data_rows <- floor(0.70 * nrow(data))
train_indices <- sample(c(1:nrow(data)), data_rows)
train_data <- data[train_indices,]
test_data <- data[-train_indices,]

model = neuralnet(
  TARGET~NAME_CONTRACT_TYPE+CNT_CHILDREN+AMT_INCOME_TOTAL+AMT_ANNUITY+NAME_INCOME_TYPE+NAME_EDUCATION_TYPE+NAME_FAMILY_STATUS+NAME_HOUSING_TYPE+REGION_POPULATION_RELATIVE+DAYS_BIRTH+DAYS_REGISTRATION+OWN_CAR_AGE+CNT_FAM_MEMBERS+REGION_RATING_CLIENT+REG_REGION_NOT_WORK_REGION+REG_CITY_NOT_LIVE_CITY+EXT_SOURCE_3+APARTMENTS_AVG+BASEMENTAREA_AVG+YEARS_BEGINEXPLUATATION_AVG+COMMONAREA_AVG+ELEVATORS_AVG+LIVINGAPARTMENTS_AVG+NONLIVINGAREA_AVG+APARTMENTS_MODE+YEARS_BEGINEXPLUATATION_MODE+YEARS_BUILD_MODE+COMMONAREA_MODE+FLOORSMAX_MODE+FLOORSMIN_MODE+LIVINGAPARTMENTS_MODE+LIVINGAREA_MODE+NONLIVINGAREA_MODE+BASEMENTAREA_MEDI+ELEVATORS_MEDI+ENTRANCES_MEDI+FLOORSMAX_MEDI+FLOORSMIN_MEDI+LANDAREA_MEDI+LIVINGAPARTMENTS_MEDI+LIVINGAREA_MEDI+NONLIVINGAPARTMENTS_MEDI+TOTALAREA_MODE+FLAG_DOCUMENT_3,
  data=train_data,
  linear.output = FALSE,
  threshold = 0.2
)
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
#52.71503

trainControl <- trainControl(method="repeatedcv", number=10)
metric <- "Accuracy"
hyper_grid <- expand.grid(size= c(5, 10, 20),decay= c(0.01, 0.05, 0.1))
set.seed(123)
tuned_nn <- caret::train(TARGET~NAME_CONTRACT_TYPE+CNT_CHILDREN+AMT_INCOME_TOTAL+AMT_ANNUITY+NAME_INCOME_TYPE+NAME_EDUCATION_TYPE+NAME_FAMILY_STATUS+NAME_HOUSING_TYPE+REGION_POPULATION_RELATIVE+DAYS_BIRTH+DAYS_REGISTRATION+OWN_CAR_AGE+CNT_FAM_MEMBERS+REGION_RATING_CLIENT+REG_REGION_NOT_WORK_REGION+REG_CITY_NOT_LIVE_CITY+EXT_SOURCE_3+APARTMENTS_AVG+BASEMENTAREA_AVG+YEARS_BEGINEXPLUATATION_AVG+COMMONAREA_AVG+ELEVATORS_AVG+LIVINGAPARTMENTS_AVG+NONLIVINGAREA_AVG+APARTMENTS_MODE+YEARS_BEGINEXPLUATATION_MODE+YEARS_BUILD_MODE+COMMONAREA_MODE+FLOORSMAX_MODE+FLOORSMIN_MODE+LIVINGAPARTMENTS_MODE+LIVINGAREA_MODE+NONLIVINGAREA_MODE+BASEMENTAREA_MEDI+ELEVATORS_MEDI+ENTRANCES_MEDI+FLOORSMAX_MEDI+FLOORSMIN_MEDI+LANDAREA_MEDI+LIVINGAPARTMENTS_MEDI+LIVINGAREA_MEDI+NONLIVINGAPARTMENTS_MEDI+TOTALAREA_MODE+FLAG_DOCUMENT_3,
                         data = train_data, method = "nnet", tuneGrid = hyper_grid, 
                         trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
#--------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
#-------------------------- START : XGBOOST ----------------------------------
#-------------------------------------------------------------------------------------------------------------
library(xgboost)
library(caret)  
library(e1071)           

#Step 2 - Train and Test data
# createDataPartition() function from the caret package to split the original dataset into a training and testing set and split data into training (80%) and testing set (20%)
parts = createDataPartition(GA_XGBoost_data$TARGET, p = 0.7, list = F)
train = GA_XGBoost_data[parts, ]
test = GA_XGBoost_data[-parts, ]

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
#Accuracy : 0.9157 

xgb_grid_1 = expand.grid(max_depth = c(3, 5, 7), 
                         nrounds = (1:10)*50,    # number of trees
                         # default values below
                         eta = c(0.01, 0.001, 0.0001),
                         gamma = 0,
                         subsample = 1,
                         min_child_weight = 1,
                         colsample_bytree = 0.6)
control <- trainControl(method = "cv", number = 10)
model = caret::train(TARGET~.,data=xgboost_train,method='xgbTree',trControl=control,tunegrid=xgb_grid_1)
print(model)

#------------------------------------------------------
#Plot bar chart for the accuracy of each model
library(ggplot2)
library(viridis)
model <- c(rep("RF",2),rep("KNN",2),rep("SVM",2),rep("DT",2),rep("ANN",2),rep("XGB",2),rep("LR",2))
condition <- rep(c("before tuning","after tuning"),7)
accuracy <- c(95.85,96.11,64.9,65.48,78.08,81.82,74.24,74.71,52.72,56.32,91.57,92.28,73.2,74.56)
GA_models_accuracy_data <- data.frame(model,condition,accuracy)

ggplot(GA_models_accuracy_data,aes(fill=condition,y=accuracy,x=model))+
  ylim(0,100)+
  geom_col(position = "dodge",stat = "identity") + 
  theme_gray()+
  scale_fill_manual(values=c("#9999CC", "#66CC99"))
