#Functions to be called from runGA.R
feature_selection_random_forest <- function(xx,yy)
{
  xx <<- runGA_traindata
  yy <<- runGA_testdata
  RF_model <- randomForest(TARGET ~.,data=runGA_traindata,importance = TRUE)
  runGA_testdata$pred <- predict(RF_model, newdata = runGA_testdata,type="class")
  
  #Maximise accuracy
  return( mean(runGA_testdata$TARGET == runGA_testdata$pred))
  
}

feature_selection_logistic_regression <- function(xx,yy)
{
  xx <<- runGA_traindata
  yy <<- runGA_testdata
  model_glm <- glm(TARGET ~., data = runGA_traindata, family = binomial(link='logit'))
  p <- predict(model_glm, newdata=runGA_testdata, type="response")
  pr <- prediction(p, runGA_testdata$TARGET)
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  return(auc)
}


feature_selection_KNN <- function(xx,yy)
{
  xx <<- runGA_traindata
  yy <<- runGA_testdata
  metric <- "Accuracy"
  set.seed(7)
  grid <- expand.grid(.k=seq(1,20,by=1))
  fit.knn <- train(TARGET ~., data=runGA_traindata, method="knn", 
                 metric=metric, tuneGrid=grid)
  prediction <- predict(fit.knn, newdata = runGA_testdata)
  cf <- confusionMatrix(prediction, runGA_testdata$TARGET)
  return(cf$overall[1])
}

feature_selection_DT <- function(xx,yy)
{
  xx <<- runGA_traindata
  yy <<- runGA_testdata
  fit <- rpart(TARGET~., data = runGA_traindata, method = 'class')
  predict_unseen <-predict(fit, runGA_testdata, type = 'class')
  cf <- confusionMatrix(predict_unseen, runGA_testdata$TARGET)
  return(cf$overall[1])
}

feature_selection_SVM <- function(xx,yy)
{
  xx <<- runGA_traindata
  yy <<- runGA_testdata
  
  print("Creating the model")
  classifier = svm(formula = TARGET ~ .,
                   data = runGA_traindata,
                   type = 'C-classification',
                   kernel = 'linear')
  print("model created")
  y_pred = predict(classifier, newdata = runGA_testdata)
  cf <- confusionMatrix(y_pred, runGA_testdata$TARGET)
  print("Calculated accuracy")
  return(cf$overall[1])
}

feature_selection_ANN <- function(xx,yy)
{
  xx <<- runGA_traindata
  yy <<- runGA_testdata
  model = neuralnet(
    TARGET~NAME_CONTRACT_TYPE+CODE_GENDER+FLAG_OWN_CAR+CNT_CHILDREN+AMT_INCOME_TOTAL+AMT_CREDIT+AMT_ANNUITY+AMT_GOODS_PRICE         
    +NAME_INCOME_TYPE+NAME_EDUCATION_TYPE+NAME_FAMILY_STATUS+NAME_HOUSING_TYPE+REGION_POPULATION_RELATIVE
    +DAYS_BIRTH+DAYS_EMPLOYED+DAYS_REGISTRATION+DAYS_ID_PUBLISH+OWN_CAR_AGE+FLAG_MOBIL+OCCUPATION_TYPE+CNT_FAM_MEMBERS 
    +REGION_RATING_CLIENT+REGION_RATING_CLIENT_W_CITY+HOUR_APPR_PROCESS_START+REG_REGION_NOT_LIVE_REGION+REG_REGION_NOT_WORK_REGION
    +REG_CITY_NOT_LIVE_CITY+REG_CITY_NOT_WORK_CITY+LIVE_CITY_NOT_WORK_CITY+EXT_SOURCE_1+EXT_SOURCE_2+EXT_SOURCE_3+APARTMENTS_AVG+
      BASEMENTAREA_AVG+YEARS_BEGINEXPLUATATION_AVG+YEARS_BUILD_AVG+COMMONAREA_AVG+ELEVATORS_AVG+ENTRANCES_AVG+FLOORSMAX_AVG+FLOORSMIN_AVG
    +LANDAREA_AVG+LIVINGAPARTMENTS_AVG+LIVINGAREA_AVG+NONLIVINGAPARTMENTS_AVG+NONLIVINGAREA_AVG+APARTMENTS_MODE+BASEMENTAREA_MODE+
      YEARS_BEGINEXPLUATATION_MODE+YEARS_BUILD_MODE+COMMONAREA_MODE+ELEVATORS_MODE+ENTRANCES_MODE+FLOORSMAX_MODE+FLOORSMIN_MODE+LANDAREA_MODE+
      LIVINGAPARTMENTS_MODE+LIVINGAREA_MODE+NONLIVINGAPARTMENTS_MODE+NONLIVINGAREA_MODE+APARTMENTS_MEDI+BASEMENTAREA_MEDI+YEARS_BEGINEXPLUATATION_MEDI+
      YEARS_BUILD_MEDI+COMMONAREA_MEDI+ELEVATORS_MEDI+ENTRANCES_MEDI+FLOORSMAX_MEDI+FLOORSMIN_MEDI+LANDAREA_MEDI+LIVINGAPARTMENTS_MEDI+LIVINGAREA_MEDI+
      NONLIVINGAPARTMENTS_MEDI+NONLIVINGAREA_MEDI+TOTALAREA_MODE,
    data=runGA_traindata,
    linear.output = FALSE,
    threshold = 0.2
  )
  
  pred <- predict(model, runGA_testdata)
  labels <- c(1,2)
  prediction_label <- data.frame(max.col(pred)) %>%     
    mutate(pred=labels[max.col.pred.]) %>%
    select(2) %>%
    unlist()
  
  table(runGA_testdata$TARGET, prediction_label)
  
  check = as.numeric(runGA_testdata$TARGET) == max.col(pred)
  accuracy = (sum(check)/nrow(runGA_testdata))*100
  return(accuracy)
}

feature_selection_XGBoost <- function(xx,yy)
{
  xx <<- runGA_traindata
  yy <<- runGA_testdata
  
  X_train = data.matrix(runGA_traindata[,-1])                  # independent variables for train
  y_train = runGA_traindata[,1]                                # dependent variables for train
  
  X_test = data.matrix(runGA_testdata[,-1])                    # independent variables for test
  y_test = runGA_testdata[,1] 
  
  xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
  xgboost_test = xgb.DMatrix(data=X_test, label=y_test)
  
  model <- xgboost(data = xgboost_train,                    # the data   
                   max.depth=3,                          # max depth 
                   nrounds=50)                              # max number of boosting iterations
  
  pred_test = predict(model, xgboost_test)
  pred_test[(pred_test>3)] = 3
  pred_y = as.factor((levels(y_test))[round(pred_test)])
  conf_mat = confusionMatrix(y_test, pred_y)
  return(conf_mat$overall[1])
}




