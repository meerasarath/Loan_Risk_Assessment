#File to find the features using PSO method.

library(pso) # load pso 

#Functions to be called from run_PSO_FS_test()
feature_selection_PSO_DT <- function(chromosome,xx,yy)
{
  featureIndices <- which(chromosome[1:length(chromosome)] >= 0.5)
  fit <- rpart(TARGET~., data = runPSO_traindata, method = 'class')
  predict_unseen <-predict(fit, runPSO_testdata, type = 'class')
  cf <- confusionMatrix(predict_unseen, runPSO_testdata$TARGET)
  return(cf$overall[1])
}

feature_selection_PSO_logistic_regression <- function(chromosome,xx,yy)
{
  featureIndices <- which(chromosome[1:length(chromosome)] >= 0.5)
  xx <<- runPSO_traindata
  yy <<- runPSO_testdata
  model_glm <- glm(TARGET ~., data = runPSO_traindata, family = binomial(link='logit'))
  p <- predict(model_glm, newdata=runPSO_testdata, type="response")
  pr <- prediction(p, runPSO_testdata$TARGET)
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  return(auc)
}

feature_selection_PSO_ANN <- function(chromosome,xx,yy)
{
  featureIndices <- which(chromosome[1:length(chromosome)] >= 0.5)
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
    data=runPSO_traindata,
    linear.output = FALSE,
    threshold = 0.2
  )
  
  pred <- predict(model, runPSO_testdata)
  labels <- c(1,2)
  prediction_label <- data.frame(max.col(pred)) %>%     
    mutate(pred=labels[max.col.pred.]) %>%
    select(2) %>%
    unlist()
  
  table(runPSO_testdata$TARGET, prediction_label)
  
  check = as.numeric(runPSO_testdata$TARGET) == max.col(pred)
  accuracy = (sum(check)/nrow(runPSO_testdata))*100
  return(accuracy)
}

feature_selection_PSO_XGBoost <- function(chromosome,xx,yy)
{
  featureIndices <- which(chromosome[1:length(chromosome)] >= 0.5)
  X_train = data.matrix(runPSO_traindata[,-1])                  # independent variables for train
  y_train = runPSO_traindata[,1]                                # dependent variables for train
  
  X_test = data.matrix(runPSO_testdata[,-1])                    # independent variables for test
  y_test = runPSO_testdata[,1] 
  
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

feature_selection_PSO_SVM <- function(chromosome,xx,yy)
{
  xx <<- runPSO_traindata
  yy <<- runPSO_testdata
  featureIndices <- which(chromosome[1:length(chromosome)] >= 0.5)
  print("Creating the model")
  classifier = svm(formula = TARGET ~ .,
                   data = runPSO_traindata,
                   type = 'C-classification',
                   kernel = 'linear')
  print("model created")
  y_pred = predict(classifier, newdata = runPSO_testdata)
  cf <- confusionMatrix(y_pred, runPSO_testdata$TARGET)
  print("Calculated accuracy")
  return(cf$overall[1])
}

feature_selection_random_forest <- function(xx,yy)
{
  xx <<- runPSO_traindata
  yy <<- runPSO_testdata
  
  RF_model <- randomForest(TARGET ~.,data=rrunPSO_traindata,importance = TRUE)
  runGA_testdata$pred <- predict(RF_model, newdata = runPSO_testdata,type="class")
  
  #Maximise accuracy
  return( mean(runPSO_testdata$TARGET == runPSO_testdata$pred))
  
}


feature_selection_KNN <- function(xx,yy)
{
  xx <<- runPSO_traindata
  yy <<- runPSO_testdata
  metric <- "Accuracy"
  set.seed(7)
  grid <- expand.grid(.k=seq(1,20,by=1))
  fit.knn <- train(TARGET ~., data=runPSO_traindata, method="knn", 
                   metric=metric, tuneGrid=grid)
  prediction <- predict(fit.knn, newdata = runPSO_testdata)
  cf <- confusionMatrix(prediction, runPSO_testdata$TARGET)
  return(cf$overall[1])
}



getPSOParams <- function(){
  #problem specific parameters
  #D=1;  #2 dimensional solutions 
  #lower=rep(-5.2,D)  #lower bound on the solution variables
  #upper=rep(5.2,D)   #upper bound on the solution variables
  # type '?psoptim' to understand why we are setting up the parameters below.
  #C=list(trace=1,maxit=10,REPORT=1,trace.stats=1,s=5)  #Control parameters for PSO; see "Details" after typing ?psoptim
  
  data <- runPSO_traindata
  xx <<- data[,-ncol(data)]
  yy <<- data[,ncol(data)]
  lower.bounds <- c( -1, -1, rep(0, ncol(xx)))
  upper.bounds <- c(10, 10, rep(1, ncol(xx)))
  
  C=list(trace=1,maxit=20,trace.stats=1,s=60,v.max=6,c.p=2,c.g=2)
  #C=list(trace=1,trace.stats=1,v.max=6,c.p=2,c.g=2)
  #p <- list(D=D,lower=lower, upper=upper,C=C)
  p <- list(D=2,C=C) 
  return (p)
}

run_PSO_FS_test <- function(){
  set.seed(12345) # set for replicability 
  
  best_fitness <- matrix(nrow = 40, ncol = 30)
  bestfitnessPSO <<- 0
  bestSolutionPSO <<- NULL
  
  p <- getPSOParams()
  data <- pso_data
  xx <<- data[,-ncol(data)]
  yy <<- data[,ncol(data)]
  lower.bounds <- c( -1, -1, rep(0, ncol(xx)))
  upper.bounds <- c(10, 10, rep(1, ncol(xx)))
  
  noRuns = 30
  
  # perform the optimization:
  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    print("Calling PSO optim")
    PSO=psoptim(rep(NA,121),fn=feature_selection_PSO_SVM,lower=lower.bounds,      #verify (?psoptim) to understand why we are passing these arguments
                upper=upper.bounds,control=p$C)
    #psoptim(par=rep(NA,121),fn=getFitnessPSO)
    #cat("best:",PSO$par,"f:",PSO$value,"\n")                #print the best solution and its fitness
    print("PSoptim completed")
    best_fitness[,i]<- PSO$stats$error
    #print("best value")
    #print(PSO$value)
    if (bestfitnessPSO < abs(PSO$value)){
      bestfitnessPSO <<- abs(PSO$value)
      bestSolutionPSO <<- PSO$par
    }
    print(bestfitnessPSO)
    print(bestSolutionPSO)
  }
  return (best_fitness)                    
}


getBestFitnessPSO<-function(){
  return(bestfitnessPSO)
}

getBestSolutionPSO<-function(){
  return(bestSolutionPSO)
}

# result:
plotresults <-function(PSO,j=1){        #supply a PSO object, and the index of the solution variable to plot. 
  p <- getPSOParams()                   #get the PSO parameters set up as above
  
  s <- p$C$s                            #extract the required parameters out of p
  maxit <- p$C$maxit
  pdf("psoptim1.pdf",width=5,height=5)  #set up an output PDF file
  #plot the value of the jth solution variable as iterations go on. 
  plot(xlim=c(1,maxit),rep(1,s),PSO$stats$x[[1]][j,],pch=19,
       xlab="iterations",ylab=paste("s_",j," value",sep=""))
  for(i in 2:maxit) points(rep(i,s),PSO$stats$x[[i]][j,],pch=19)
  dev.off()
  pdf("psoptim2.pdf",width=5,height=5)
  plot(PSO$stats$error,type="l",lwd=2,xlab="iterations",
       ylab="best fitness")
  dev.off()
}



