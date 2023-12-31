#******************************************************************************************
#------------------------------- FEATURE SUBSETTING AFTER GA -------------------------------------
##******************************************************************************************

str(data_ga) #From file dissertation_datacleaning.R
GA_DT_data <- data_ga[c("TARGET","FLAG_OWN_CAR","CNT_CHILDREN","AMT_INCOME_TOTAL","AMT_ANNUITY","NAME_INCOME_TYPE","NAME_HOUSING_TYPE","REGION_POPULATION_RELATIVE","DAYS_BIRTH","DAYS_EMPLOYED","CNT_FAM_MEMBERS","REGION_RATING_CLIENT","HOUR_APPR_PROCESS_START","REG_REGION_NOT_LIVE_REGION","REG_REGION_NOT_WORK_REGION","LIVE_CITY_NOT_WORK_CITY","EXT_SOURCE_1","EXT_SOURCE_3","YEARS_BUILD_AVG","ELEVATORS_AVG","ENTRANCES_AVG","BASEMENTAREA_MODE","YEARS_BEGINEXPLUATATION_MODE","YEARS_BUILD_MODE","ELEVATORS_MODE","ENTRANCES_MODE","LIVINGAPARTMENTS_MODE","NONLIVINGAPARTMENTS_MODE","BASEMENTAREA_MEDI","YEARS_BUILD_MEDI","ELEVATORS_MEDI","FLOORSMAX_MEDI","FLOORSMIN_MEDI","LIVINGAPARTMENTS_MEDI","LIVINGAREA_MEDI","NONLIVINGAREA_MEDI","TOTALAREA_MODE","FLAG_DOCUMENT_3")]
train_index <- sample(1:nrow(GA_DT_data), nrow(GA_DT_data)*0.7)
ga_dt_traindata <- GA_DT_data[train_index, ]
ga_dt_testdata <- GA_DT_data[-train_index, ]

str(data_ga)
GA_ANN_data <- data_ga[c("TARGET","NAME_CONTRACT_TYPE","CNT_CHILDREN","AMT_INCOME_TOTAL","AMT_ANNUITY","NAME_INCOME_TYPE","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS","NAME_HOUSING_TYPE","REGION_POPULATION_RELATIVE","DAYS_BIRTH","DAYS_REGISTRATION","OWN_CAR_AGE","CNT_FAM_MEMBERS","REGION_RATING_CLIENT","REG_REGION_NOT_WORK_REGION","REG_CITY_NOT_LIVE_CITY","EXT_SOURCE_3","APARTMENTS_AVG","BASEMENTAREA_AVG","YEARS_BEGINEXPLUATATION_AVG","COMMONAREA_AVG","ELEVATORS_AVG","LIVINGAPARTMENTS_AVG","NONLIVINGAREA_AVG","APARTMENTS_MODE","YEARS_BEGINEXPLUATATION_MODE","YEARS_BUILD_MODE","COMMONAREA_MODE","FLOORSMAX_MODE","FLOORSMIN_MODE","LIVINGAPARTMENTS_MODE","LIVINGAREA_MODE","NONLIVINGAREA_MODE","BASEMENTAREA_MEDI","ELEVATORS_MEDI","ENTRANCES_MEDI","FLOORSMAX_MEDI","FLOORSMIN_MEDI","LANDAREA_MEDI","LIVINGAPARTMENTS_MEDI","LIVINGAREA_MEDI","NONLIVINGAPARTMENTS_MEDI","TOTALAREA_MODE","FLAG_DOCUMENT_3")]
train_index <- sample(1:nrow(GA_ANN_data), nrow(GA_ANN_data)*0.7)
ga_ann_traindata <- GA_ANN_data[train_index, ]
ga_ann_testdata <- GA_ANN_data[-train_index, ]

str(data_ga)
GA_XGBoost_data <- data_ga[c("TARGET","CODE_GENDER","FLAG_OWN_CAR","AMT_INCOME_TOTAL","NAME_INCOME_TYPE","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS","DAYS_REGISTRATION","DAYS_ID_PUBLISH","FLAG_MOBIL","CNT_FAM_MEMBERS","REGION_RATING_CLIENT_W_CITY","REG_REGION_NOT_LIVE_REGION","REG_CITY_NOT_WORK_CITY","APARTMENTS_AVG","BASEMENTAREA_AVG","YEARS_BUILD_AVG","LANDAREA_AVG","COMMONAREA_MODE","LIVINGAPARTMENTS_MODE","NONLIVINGAREA_MODE","BASEMENTAREA_MEDI","YEARS_BEGINEXPLUATATION_MEDI","COMMONAREA_MEDI","ELEVATORS_MEDI","FLOORSMAX_MEDI","FLOORSMIN_MEDI","LIVINGAPARTMENTS_MEDI","LIVINGAREA_MEDI","NONLIVINGAREA_MEDI","TOTALAREA_MODE","FLAG_DOCUMENT_3")]
train_index <- sample(1:nrow(GA_XGBoost_data), nrow(GA_XGBoost_data)*0.7)
ga_xg_traindata <- GA_XGBoost_data[train_index, ]
ga_xg_testdata <- GA_XGBoost_data[-train_index, ]

str(data_ga)
GA_KNN_data <- data_ga[c("TARGET","FLAG_OWN_CAR","CNT_CHILDREN","AMT_ANNUITY","NAME_INCOME_TYPE","REGION_POPULATION_RELATIVE","DAYS_BIRTH","DAYS_REGISTRATION","OWN_CAR_AGE","REGION_RATING_CLIENT_W_CITY","REG_REGION_NOT_WORK_REGION","REG_CITY_NOT_WORK_CITY","LIVE_CITY_NOT_WORK_CITY","EXT_SOURCE_2","APARTMENTS_AVG","YEARS_BUILD_AVG","ELEVATORS_AVG","NONLIVINGAPARTMENTS_AVG","NONLIVINGAREA_AVG","APARTMENTS_MODE","YEARS_BUILD_MODE","COMMONAREA_MODE","ELEVATORS_MODE","FLOORSMAX_MODE","NONLIVINGAPARTMENTS_MODE","ELEVATORS_MEDI","ENTRANCES_MEDI","FLOORSMAX_MEDI","LIVINGAREA_MEDI","NONLIVINGAPARTMENTS_MEDI","TOTALAREA_MODE","FLAG_DOCUMENT_3")]
train_index <- sample(1:nrow(GA_KNN_data), nrow(GA_KNN_data)*0.7)
ga_knn_traindata <- GA_KNN_data[train_index, ]
ga_knn_testdata <- GA_KNN_data[-train_index, ]


str(data_ga)
GA_LR_data <- data_ga[c("TARGET","CODE_GENDER","CNT_CHILDREN","AMT_ANNUITY","AMT_GOODS_PRICE","NAME_INCOME_TYPE","NAME_EDUCATION_TYPE","NAME_HOUSING_TYPE","REGION_POPULATION_RELATIVE","DAYS_BIRTH","DAYS_EMPLOYED","DAYS_ID_PUBLISH","OWN_CAR_AGE","CNT_FAM_MEMBERS","HOUR_APPR_PROCESS_START","REG_REGION_NOT_LIVE_REGION","REG_CITY_NOT_WORK_CITY","LIVE_CITY_NOT_WORK_CITY","EXT_SOURCE_1","EXT_SOURCE_2","YEARS_BEGINEXPLUATATION_AVG","COMMONAREA_AVG","ELEVATORS_AVG","FLOORSMIN_AVG","LIVINGAREA_AVG","NONLIVINGAPARTMENTS_AVG","YEARS_BEGINEXPLUATATION_MODE","YEARS_BUILD_MODE","COMMONAREA_MODE","ENTRANCES_MODE","FLOORSMAX_MODE","FLOORSMIN_MODE","LANDAREA_MODE","LIVINGAREA_MODE","YEARS_BEGINEXPLUATATION_MEDI","ELEVATORS_MEDI","ENTRANCES_MEDI","FLOORSMAX_MEDI","FLOORSMIN_MEDI","LIVINGAPARTMENTS_MEDI")]
train_index <- sample(1:nrow(GA_LR_data), nrow(GA_LR_data)*0.7)
ga_lr_traindata <- GA_LR_data[train_index, ]
ga_lr_testdata <- GA_LR_data[-train_index, ]


str(data_ga)
GA_SVM_data <- data_ga[c("TARGET","CODE_GENDER","CNT_CHILDREN","AMT_ANNUITY","AMT_GOODS_PRICE","NAME_INCOME_TYPE","NAME_EDUCATION_TYPE","NAME_HOUSING_TYPE","REGION_POPULATION_RELATIVE","DAYS_BIRTH","DAYS_EMPLOYED","DAYS_ID_PUBLISH","OWN_CAR_AGE","FLAG_MOBIL","CNT_FAM_MEMBERS","HOUR_APPR_PROCESS_START","REG_REGION_NOT_LIVE_REGION","REG_CITY_NOT_WORK_CITY","LIVE_CITY_NOT_WORK_CITY","EXT_SOURCE_1","EXT_SOURCE_2","YEARS_BEGINEXPLUATATION_AVG","COMMONAREA_AVG","ELEVATORS_AVG","FLOORSMIN_AVG","LIVINGAREA_AVG","NONLIVINGAPARTMENTS_AVG","YEARS_BEGINEXPLUATATION_MODE","YEARS_BUILD_MODE","COMMONAREA_MODE","ENTRANCES_MODE","FLOORSMAX_MODE","FLOORSMIN_MODE","LANDAREA_MODE","LIVINGAREA_MODE","YEARS_BEGINEXPLUATATION_MEDI","ELEVATORS_MEDI","ENTRANCES_MEDI","FLOORSMAX_MEDI","FLOORSMIN_MEDI","LIVINGAPARTMENTS_MEDI")]
train_index <- sample(1:nrow(GA_SVM_data), nrow(GA_SVM_data)*0.7)
ga_svm_traindata <- GA_SVM_data[train_index, ]
ga_svm_testdata <- GA_SVM_data[-train_index, ]


str(data_ga)
GA_RF_data <- data_ga[c("TARGET","FLAG_OWN_CAR","CNT_CHILDREN","AMT_INCOME_TOTAL","AMT_GOODS_PRICE","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS","DAYS_BIRTH","DAYS_REGISTRATION","OWN_CAR_AGE","FLAG_MOBIL","OCCUPATION_TYPE","REGION_RATING_CLIENT_W_CITY","HOUR_APPR_PROCESS_START","REG_REGION_NOT_LIVE_REGION","REG_REGION_NOT_WORK_REGION","REG_CITY_NOT_WORK_CITY","LIVE_CITY_NOT_WORK_CITY","EXT_SOURCE_2","APARTMENTS_AVG","FLOORSMIN_AVG","LIVINGAPARTMENTS_AVG","LIVINGAREA_AVG","APARTMENTS_MODE","YEARS_BUILD_MODE","COMMONAREA_MODE","LIVINGAPARTMENTS_MODE","LIVINGAREA_MODE","NONLIVINGAPARTMENTS_MODE","APARTMENTS_MEDI","YEARS_BEGINEXPLUATATION_MEDI","YEARS_BUILD_MEDI","FLOORSMAX_MEDI","LIVINGAREA_MEDI","NONLIVINGAPARTMENTS_MEDI","NONLIVINGAREA_MEDI")]
train_index <- sample(1:nrow(GA_RF_data), nrow(GA_RF_data)*0.7)
ga_rf_traindata <- GA_RF_data[train_index, ]
ga_rf_testdata <- GA_RF_data[-train_index, ]

#----Bar diagram to show the no.of features selected & similar features for each model using GA & PSO
library(ggplot2)
library(viridis)
model <- c(rep("DT",3),rep("ANN",3),rep("XGB",3),rep("KNN",3),rep("LR",3),rep("SVM",3),rep("RF",3))
condition <- rep(c("GA","PSO","Similar"),7)
features <- c(37,38,14,44,39,22,31,39,17,31,38,16,39,39,20,40,39,20,35,37,17)
features_selected_for_models <- data.frame(model,condition,features)

ggplot(features_selected_for_models,aes(fill=condition,y=features,x=model))+
  ylim(0,100)+
  geom_col(position = "dodge") + 
  theme_gray()+
  geom_text(aes(label=features),colour = "white", size = 3,
            vjust = 1.5, position = position_dodge(.9))+
  ggtitle("Feature Selection for All Models")

