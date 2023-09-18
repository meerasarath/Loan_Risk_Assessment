#******************************************************************************************
#------------------------------- FEATURE SELECTION -------------------------------------
##******************************************************************************************

#---------------------------START : FEATURE SELECTION (IG)------------------------------------
data_IG
train_index <- sample(1:nrow(data_IG), nrow(data_IG)*0.7)
IG_traindata <- data_IG[train_index, ]
IG_testdata <- data_IG[-train_index, ]

library(FSelector)
ig <- information.gain(formula(data_IG),data_IG)
avg_ig <- sum(information.gain(formula(data_IG),data_IG))/122
cat("Average Information Gain is ", avg_ig)

#Result:
#Average Information Gain is  0.0002092386

#Select features with IG >  0.0002092386
subset(ig, ig > 0.0002092386)

#Result:
#attr_importance
#CODE_GENDER                   0.0002520892
#NAME_TYPE_SUITE               0.0019443035
#NAME_INCOME_TYPE              0.0004440629
#NAME_EDUCATION_TYPE           0.0006107991
#NAME_FAMILY_STATUS            0.0010707567
#NAME_HOUSING_TYPE             0.0010604147
#OCCUPATION_TYPE               0.0032269065
#WEEKDAY_APPR_PROCESS_START    0.0011314198
#ORGANIZATION_TYPE             0.0118365475
#FONDKAPREMONT_MODE            0.0010302739
#HOUSETYPE_MODE                0.0010121415
#WALLSMATERIAL_MODE            0.0013132013
#EMERGENCYSTATE_MODE           0.0003073781

#---------------------------END : FEATURE SELECTION (IG)------------------------------------

#--------------------------- START : DATA SUBSETTING --------------------------------------

##------Information Gain-------

colnames(data_IG)[2] <- "TARGET"
ig_data <- data_IG[c("TARGET","CODE_GENDER", "NAME_INCOME_TYPE","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS","NAME_HOUSING_TYPE", "OCCUPATION_TYPE")]
ig_data$TARGET <- as.factor(ig_data$TARGET)
str(ig_data)
train_index <- sample(1:nrow(ig_data), nrow(ig_data)*0.7)
ig_traindata <- ig_data[train_index, ]
ig_testdata <- ig_data[-train_index, ]

#--------------------------- END : DATA SUBSETTING --------------------------------------

