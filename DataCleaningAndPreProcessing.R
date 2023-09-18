#---------------------------START : DATA READ--------------------------------------
dataframe <- read.csv("/Users/meeramohanm/BCU/Dissertation/home-credit-default-risk/application_train.csv")
nrow(dataframe)
str(dataframe)

# Calculate missing value percentage for each column
print("Missing Value Percentage for Each Column in the Dataset")
for(i in 1:ncol(data))
{
  missing_percentage = (sum(is.na(data[i])) / nrow(data[i])) * 100
  cat(colnames(data[i]), ":", missing_percentage,"\n")
}




#---------------------------END : DATA READ---------------------------------------------


#******************************************************************************************
#------------------------------- DATA PREPROCESSING -------------------------------------
##******************************************************************************************

#---------------------------START : DATA CLEANING--------------------------------------
#check for null values
is.null(dataframe) 
#Result : FALSE

#Check for duplicate data
sum(duplicated(dataframe))
#Result : 0

#Replace blanks with NA
dataframe[dataframe == ''] <- NA


#Check for NA values
table(is.na.data.frame(dataframe))
#Result : 
#FALSE     TRUE 
#2,91,28,248  83,88,094 
cleaned_data <- na.omit(dataframe)
table(is.na.data.frame(cleaned_data))
#Result:
#FALSE 
#1384822 

#data <- as.data.frame(unclass(cleaned_data),stringsAsFactors = TRUE)
data <- as.data.frame(unclass(cleaned_data))
str(data)

edaData <- cleaned_data
data <- cleaned_data
cor_data <- cleaned_data


#-----START : Data Imbalance Check and Oversampling-------------
x <- table(data$TARGET)
pie_labels <- paste0(round(100 * x/sum(x), 2), "%") #Computing %
pie(x, labels = pie_labels, main= "Data Imbalance Check")
legend("topright", legend <- c(0,1),fill = c("white", "lightblue")) #6.11%

table(data$TARGET)

if(!require('imbalance')) {
  install.packages('imbalance')
  library('imbalance')
}

data_FS <- data

data_FS$TARGET <- as.factor(data_FS$TARGET)
data_FS$NAME_CONTRACT_TYPE <- as.factor(data_FS$NAME_CONTRACT_TYPE)
data_FS$CODE_GENDER <- as.factor(data_FS$CODE_GENDER)
data_FS$FLAG_OWN_CAR <- as.factor(data_FS$FLAG_OWN_CAR)
data_FS$FLAG_OWN_REALTY <- as.factor(data_FS$FLAG_OWN_REALTY)
data_FS$NAME_CONTRACT_TYPE <- as.factor(data_FS$NAME_CONTRACT_TYPE)
data_FS$NAME_TYPE_SUITE <- as.factor(data_FS$NAME_TYPE_SUITE)
data_FS$NAME_INCOME_TYPE <- as.factor(data_FS$NAME_INCOME_TYPE)
data_FS$NAME_EDUCATION_TYPE   <- as.factor(data_FS$NAME_EDUCATION_TYPE)
data_FS$NAME_FAMILY_STATUS <- as.factor(data_FS$NAME_FAMILY_STATUS)
data_FS$NAME_HOUSING_TYPE <- as.factor(data_FS$NAME_HOUSING_TYPE)
data_FS$OCCUPATION_TYPE <- as.factor(data_FS$OCCUPATION_TYPE)
data_FS$WEEKDAY_APPR_PROCESS_START <- as.factor(data_FS$WEEKDAY_APPR_PROCESS_START)
data_FS$ORGANIZATION_TYPE <- as.factor(data_FS$ORGANIZATION_TYPE)
data_FS$FONDKAPREMONT_MODE <- as.factor(data_FS$FONDKAPREMONT_MODE)
data_FS$HOUSETYPE_MODE <- as.factor(data_FS$HOUSETYPE_MODE)
data_FS$WALLSMATERIAL_MODE <- as.factor(data_FS$WALLSMATERIAL_MODE)
data_FS$EMERGENCYSTATE_MODE  <- as.factor(data_FS$EMERGENCYSTATE_MODE)

data_FS$TARGET <- as.numeric(data_FS$TARGET)
data_FS$NAME_CONTRACT_TYPE <- as.numeric(data_FS$NAME_CONTRACT_TYPE)
data_FS$CODE_GENDER <- as.numeric(data_FS$CODE_GENDER)
data_FS$FLAG_OWN_CAR <- as.numeric(data_FS$FLAG_OWN_CAR)
data_FS$FLAG_OWN_REALTY <- as.numeric(data_FS$FLAG_OWN_REALTY)
data_FS$NAME_CONTRACT_TYPE <- as.numeric(data_FS$NAME_CONTRACT_TYPE)
data_FS$NAME_TYPE_SUITE <- as.numeric(data_FS$NAME_TYPE_SUITE)
data_FS$NAME_INCOME_TYPE <- as.numeric(data_FS$NAME_INCOME_TYPE)
data_FS$NAME_EDUCATION_TYPE   <- as.numeric(data_FS$NAME_EDUCATION_TYPE)
data_FS$NAME_FAMILY_STATUS <- as.numeric(data_FS$NAME_FAMILY_STATUS)
data_FS$NAME_HOUSING_TYPE <- as.numeric(data_FS$NAME_HOUSING_TYPE)
data_FS$OCCUPATION_TYPE <- as.numeric(data_FS$OCCUPATION_TYPE)
data_FS$WEEKDAY_APPR_PROCESS_START <- as.numeric(data_FS$WEEKDAY_APPR_PROCESS_START)
data_FS$ORGANIZATION_TYPE <- as.numeric(data_FS$ORGANIZATION_TYPE)
data_FS$FONDKAPREMONT_MODE <- as.numeric(data_FS$FONDKAPREMONT_MODE)
data_FS$HOUSETYPE_MODE <- as.numeric(data_FS$HOUSETYPE_MODE)
data_FS$WALLSMATERIAL_MODE <- as.numeric(data_FS$WALLSMATERIAL_MODE)
data_FS$EMERGENCYSTATE_MODE  <- as.numeric(data_FS$EMERGENCYSTATE_MODE)


colnames(data_FS)[2] <- "Class"
newDataset <- oversample(data_FS, ratio = 0.9, method = "MWMOTE") #52.63%, 47.37%
x <- table(newDataset$Class)
pie_labels <- paste0(round(100 * x/sum(x), 2), "%") #Computing %
pie(x, labels = pie_labels, main= "Data Balance Check After Oversampling")
legend("topright", legend <- c(0,1),fill = c("white", "lightblue"))

#-----END : Data Imbalance Check and Oversampling-------------

#Assigning oversampled dataset for IG,GA, and PSO FS methods.

data_IG <- newDataset

data_ga <- newDataset
data_ga <- data_ga[-1]
colnames(data_ga)[1] <- "TARGET"
data_ga$TARGET <- as.factor(data_ga$TARGET)
str(data_ga)
train_index <- sample(1:nrow(data_ga), nrow(data_ga)*0.7)
ga_traindata <- data_ga[train_index, ]
ga_testdata <- data_ga[-train_index, ]

#splitting the train set for wrapper function
runGA_train_index <- sample(1:nrow(ga_traindata), nrow(ga_traindata)*0.7)
runGA_traindata <- ga_traindata[runGA_train_index, ]
runGA_testdata <- ga_traindata[-runGA_train_index, ]


data_pso <- newDataset
data_pso <- data_pso[-1]
colnames(data_pso)[1] <- "TARGET"
data_pso$TARGET <- as.factor(data_pso$TARGET)
str(data_pso)
train_index <- sample(1:nrow(data_pso), nrow(data_pso)*0.7)
pso_traindata <- data_pso[train_index, ]
pso_testdata <- data_pso[-train_index, ]

nrow(runPSO_traindata)
pso_data <- slice_sample(pso_traindata,n=1000)
significant_data <- pso_data[,c("TARGET","NAME_CONTRACT_TYPE","CODE_GENDER","FLAG_OWN_CAR","CNT_CHILDREN","AMT_INCOME_TOTAL","AMT_CREDIT","AMT_ANNUITY","AMT_GOODS_PRICE",
                                "NAME_INCOME_TYPE","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS","NAME_HOUSING_TYPE","REGION_POPULATION_RELATIVE","DAYS_BIRTH",
                                "DAYS_EMPLOYED","DAYS_REGISTRATION","DAYS_ID_PUBLISH","OWN_CAR_AGE","FLAG_MOBIL",
                                "OCCUPATION_TYPE","CNT_FAM_MEMBERS","REGION_RATING_CLIENT",
                                "REGION_RATING_CLIENT_W_CITY","HOUR_APPR_PROCESS_START","REG_REGION_NOT_LIVE_REGION",
                                "REG_REGION_NOT_WORK_REGION","REG_CITY_NOT_LIVE_CITY","REG_CITY_NOT_WORK_CITY",
                                "LIVE_CITY_NOT_WORK_CITY","EXT_SOURCE_1","EXT_SOURCE_2","EXT_SOURCE_3","APARTMENTS_AVG",
                                "BASEMENTAREA_AVG","YEARS_BEGINEXPLUATATION_AVG","YEARS_BUILD_AVG","COMMONAREA_AVG","ELEVATORS_AVG","ENTRANCES_AVG","FLOORSMAX_AVG","FLOORSMIN_AVG",
                                "LANDAREA_AVG","LIVINGAPARTMENTS_AVG","LIVINGAREA_AVG","NONLIVINGAPARTMENTS_AVG",
                                "NONLIVINGAREA_AVG","APARTMENTS_MODE","BASEMENTAREA_MODE",
                                "YEARS_BEGINEXPLUATATION_MODE","YEARS_BUILD_MODE","COMMONAREA_MODE",
                                "ELEVATORS_MODE","ENTRANCES_MODE","FLOORSMAX_MODE","FLOORSMIN_MODE",
                                "LANDAREA_MODE","LIVINGAPARTMENTS_MODE","LIVINGAREA_MODE",
                                "NONLIVINGAPARTMENTS_MODE","NONLIVINGAREA_MODE","APARTMENTS_MEDI",
                                "BASEMENTAREA_MEDI","YEARS_BEGINEXPLUATATION_MEDI","YEARS_BUILD_MEDI",
                                "COMMONAREA_MEDI","ELEVATORS_MEDI","ENTRANCES_MEDI","FLOORSMAX_MEDI",
                                "FLOORSMIN_MEDI","LANDAREA_MEDI","LIVINGAPARTMENTS_MEDI","LIVINGAREA_MEDI",
                                "NONLIVINGAPARTMENTS_MEDI","NONLIVINGAREA_MEDI","TOTALAREA_MODE")]
#splitting the train set for wrapper function
library(dplyr)
pso_data <- significant_data
runPSO_train_index <- sample(1:nrow(pso_data), nrow(pso_data)*0.7)
runPSO_traindata <- pso_data[runPSO_train_index, ]
runPSO_testdata <- pso_data[-runPSO_train_index, ]
nrow(runPSO_testdata)
nrow(runPSO_traindata)

#---------------------------END : DATA CLEANING--------------------------------------

#-------------------------- STATISTICAL SIGNIFICANCE CHECK : Start-----------------------------------------

chisq_contract_type <- chisq.test(data$TARGET,data$NAME_CONTRACT_TYPE)
chisq_contract_type
#p-value = 2.745e-05 < 0.05

chisq_result <- chisq.test(data$TARGET,data$CODE_GENDER)
chisq_result
#p-value : 0.002484

chisq.test(data$TARGET,data$FLAG_OWN_REALTY)
#p-value = 0.7898

#Changed from N , Y to 0,1 to avoid removal of Ns
dataframe$FLAG_OWN_CAR <- as.numeric(as.factor(dataframe$FLAG_OWN_CAR))
dataframe$FLAG_DOCUMENT_4 <- as.numeric(as.factor(dataframe$FLAG_DOCUMENT_4))
chisq.test(dataframe$TARGET,dataframe$FLAG_OWN_CAR) #significant
chisq.test(dataframe$TARGET,dataframe$FLAG_DOCUMENT_2)
chisq.test(dataframe$TARGET,dataframe$FLAG_DOCUMENT_3) #significant
chisq.test(dataframe$TARGET,dataframe$FLAG_DOCUMENT_4)


#p-value < 2.2e-16
t.test(data$TARGET,data$CNT_CHILDREN)
t.test(data$TARGET,data$AMT_INCOME_TOTAL)
t.test(data$TARGET,data$AMT_CREDIT)
t.test(data$TARGET,data$AMT_ANNUITY)
t.test(data$TARGET,data$AMT_GOODS_PRICE)

chisq.test(data$TARGET,data$NAME_TYPE_SUITE)
#p-value = 0.6658

chisq.test(data$TARGET,data$NAME_INCOME_TYPE)
#p-value = 0.03376

chisq.test(data$TARGET,data$NAME_EDUCATION_TYPE)
#p-value = 6.974e-06

chisq.test(data$TARGET,data$NAME_FAMILY_STATUS)
#p-value = 0.004563

chisq.test(data$TARGET,data$NAME_HOUSING_TYPE)
#p-value = 0.002434

#p-value < 2.2e-16
t.test(data$TARGET,data$REGION_POPULATION_RELATIVE)
t.test(data$TARGET,data$DAYS_BIRTH)
t.test(data$TARGET,data$DAYS_EMPLOYED)
t.test(data$TARGET,data$DAYS_REGISTRATION)
t.test(data$TARGET,data$DAYS_ID_PUBLISH)
t.test(data$TARGET,data$OWN_CAR_AGE)
t.test(data$TARGET,data$FLAG_MOBIL)


chisq.test(data$TARGET,data$FLAG_WORK_PHONE)
#p-value = 0.2066

chisq.test(data$TARGET,data$FLAG_EMP_PHONE)
#p-value = 1

chisq.test(data$TARGET,data$FLAG_WORK_PHONE)
#p-value = 0.2066

chisq.test(data$TARGET,data$FLAG_CONT_MOBILE)
#p-value = 0.9033

chisq.test(data$TARGET,data$FLAG_PHONE)
#p-value = 0.1664

chisq.test(data$TARGET,data$FLAG_EMAIL)
#p-value = 0.5504

chisq.test(data$TARGET,data$OCCUPATION_TYPE)
#p-value = 0.0002315

#p-value < 2.2e-16
t.test(dataframe$TARGET,dataframe$CNT_FAM_MEMBERS)
chisq.test(dataframe$TARGET,dataframe$REGION_RATING_CLIENT)
chisq.test(dataframe$TARGET,dataframe$REGION_RATING_CLIENT_W_CITY)
t.test(dataframe$TARGET,dataframe$HOUR_APPR_PROCESS_START)

chisq.test(dataframe$TARGET,dataframe$REG_REGION_NOT_LIVE_REGION)
#p-value = 0.002177

chisq.test(dataframe$TARGET,dataframe$REG_REGION_NOT_WORK_REGION)
#p-value = 0.0001258

chisq.test(dataframe$TARGET,dataframe$LIVE_REGION_NOT_WORK_REGION)
#p-value = 0.1219

#p-value < 2.2e-16
chisq.test(dataframe$TARGET,dataframe$REG_CITY_NOT_LIVE_CITY)
chisq.test(dataframe$TARGET,dataframe$REG_CITY_NOT_WORK_CITY)
chisq.test(dataframe$TARGET,dataframe$LIVE_CITY_NOT_WORK_CITY)

chisq.test(data_FS$Class,data_FS$ORGANIZATION_TYPE)
#p-value = 0.09436

chisq.test(data$TARGET,data$WEEKDAY_APPR_PROCESS_START)
#p-value = 0.8217

#p-value < 2.2e-16
t.test(data_FS$Class,data_FS$EXT_SOURCE_1)
t.test(data_FS$Class,data_FS$EXT_SOURCE_2)
t.test(data_FS$Class,data_FS$EXT_SOURCE_3)
t.test(data_FS$Class,data_FS$APARTMENTS_AVG)
t.test(data_FS$Class,data_FS$BASEMENTAREA_AVG)
t.test(data_FS$Class,data_FS$YEARS_BEGINEXPLUATATION_AVG)
t.test(data_FS$Class,data_FS$YEARS_BUILD_AVG)
t.test(data_FS$Class,data_FS$COMMONAREA_AVG)
t.test(data_FS$Class,data_FS$FLOORSMAX_AVG)
t.test(data_FS$Class,data_FS$FLOORSMIN_AVG)
t.test(data_FS$Class,data_FS$LANDAREA_AVG)
t.test(data_FS$Class,data_FS$ELEVATORS_AVG)
t.test(data_FS$Class,data_FS$ENTRANCES_AVG)
t.test(data_FS$Class,data_FS$LIVINGAPARTMENTS_AVG)
t.test(data_FS$Class,data_FS$NONLIVINGAPARTMENTS_AVG)
t.test(data_FS$Class,data_FS$NONLIVINGAREA_AVG)
t.test(data_FS$Class,data_FS$YEARS_BEGINEXPLUATATION_MODE)
t.test(data_FS$Class,data_FS$YEARS_BUILD_MODE)
t.test(data_FS$Class,data_FS$COMMONAREA_MODE)
t.test(data_FS$Class,data_FS$ELEVATORS_MODE)
t.test(data_FS$Class,data_FS$FLOORSMAX_MODE)
t.test(data_FS$Class,data_FS$FLOORSMIN_MODE)
t.test(data_FS$Class,data_FS$LANDAREA_MODE)
t.test(data_FS$Class,data_FS$LIVINGAPARTMENTS_MODE)
t.test(data_FS$Class,data_FS$LIVINGAREA_MODE)
t.test(data_FS$Class,data_FS$NONLIVINGAPARTMENTS_MODE)
t.test(data_FS$Class,data_FS$NONLIVINGAREA_MODE)
t.test(data_FS$Class,data_FS$APARTMENTS_MEDI)
t.test(data_FS$Class,data_FS$BASEMENTAREA_MEDI)
t.test(data_FS$Class,data_FS$YEARS_BEGINEXPLUATATION_MEDI)
t.test(data_FS$Class,data_FS$YEARS_BUILD_MEDI)
t.test(data_FS$Class,data_FS$COMMONAREA_MEDI)
t.test(data_FS$Class,data_FS$ELEVATORS_MEDI)
t.test(data_FS$Class,data_FS$ENTRANCES_MEDI)
t.test(data_FS$Class,data_FS$FLOORSMAX_MEDI)
t.test(data_FS$Class,data_FS$FLOORSMIN_MEDI)
t.test(data_FS$Class,data_FS$LANDAREA_MEDI)
t.test(data_FS$Class,data_FS$LIVINGAPARTMENTS_MEDI)
t.test(data_FS$Class,data_FS$LIVINGAREA_MEDI)
t.test(data_FS$Class,data_FS$NONLIVINGAPARTMENTS_MEDI)
t.test(data_FS$Class,data_FS$NONLIVINGAREA_MEDI)
t.test(data_FS$Class,data_FS$BASEMENTAREA_MODE)


chisq.test(data_FS$Class,data_FS$FONDKAPREMONT_MODE)
#p-value = 0.188

chisq.test(data_FS$Class,data_FS$HOUSETYPE_MODE)
# p-value = 0.1412

t.test(data_FS$Class,data_FS$TOTALAREA_MODE)
#p-value < 2.2e-16

chisq.test(data_FS$Class,data_FS$WALLSMATERIAL_MODE)
# p-value = 0.1144

chisq.test(data_FS$Class,data_FS$EMERGENCYSTATE_MODE)
#p-value = 0.3179

t.test(data_FS$Class,data_FS$OBS_30_CNT_SOCIAL_CIRCLE)
#p-value p-value = 0.3807

chisq.test(data_FS$Class,data_FS$DEF_30_CNT_SOCIAL_CIRCLE)
#p-value = 0.1022

chisq.test(data_FS$Class,data_FS$DEF_60_CNT_SOCIAL_CIRCLE)
# p-value = 0.08505

chisq.test(data_FS$Class,data_FS$OBS_60_CNT_SOCIAL_CIRCLE)
#p-value = 0.339

chisq.test(data_FS$Class,data_FS$DAYS_LAST_PHONE_CHANGE)
#p-value = 0.9046

#INSIGNIFICANT
chisq.test(data_FS$Class,data_FS$AMT_REQ_CREDIT_BUREAU_MON)
chisq.test(data_FS$Class,data_FS$AMT_REQ_CREDIT_BUREAU_DAY)
chisq.test(data_FS$Class,data_FS$AMT_REQ_CREDIT_BUREAU_HOUR)
chisq.test(data_FS$Class,data_FS$AMT_REQ_CREDIT_BUREAU_WEEK)
chisq.test(data_FS$Class,data_FS$AMT_REQ_CREDIT_BUREAU_QRT)
chisq.test(data_FS$Class,data_FS$AMT_REQ_CREDIT_BUREAU_YEAR)
t.test(dataframe$Class,dataframe$FLAG_DOCUMENT_2)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_5)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_6)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_8)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_10)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_12)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_13)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_14)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_15)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_16)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_17)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_18)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_19)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_20)
chisq.test(data_FS$Class,data_FS$FLAG_DOCUMENT_21)
#--------------------------STATISTICAL SIGNIFICANCE CHECK : End-----------------------------------------
