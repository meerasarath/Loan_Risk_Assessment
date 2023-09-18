#******************************************************************************************
#------------------------------- EXPLORATORY DATA ANALYSIS -------------------------------------
##******************************************************************************************

#edaData <- read.csv("/Users/meeramohanm/BCU/Dissertation/home-credit-default-risk/application_train.csv")
#Use cleaned data from DataCleaningAndPreProcessing.R.
nrow(edaData)


#1. APPLICATIONS REJECTED AND APPROVED IN THE GIVEN DATASET
rejection_data <- subset(edaData, edaData$TARGET == 1)
approved_data <- subset(edaData, edaData$TARGET == 0)
rejection_percentage <- ((nrow(rejection_data) - 1)/(nrow(edaData) - 1)) * 100
approved_percentage <- ((nrow(approved_data) - 1)/(nrow(edaData) - 1)) * 100
df <- data.frame(application_status = c("Rejected","Approved"), value = c(rejection_percentage,approved_percentage))
df

library(ggplot2)
bp<- ggplot(df, aes(x="", y=value, fill=application_status))+
  geom_bar(width = 1, stat = "identity") + geom_text(aes(label = c("6.104%","93.884%")),position = position_stack(vjust = 0.5))
bp
pie <- bp + coord_polar("y", start=0) 
pie 

#2.Cash/Revolving loans and loans approval
approved_data$NAME_CONTRACT_TYPE <- as.factor(approved_data$NAME_CONTRACT_TYPE)
cash_loans <- subset(approved_data,approved_data$NAME_CONTRACT_TYPE == "Cash loans")
revolving_loans <- subset(approved_data,approved_data$NAME_CONTRACT_TYPE == "Revolving loans")
appr_cash_loans_percentage <- nrow(cash_loans)/nrow(approved_data) * 100
appr_revolving_loans_percentage <- ((nrow(revolving_loans) - 1)/(nrow(approved_data))) * 100

rejection_data$NAME_CONTRACT_TYPE <- as.factor(rejection_data$NAME_CONTRACT_TYPE)
cash_loans_rej <- subset(rejection_data,rejection_data$NAME_CONTRACT_TYPE == "Cash loans")
revolving_loans_rej <- subset(rejection_data,rejection_data$NAME_CONTRACT_TYPE == "Revolving loans")
rej_cash_loans_percentage <- ((nrow(cash_loans_rej) -1)/(nrow(rejection_data))) * 100
rej_revolving_loans_percentage <- ((nrow(revolving_loans_rej) - 1)/(nrow(rejection_data))) * 100

df <- data.frame(loan_status = c("Approved","Approved","Rejected","Rejected"),loan_type = c("Cash","Revolving","Cash","Revolving"),value = c(appr_cash_loans_percentage,appr_revolving_loans_percentage,rej_cash_loans_percentage,rej_revolving_loans_percentage))
bp <- ggplot(df,aes(x=loan_status, y=value, fill=loan_type))+
  geom_bar(position='stack', stat = "identity") +
  scale_fill_manual(values=c('#8225BE', '#D4C443'))
#geom_text(aes(label = c("90.2%","9.8%")),position = position_stack(vjust = 0.5))
bp

#3. Gender and Loan Approval
approved_data$CODE_GENDER <- as.factor(approved_data$CODE_GENDER)
female_count <- subset(approved_data,approved_data$CODE_GENDER == "F")
male_count <- subset(approved_data,approved_data$CODE_GENDER == "M")
female_percent <- ((nrow(female_count) -1)/(nrow(approved_data))) * 100
male_percent <- ((nrow(male_count) -1)/(nrow(approved_data))) * 100

rejection_data$CODE_GENDER <- as.factor(rejection_data$CODE_GENDER)
rej_female_count <- subset(rejection_data,rejection_data$CODE_GENDER == "F")
rej_male_count <- subset(rejection_data,rejection_data$CODE_GENDER == "M")
rej_female_percent <- ((nrow(rej_female_count) -1)/(nrow(rejection_data))) * 100
rej_male_percent <- ((nrow(rej_male_count) -1)/(nrow(rejection_data))) * 100

df <- data.frame(loan_status = c("Approved","Approved","Rejected","Rejected"),gender = c("Male","Female","Male","Female"),value = c(male_percent,female_percent,rej_male_percent,rej_female_percent))
bp <- ggplot(df,aes(x=loan_status, y=value, fill=gender))+
  geom_bar(position='dodge', stat = "identity") 
#geom_text(aes(label = c("90.2%","9.8%")),position = position_stack(vjust = 0.5))
bp


#4.Own car and Laon Approval
approved_data$FLAG_OWN_CAR <- as.factor(approved_data$FLAG_OWN_CAR)
app_car_y_count <- subset(approved_data,approved_data$FLAG_OWN_CAR == "Y")
rej_car_y_count <- subset(rejection_data,rejection_data$FLAG_OWN_CAR == "Y")
app_car_y_percent <- ((nrow(app_car_y_count) -1)/(nrow(approved_data))) * 100
rej_car_y_percent <- ((nrow(rej_car_y_count) -1)/(nrow(rejection_data))) * 100

df <- data.frame(car_status = c("Payable","Not Payable"), value = c(app_car_y_percent,rej_car_y_percent))
bp<- ggplot(df, aes(x="", y=value, fill=car_status))+
  geom_col(position="dodge") + 
  scale_fill_manual(values=c('light blue', 'grey')) +
  geom_text(aes(label=c(99.98,99.81)), vjust= 1.5,position = position_dodge(.9))+
  xlab("Loan Repay Status")+
  ylab("Percentage of Car Owners")
  
bp

#5.Own Realty 
realty_y_count <- subset(approved_data,approved_data$FLAG_OWN_REALTY == "Y")
realty_n_count <- subset(approved_data,approved_data$FLAG_OWN_REALTY == "N")
realty_y_percent <- ((nrow(realty_y_count) -1)/(nrow(approved_data))) * 100
realty_n_percent <- ((nrow(realty_n_count) -1)/(nrow(approved_data))) * 100

rej_realty_y_count <- subset(rejection_data,rejection_data$FLAG_OWN_REALTY == "Y")
rej_realty_n_count <- subset(rejection_data,rejection_data$FLAG_OWN_REALTY == "N")
rej_realty_y_percent <- ((nrow(rej_realty_y_count) -1)/(nrow(rejection_data))) * 100
rej_realty_n_percent <- ((nrow(rej_realty_n_count) -1)/(nrow(rejection_data))) * 100

df <- data.frame(loan_status = c("Approved","Approved","Rejected","Rejected"),realty = c("Owns Realty","No Realty","Owns Realty","No Realty"),value = c(realty_y_percent,realty_n_percent,rej_realty_y_percent,rej_realty_n_percent))
bp <- ggplot(df,aes(x=loan_status, y=value, fill=realty))+
  geom_bar(position='dodge', stat = "identity") +
  scale_fill_manual(values=c('dark green', 'light green'))+
  ylab("Realty Ownership Status") +
  xlab("Loan Application Status")
bp


#6. Count children
app_cnt_child <- as.data.frame(table(approved_data$CNT_CHILDREN))
rej_cnt_child <- as.data.frame(table(rejection_data$CNT_CHILDREN))
status <- rep("approved",6)
app_cnt_child_data <- data.frame(status,app_cnt_child)
status <- rep("rejected",4)
rej_cnt_child_data <- data.frame(status,rej_cnt_child)
cnt_child_data <- rbind(app_cnt_child_data,rej_cnt_child_data)
colnames(app_cnt_child_data)[2] <- "Children"
colnames(app_cnt_child_data)[3] <- "Count"

ggplot(data = app_cnt_child_data,aes(x=Children,y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  theme_minimal() +
  ggtitle("Loan Payable")

colnames(rej_cnt_child_data)[2] <- "Children"
colnames(rej_cnt_child_data)[3] <- "Count"
ggplot(data = rej_cnt_child_data,aes(x=Children,y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  theme_minimal()+
  ggtitle("Loan Not Payable")

#7. NAME_TYPE_SUITE
app_name_type_suite <- as.data.frame(table(approved_data$NAME_TYPE_SUITE))
rej_name_type_suite <- as.data.frame(table(rejection_data$NAME_TYPE_SUITE))
app_name_type_suite <- app_name_type_suite[-1,]
rej_name_type_suite <- rej_name_type_suite[-1,]
library(dplyr)
library(forcats)

# Reorder following the value of another column:
app_name_type_suite %>%
  mutate(Var1 = fct_reorder(Var1, Freq)) %>%
  ggplot( aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  ylab("Count") +
  xlab("Accompanied by")+
  ggtitle("Loan Payable")
  theme_bw()

rej_name_type_suite %>%
    mutate(Var1 = fct_reorder(Var1, Freq)) %>%
    ggplot( aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
    coord_flip() +
  ylab("Count") +
  xlab("Accompanied by")+
    ggtitle("Loan Not Payable")
  theme_bw()


#8. NAME_INOCME_TYPE
app_income_type<- as.data.frame(table(approved_data$NAME_INCOME_TYPE))
rej_income_type <- as.data.frame(table(rejection_data$NAME_INCOME_TYPE))

library(ggrepel)
app_income_type %>%
    arrange(Freq) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
    mutate(name=factor(Var1, levels=Var1)) %>%   # This trick update the factor levels
    ggplot( aes(x=name, y=Freq)) +
    geom_segment( aes(xend=name, yend=0)) +
    geom_point( size=4, color="orange") +
  geom_label_repel(aes(label = Freq, size = NULL),
                   arrow = arrow(length = unit(0.03, "npc"), 
                                 type = "closed", ends = "last"),
                   nudge_y = 3,
                   segment.size  = 0.3
  )+
    ggtitle("Income Type -- Loan Payable")+
    coord_flip() +
    theme_bw() +
    xlab("")

rej_income_type%>%
  arrange(Freq) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name=factor(Var1, levels=Var1)) %>%   # This trick update the factor levels
  ggplot( aes(x=name, y=Freq)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=4, color="orange") +
  geom_label_repel(aes(label = Freq, size = NULL),
                   arrow = arrow(length = unit(0.03, "npc"), 
                                 type = "closed", ends = "last"),
                   nudge_y = 3,
                   segment.size  = 0.3
  )+
  ggtitle("Income Type -- Loan Not Payable")+
  coord_flip() +
  theme_bw() +
  xlab("")

#9.NAME_EDUCATION_TYPE
app_education_type<- as.data.frame(table(approved_data$NAME_EDUCATION_TYPE))
rej_education_type <- as.data.frame(table(rejection_data$NAME_EDUCATION_TYPE))

ggplot(app_education_type, aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity", width=0.2) +
  geom_label_repel(aes(label = Freq, size = NULL),
                   arrow = arrow(length = unit(0.03, "npc"), 
                                 type = "closed", ends = "last"),
                   nudge_y = 3,
                   segment.size  = 0.3
  )+
  ggtitle("Education Type -- Loan Payable")+
  coord_flip() +
  xlab("Education Type")+
  ylab("Count")

ggplot(rej_education_type, aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity", width=0.2) +
  geom_label_repel(aes(label = Freq, size = NULL),
                   arrow = arrow(length = unit(0.03, "npc"), 
                                 type = "closed", ends = "last"),
                   nudge_y = 3,
                   segment.size  = 0.3
  )+
  ggtitle("Education Type -- Loan Not Payable")+
  coord_flip() +
  xlab("Education Type")+
  ylab("Count")

#10. NAME_FAMILY_STATUS
app_family_type<- as.data.frame(table(approved_data$NAME_FAMILY_STATUS))
rej_family_type <- as.data.frame(table(rejection_data$NAME_FAMILY_STATUS))

ggplot(app_family_type, aes(x=Var1, y=Freq)) +
  geom_segment( aes(x=Var1, xend=Var1, y=0, yend=Freq) , size=1, color="blue", linetype="dotdash" ) +
  geom_point() +
  ggtitle("Family Status -- Loan Payable")+
  xlab("Family status")+
  ylab("Count")

ggplot(rej_family_type, aes(x=Var1, y=Freq)) +
  geom_segment( aes(x=Var1, xend=Var1, y=0, yend=Freq) , size=1, color="blue", linetype="dotdash" ) +
  geom_point() +
  ggtitle("Family Status -- Loan Not Payable")+
  xlab("Family status")+
  ylab("Count")


#11.NAME_HOUSING_TYPE
app_housing_type<- as.data.frame(table(approved_data$NAME_HOUSING_TYPE))
rej_housing_type <- as.data.frame(table(rejection_data$NAME_HOUSING_TYPE))

app_housing_type$fraction = app_housing_type$Freq / sum(app_housing_type$Freq)
app_housing_type$ymax = cumsum(app_housing_type$fraction)
app_housing_type$ymin = c(0, head(app_housing_type$ymax, n=-1))
ggplot(app_housing_type, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  ggtitle("Housing Type -- Loan Payable")

rej_housing_type$fraction = rej_housing_type$Freq / sum(rej_housing_type$Freq)
rej_housing_type$ymax = cumsum(rej_housing_type$fraction)
rej_housing_type$ymin = c(0, head(rej_housing_type$ymax, n=-1))
ggplot(rej_housing_type, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  ggtitle("Housing Type -- Loan Not Payable")

#12.OCCUPATION_TYPE
app_occup_type<- as.data.frame(table(approved_data$OCCUPATION_TYPE))
rej_occup_type <- as.data.frame(table(rejection_data$OCCUPATION_TYPE))

app_occup_type <- app_occup_type[-1,]
rej_occup_type <- rej_occup_type[-1,]

library(treemap)
treemap(app_occup_type,index = "Var1",vSize = "Freq",type = "index",title = "Occupation Type -- Loan Payable")
treemap(rej_occup_type,index = "Var1",vSize = "Freq",type = "index",title = "Occupation Type -- Loan Not Payable")

#13. ORGANIZATION_TYPE 
app_org_type<- as.data.frame(table(approved_data$ORGANIZATION_TYPE))
rej_org_type <- as.data.frame(table(rejection_data$ORGANIZATION_TYPE))
app_org_type$id <- c(1:55)
rej_org_type$id <- c(1:43)

label_data <- app_org_type
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

p <- ggplot(app_org_type, aes(x=as.factor(id), y=Freq)) +       
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  coord_polar(start = 0) +
  geom_text(data=label_data, aes(x=id, y=Freq+10, label=Var1, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
p




