##PROJECT 

data <-read.csv(file="C:\\Users\\sasikumarchennova\\Documents\\Data Science\\Excel\\project\\Employee_Attrition_with_missing.csv",header=TRUE,sep=",")

View(data)

##Data Cleaning
#Emplyoee ID 
EM_ID <-data$employee_id
EM_ID
#AGE---
data$Age
x <- data01$Age
#mn <-mean(x,na.rm = TRUE)
mean(x,na.rm = TRUE)
Cl_Age <- round(ifelse(is.na(x),mean(x,na.rm = TRUE),x),0)
Cl_Age #AGE CLEANED

#BUSINESS TRAVEL---
x <-data$BusinessTravel
#which.max(table(x))
Cl_BT <- ifelse(is.na(x),names(which.max(table(x))),x)
Cl_BT 

#DAILY RATE
x <- data$DailyRate
mean(x,na.rm = TRUE)
Cl_DR <- round(ifelse(is.na(x),mean(x,na.rm = TRUE),x),0)
Cl_DR

#DEPARTMENT
x <- data$Department
Cl_Dpt <- ifelse(is.na(x),names(which.max(table(x))),x)
Cl_Dpt 

#DISTANCE FROM HOME 
x<-data$DistanceFromHome
Cl_DFH <- round(ifelse(is.na(x),mean(x,na.rm = TRUE),x),0)
Cl_DFH

#EDUCATION
x <- data$Education
Cl_Edu <- ifelse(is.na(x),names(which.max(table(x))),x)
Cl_EDU <- sub('.','',Cl_Edu)
Cl_EDU

#EDUCATION FIELD
x <- data$EducationField
cl_EF <-ifelse(is.na(x),names(which.max(table(x))),x)
cl_EF 

#ENVIRONMENT SITUATION
x <- data$EnvironmentSatisfaction
Cl_E <-ifelse(is.na(x),names(which.max(table(x))),x)
CL_ES <-sub('.','',Cl_E)
CL_ES

#GENDER
x<- data$Gender
Cl_G <-ifelse(is.na(x),names(which.max(table(x))),x)
Cl_G

#HourlyRate
x<-data$HourlyRate
Cl_HR <- round(ifelse(is.na(x),mean(x,na.rm = TRUE),x),0)
Cl_HR 

#JobInvolvement
x <- data$JobInvolvement
Cl_J <-ifelse(is.na(x),names(which.max(table(x))),x)
CL_JI <-sub('.','',Cl_J)
CL_JI

#JobLevel
x<-data$JobLevel
Cl_JL <- round(ifelse(is.na(x),mean(x,na.rm = TRUE),x),0)
Cl_JL

#JobRole
x<-data$JobRole
Cl_R <-ifelse(is.na(x),names(which.max(table(x))),x)
Cl_R 

#JobSatisification
x <- data$JobSatisfaction
Cl_S <-ifelse(is.na(x),names(which.max(table(x))),x)
CL_St <-sub('.','',Cl_S)
CL_St

#MaritialStatus
x<-data$MaritalStatus
Cl_MS <-ifelse(is.na(x),names(which.max(table(x))),x)
Cl_MS  

#MonthlyIncome
x<-data$MonthlyIncome
Cl_MI <- round(ifelse(is.na(x),mean(x,na.rm = TRUE),x),0)
Cl_MI

#MonthlyRate
x<-data$MonthlyRate
Cl_MR <- round(ifelse(is.na(x),mean(x,na.rm = TRUE),x),0)
Cl_MR  

#NumCompaniesWorked
x<-data$NumCompaniesWorked
Cl_NCW <- round(ifelse(is.na(x),mean(x,na.rm = TRUE),x),0)
Cl_NCW

#OverTime
x<-data$OverTime
Cl_OT <-ifelse(is.na(x),names(which.max(table(x))),x)
Cl_OT

#PercentSalaryHike
x<-data$PercentSalaryHike
Cl_PSH <- round(ifelse(is.na(x),mean(x,na.rm = TRUE),x),0)
Cl_PSH

#Performance Rating
x <- data$PerformanceRating
Cl_P <-ifelse(is.na(x),names(which.max(table(x))),x)
CL_PR <-sub('.','',Cl_P)
CL_PR

#Relationship Satisfaction
x <- data$RelationshipSatisfaction
Cl_RS <-ifelse(is.na(x),names(which.max(table(x))),x)
CL_RST <-sub('.','',Cl_RS)
CL_RST

#Training Less YEar 
x<-data$TrainingTimesLastYear
Cl_TLY <- round(ifelse(is.na(x),mean(x,na.rm = TRUE),x),0)
Cl_TLY

#WorkLifeBalance 
x <- data$WorkLifeBalance
Cl_WL <-ifelse(is.na(x),names(which.max(table(x))),x)
CL_WLB <-sub('.','',Cl_WL)
CL_WLB

#Attrition
x<-data$Attrition
Cl_AT <-ifelse(is.na(x),names(which.max(table(x))),x)
Cl_AT

Cleaned_Output<- data.frame(EM_ID,Cl_Age,Cl_BT,Cl_DR,Cl_Dpt,Cl_DFH,Cl_EDU,cl_EF,CL_ES,Cl_G,Cl_HR,CL_JI,Cl_JL,Cl_R,CL_St,Cl_MS,Cl_MI,Cl_MR,Cl_NCW,Cl_OT,Cl_PSH,CL_PR,CL_RST,Cl_TLY,CL_WLB,Cl_AT)
names(Cleaned_Output) <- c("Emp","Age","BusinessTravel","DailyRate","Department","DistanceFromHome","Education","EducationField","EnvironmentStatus","Gender","HourlyRate","JobInvolvement","JobLevel","JobRole","JobSatisfication","MartialStatus","MonthlyIncome","MonthlyRate","NoCompaniesWorked","OverTime","PercentSalaryHike","PerformanceRate","RelationshipSatisfication","TrainingTimeLastYear","WorkLIfeBalance","Attrition")
View(Cleaned_Output)

#SavingLocalDevice
write.table(Cleaned_Output,  "C:\\Users\\sasikumarchennova\\Documents\\Data Science\\R\\Project\\project_file.csv", sep=",")

#Get Cleaned Data 

data <-read.csv(file="C:\\Users\\sasikumarchennova\\Documents\\Data Science\\R\\Project\\project_file.csv",header=TRUE,sep=",")

View(data)

#Univariate Table and Charts 

myfun <-function(variable,name)
{
  table1 <- data.frame(table(variable))
  names(table1) <-c(name,"Count")
  table1$Percentage <- paste0(round(table1$Count/sum(table1$Count)*100),"%")
  table1
}

library(ggplot2)
library(plotly)

table1 <- myfun(data$BusinessTravel,"BusinessTravel")
table1

#Bar Diagram

p <- ggplot(table1 ,aes(x=BusinessTravel, y=Count))+
      geom_bar(aes(fill=BusinessTravel),stat = "identity")+
      geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
      ggtitle("Business Travel")
BusinessTravel_bar <-ggplotly(p)
BusinessTravel_bar

##>Pie Chart 

lbls <- table1[,1]
slices <- table1[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'Business Travel')
fig


table2 <-myfun(data$Department,"Department")

#Bar Diagram
p <- ggplot(table2 ,aes(x=Department, y=Count))+
  geom_bar(aes(fill=Department),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("Department")
Department_bar <-ggplotly(p)
Department_bar

##>Pie Chart 

lbls <- table2[,1]
slices <- table2[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'Department')
fig


table3 <-myfun(data$Education,"Education")
#Bar Diagram

p <- ggplot(table3 ,aes(x=Education, y=Count))+
  geom_bar(aes(fill=Education),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("Education")
Education_bar <-ggplotly(p)
Education_bar

##>Pie Chart 

lbls <- table3[,1]
slices <- table3[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'Education')
fig

table4 <-myfun(data$EducationField,"EducationField")

#Bar Diagram

p <- ggplot(table4 ,aes(x=EducationField, y=Count))+
  geom_bar(aes(fill=EducationField),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("EducationField")
EducationField_bar <-ggplotly(p)
EducationField_bar

##>Pie Chart 

lbls <- table4[,1]
slices <- table4[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'EducationField')
fig


table5 <-myfun(data$EnvironmentStatus,"EnvStatus")

#Bar Diagram

p <- ggplot(table5 ,aes(x=EnvStatus, y=Count))+
  geom_bar(aes(fill=EnvStatus),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("EnvStatus")
EnvStatus_bar <-ggplotly(p)
EnvStatus_bar

##>Pie Chart 

lbls <- table5[,1]
slices <- table5[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'EnvStatus')
fig

table6 <-myfun(data$Gender,"Gender")

#Bar Diagram

p <- ggplot(table6 ,aes(x=Gender, y=Count))+
  geom_bar(aes(fill=Gender),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("Gender")
Gender_bar <-ggplotly(p)
Gender_bar

##>Pie Chart 

lbls <- table6[,1]
slices <- table6[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'Gender')
fig


table7 <-myfun(data$JobInvolvement,"JobInv")

#Bar Diagram

p <- ggplot(table7 ,aes(x=Gender, y=Count))+
  geom_bar(aes(fill=Gender),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("Gender")
Gender_bar <-ggplotly(p)
Gender_bar

##>Pie Chart 

lbls <- table7[,1]
slices <- table7[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'JobInv')
fig


table8 <-myfun(data$JobRole,"Jobrole")

#Bar Diagram

p <- ggplot(table8 ,aes(x=Jobrole, y=Count))+
  geom_bar(aes(fill=Jobrole),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("Jobrole")
Jobrole_bar <-ggplotly(p)
Jobrole_bar

##>Pie Chart 

lbls <- table8[,1]
slices <- table8[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'Jobrole')
fig

table9 <-myfun(data$JobSatisfication,"JobSatifcation")

#Bar Diagram

p <- ggplot(table9 ,aes(x=JobSatifcation, y=Count))+
  geom_bar(aes(fill=JobSatifcation),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("JobSatifcation")
JobSatifcation_bar <-ggplotly(p)
JobSatifcation_bar

##>Pie Chart 

lbls <- table9[,1]
slices <- table9[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'JobSatifcation')
fig

table10 <-myfun(data$MartialStatus,"MartialStatus")

#Bar Diagram

p <- ggplot(table10 ,aes(x=MartialStatus, y=Count))+
  geom_bar(aes(fill=MartialStatus),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("MartialStatus")
MartialStatus_bar <-ggplotly(p)
MartialStatus_bar

##>Pie Chart 

lbls <- table10[,1]
slices <- table10[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'MartialStatus')
fig

table11 <-myfun(data$OverTime,"OverTime")

#Bar Diagram

p <- ggplot(table11 ,aes(x=OverTime, y=Count))+
  geom_bar(aes(fill=OverTime),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("OverTime")
OverTime_bar <-ggplotly(p)
OverTime_bar

##>Pie Chart 

lbls <- table11[,1]
slices <- table11[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'OverTime')
fig



table12 <-myfun(data$PerformanceRate,"PerformanceRate")

#Bar Diagram

p <- ggplot(table12 ,aes(x=PerformanceRate, y=Count))+
  geom_bar(aes(fill=PerformanceRate),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("PerformanceRate")
PerformanceRate_bar <-ggplotly(p)
PerformanceRate_bar

##>Pie Chart 

lbls <- table12[,1]
slices <- table12[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'PerformanceRate')
fig

table13 <-myfun(data$RelationshipSatisfication,"RelationshipSatisfication")

#Bar Diagram

p <- ggplot(table13 ,aes(x=RelationshipSatisfication, y=Count))+
  geom_bar(aes(fill=RelationshipSatisfication),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("RelationshipSatisfication")
RelationshipSatisfication_bar <-ggplotly(p)
RelationshipSatisfication_bar

##>Pie Chart 

lbls <- table13[,1]
slices <- table13[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'RelationshipSatisfication')
fig

table14 <-myfun(data$WorkLIfeBalance,"worklifebalance")

#Bar Diagram

p <- ggplot(table14 ,aes(x=worklifebalance, y=Count))+
  geom_bar(aes(fill=worklifebalance),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("worklifebalance")
worklifebalance_bar <-ggplotly(p)
worklifebalance_bar

##>Pie Chart 

lbls <- table14[,1]
slices <- table14[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'worklifebalance')
fig

table15 <-myfun(data$Attrition,"Attrition")

#Bar Diagram

p <- ggplot(table15 ,aes(x=Attrition, y=Count))+
  geom_bar(aes(fill=Attrition),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("Attrition")
Attrition_bar <-ggplotly(p)
Attrition_bar

##>Pie Chart 

lbls <- table15[,1]
slices <- table15[,2]
fig <- plot_ly(type='pie', labels=lbls, values=slices, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'Attrition')
fig

library(ggplot2)
library(plotly)

#Age ---

data$age_group <-ifelse(data$Age<=18,1,
                        ifelse(data$Age<=29,2,
                               ifelse(data$Age<=41,3,
                                      ifelse(data$Age<=53,4,
                                             ifelse(data$Age<=60,5,6)))))

data$age_group <-factor(data$age_group,
                        levels = c(1,2,3,4,5,6),
                        labels = c("Age: <18","Age: 19-29","Age: 30-41","Age: 42-53","Age: 54-60","Age: 60+"))
table16<- myfun(data$age_group,"Age")
table16


p <- ggplot(table16 ,aes(x=Age, y=Count))+
  geom_bar(aes(fill=Age),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("Age")
age_bar_chat <-ggplotly(p)
age_bar_chat



#Daily Rate---

data$daily_ra <-ifelse(data$DailyRate<=200,1,
                       ifelse(data$DailyRate<=499,2,
                              ifelse(data$DailyRate<=897,3,
                                     ifelse(data$DailyRate<=1296,4,
                                            ifelse(data$DailyRate<=1697,5,6)))))

data$daily_ra <-factor(data$daily_ra,
                       levels = c(1,2,3,4,5,6),
                       labels = c("Daily Rate: <200","Daily Rate: 201-499","Daily Rate: 500-897","Daily Rate: 898-1296","Daily Rate: 1297-1693","Daily Rate: >1694"))
table17<- myfun(data$daily_ra,"DailyRate")
table17

p <- ggplot(table17 ,aes(x=DailyRate, y=Count))+
  geom_bar(aes(fill=DailyRate),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("DailyRate")
DailyRate_bar <-ggplotly(p)
DailyRate_bar


#Distance From Home ----

data$distance_f_h <-ifelse(data$DistanceFromHome<=5,1,
                           ifelse(data$DistanceFromHome<=10,2,
                                  ifelse(data$DistanceFromHome<=15,3,
                                         ifelse(data$DistanceFromHome<=20,4,
                                                ifelse(data$DistanceFromHome<=25,5,6)))))

data$distance_f_h <-factor(data$distance_f_h,
                           levels = c(1,2,3,4,5,6),
                           labels = c("<5 KM","6 -10 KM","11-15 KM","16-20 KM","21-25 KM",">25 KM"))
table18<- myfun(data$distance_f_h,"DistanceFromHome")
table18

p <- ggplot(table18 ,aes(x=DistanceFromHome, y=Count))+
  geom_bar(aes(fill=DistanceFromHome),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("DistanceFromHome")
DistanceFromHome_bar <-ggplotly(p)
DistanceFromHome_bar

##Hourly Rate ----

data$hourly_rate <-ifelse(data$HourlyRate<=49,1,
                           ifelse(data$HourlyRate<=69,2,
                                  ifelse(data$HourlyRate<=89,3,
                                         ifelse(data$HourlyRate<=109,4,5))))

data$hourly_rate <-factor(data$hourly_rate,
                           levels = c(1,2,3,4,5),
                           labels = c("<49","50-69 ","70-89 ","90-109 ",">110 "))

table18<- myfun(data$hourly_rate,"HourlyRate")


p <- ggplot(table18 ,aes(x=HourlyRate, y=Count))+
  geom_bar(aes(fill=HourlyRate),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("HourlyRate")
HourlyRate_bar <-ggplotly(p)
HourlyRate_bar

#Job Level

data$job_levl <-ifelse(data$JobLevel<=1,1,
                          ifelse(data$JobLevel<=2,2,
                                 ifelse(data$JobLevel<=3,3,
                                        ifelse(data$JobLevel<=4,4,
                                               ifelse(data$JobLevel<=5,5,6)))))

data$job_levl <-factor(data$job_levl,
                          levels = c(1,2,3,4,5,6),
                          labels = c("<=1","1-2","2-3","3-4","4-5",">5"))

table19<- myfun(data$job_levl,"JobLevel")

p <- ggplot(table19 ,aes(x=JobLevel, y=Count))+
  geom_bar(aes(fill=JobLevel),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("JobLevel")
JobLevel_bar <-ggplotly(p)
JobLevel_bar

#Monthly Income 

data$month_inc <-ifelse(data$MonthlyIncome<=6008,1,
                       ifelse(data$MonthlyIncome<=11008,2,
                              ifelse(data$MonthlyIncome<=16008,3,
                                     ifelse(data$MonthlyIncome<=21008,4,5))))

data$month_inc <-factor(data$month_inc,
                       levels = c(1,2,3,4,5),
                       labels = c("1009-6008","6009-11008","11008-16008","16009-21008",">21009"))

table20<- myfun(data$month_inc,"MonthlyIncome")

p <- ggplot(table20 ,aes(x=MonthlyIncome, y=Count))+
  geom_bar(aes(fill=MonthlyIncome),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("MonthlyIncome")
MonthlyIncome_bar <-ggplotly(p)
MonthlyIncome_bar

#Monthly Rate

data$month_rat <-ifelse(data$MonthlyRate<=10093,1,
                        ifelse(data$MonthlyRate<=18093,2,
                               ifelse(data$MonthlyRate<=26093,3,
                                      ifelse(data$MonthlyRate<=34093,4,
                                             ifelse(data$MonthlyRate<=58093,5,6)))))

data$month_rat <-factor(data$month_rat,
                        levels = c(1,2,3,4,5),
                        labels = c("2094-10093","10094-18093","18094-26093","26094-34093",">34093"))

table21<- myfun(data$month_rat,"MonthlyRate")

p <- ggplot(table21 ,aes(x=MonthlyRate, y=Count))+
  geom_bar(aes(fill=MonthlyRate),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("MonthlyIncome")
MonthlyRate_bar <-ggplotly(p)
MonthlyRate_bar


#No of Companies Work 


data$company_work <-ifelse(data$NoCompaniesWorked<=1,1,
                        ifelse(data$NoCompaniesWorked<=3,2,
                               ifelse(data$NoCompaniesWorked<=5,3,
                                      ifelse(data$NoCompaniesWorked<=7,4,
                                             ifelse(data$NoCompaniesWorked<=9,5,6)))))

data$company_work <-factor(data$company_work,
                        levels = c(1,2,3,4,5),
                        labels = c("<=1","2-3","4-5","6-7",">8"))

table22<- myfun(data$company_work,"NoOfCompaniesWork")

p <- ggplot(table22 ,aes(x=NoOfCompaniesWork, y=Count))+
  geom_bar(aes(fill=NoOfCompaniesWork),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("NoOfCompaniesWork")
NoOfCompaniesWork_bar <-ggplotly(p)
NoOfCompaniesWork_bar

#Salary Hike 

data$hike <-ifelse(data$PercentSalaryHike<=13,1,
                           ifelse(data$PercentSalaryHike<=16,2,
                                  ifelse(data$PercentSalaryHike<=19,3,
                                         ifelse(data$PercentSalaryHike<=22,4,
                                                ifelse(data$PercentSalaryHike<=23,5,6)))))

data$hike <-factor(data$hike,
                           levels = c(1,2,3,4,5),
                           labels = c("<=13","14-16","17-19","19-22",">23"))

table23<- myfun(data$hike,"SalaryHike")

p <- ggplot(table23 ,aes(x=SalaryHike, y=Count))+
  geom_bar(aes(fill=SalaryHike),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("Percent Salary Hike")
SalaryHike_bar <-ggplotly(p)
SalaryHike_bar

#Training Time Last Year

data$TrainingLastYear <-ifelse(data$TrainingTimeLastYear<=1,1,
                   ifelse(data$TrainingTimeLastYear<=3,2,
                          ifelse(data$TrainingTimeLastYear<=6,3,4)))

data$TrainingLastYear <-factor(data$TrainingLastYear,
                   levels = c(1,2,3),
                   labels = c("<1","2-3","4-6"))

table24<- myfun(data$TrainingLastYear,"TrainingTimeLastYear")

p <- ggplot(table24 ,aes(x=TrainingTimeLastYear, y=Count))+
  geom_bar(aes(fill=TrainingTimeLastYear),stat = "identity")+
  geom_text(aes(label = Percentage, y = Count), size= 3,vjust = -0.5)+
  ggtitle("Training Time Last Year")
TrainingTimeLastYear_bar <-ggplotly(p)
TrainingTimeLastYear_bar

View(table24)

##Bivariate Table and Charts 
table(data$age_group,data$Attrition)
table(data$BusinessTravel,data$Attrition)
table(data$Department,data$Attrition)
table(data$daily_ra,data$Attrition)
table(data$distance_f_h,data$Attrition)
table(data$Education,data$Attrition)
table(data$EducationField,data$Attrition)
table(data$EnvironmentStatus,data$Attrition)
table(data$Gender,data$Attrition)
table(data$hourly_rate,data$Attrition)
table(data$job_levl,data$Attrition)
table(data$JobRole,data$Attrition)
table(data$JobSatisfication,data$Attrition)
table(data$MartialStatus,data$Attrition)
table(data$month_inc,data$Attrition)
table(data$month_rat,data$Attrition)
table(data$company_work,data$Attrition)
table(data$OverTime,data$Attrition)
table(data$hike,data$Attrition)
table(data$PerformanceRate,data$Attrition)
table(data$TrainingLastYear,data$Attrition)
table(data$RelationshipSatisfication,data$Attrition)
table(data$WorkLIfeBalance,data$Attrition)

#Bi variant Table Function 
summary <- function(indv,dv,name1,name2)
{
  t1 <- table(indv,dv)
  t2 <- data.frame(prop.table(t1, 1)*100)
  t2$freq <- paste0(round(t2$Freq,1),"%")
  names(t2) <- c(name1,name2,"Percent","Percentage")
  t2
}

#Work_Life_Balance

table01 <-summary(data$WorkLIfeBalance,data$Attrition,"Work_Life_Balance", "Attrition")
table01

p <- ggplot(table01, aes(x = Work_Life_Balance, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Work Life Balance vs Attrition ")
fig <- ggplotly(p)
fig

#Age_Group

table02 <-summary(data$age_group,data$Attrition,"Age_Group", "Attrition")
table02

p <- ggplot(table02, aes(x = Age_Group, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Age  Vs Attrition")
fig <- ggplotly(p)
fig

#Business Travel 

table03 <-summary(data$BusinessTravel,data$Attrition,"Business_Travel", "Attrition")
table03

p <- ggplot(table03, aes(x = Business_Travel, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Business Travel  Vs Attrition")
fig <- ggplotly(p)
fig

#Daily Rate 

table04 <-summary(data$daily_ra,data$Attrition,"Daily_Rate", "Attrition")
table04

p <- ggplot(table04, aes(x = Daily_Rate, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Daily Rate  Vs Attrition")
fig <- ggplotly(p)
fig

#Department
table05 <-summary(data$Department,data$Attrition,"Department", "Attrition")
table05

p <- ggplot(table05, aes(x = Department, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Department  Vs Attrition")
fig <- ggplotly(p)
fig

#Distance From Home

table06 <-summary(data$distance_f_h,data$Attrition,"Distance_From_Home", "Attrition")
table06

p <- ggplot(table06, aes(x = Distance_From_Home, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Distance From Home  Vs Attrition")
fig <- ggplotly(p)
fig

#Education

table07 <-summary(data$Education,data$Attrition,"Education", "Attrition")
table07

p <- ggplot(table07, aes(x = Education, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Education  Vs Attrition")
fig <- ggplotly(p)
fig

#Education Field 


table08 <-summary(data$EducationField,data$Attrition,"Education_Field", "Attrition")
table08

p <- ggplot(table08, aes(x = Education_Field, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Education_Field  Vs Attrition")
fig <- ggplotly(p)
fig

#Environment status

table09 <-summary(data$EnvironmentStatus,data$Attrition,"Environment_status", "Attrition")
table09

p <- ggplot(table09, aes(x = Environment_status, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Environment_status  Vs Attrition")
fig <- ggplotly(p)
fig

#Gender 

table10 <-summary(data$Gender,data$Attrition,"Gender", "Attrition")
table10

p <- ggplot(table10, aes(x = Gender, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Gender  Vs Attrition")
fig <- ggplotly(p)
fig

#Hourly Rate

table11 <-summary(data$hourly_rate,data$Attrition,"Hourly_Rate", "Attrition")
table11

p <- ggplot(table11, aes(x = Hourly_Rate, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Hourly Rate  Vs Attrition")
fig <- ggplotly(p)
fig

#Job Involvement 

table12 <-summary(data$JobInvolvement,data$Attrition,"Job_Involvement", "Attrition")
table12

p <- ggplot(table12, aes(x = Job_Involvement, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Job Involvement Rate  Vs Attrition")
fig <- ggplotly(p)
fig

#Job Level

table13 <-summary(data$job_levl,data$Attrition,"Job_Level", "Attrition")
table13

p <- ggplot(table13, aes(x = Job_Level, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Job Level  Vs Attrition")
fig <- ggplotly(p)
fig

#Job Role 

table14 <-summary(data$JobRole,data$Attrition,"Job_Role", "Attrition")
table14

p <- ggplot(table14, aes(x = Job_Role, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Job Level  Vs Attrition")
fig <- ggplotly(p)
fig

#Job Satisfation

table15 <-summary(data$JobSatisfication,data$Attrition,"Job_Satisfation", "Attrition")
table15

p <- ggplot(table15, aes(x = Job_Satisfation, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Job Satisfation  Vs Attrition")
fig <- ggplotly(p)
fig

#Marital Status 

table16 <-summary(data$MartialStatus,data$Attrition,"Marital_Status", "Attrition")
table16

p <- ggplot(table16, aes(x = Marital_Status, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Marital Status  Vs Attrition")
fig <- ggplotly(p)
fig

#Monthly Income 

table17 <-summary(data$month_inc,data$Attrition,"Monthly_Income", "Attrition")
table17

p <- ggplot(table17, aes(x = Monthly_Income, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Monthly Income Vs Attrition")
fig <- ggplotly(p)
fig

#Monthly Rate 

table18 <-summary(data$month_rat,data$Attrition,"Monthly_Rate", "Attrition")
table18

p <- ggplot(table18, aes(x = Monthly_Rate, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Monthly Rate Vs Attrition")
fig <- ggplotly(p)
fig

#No Companies Work 

table19 <-summary(data$company_work,data$Attrition,"No_Companies_Work", "Attrition")
table19

p <- ggplot(table19, aes(x = No_Companies_Work, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "No Companies Work Vs Attrition")
fig <- ggplotly(p)
fig

#Over Time 

table20 <-summary(data$OverTime,data$Attrition,"Over_Time", "Attrition")
table20

p <- ggplot(table20, aes(x = Over_Time, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Over Time Vs Attrition")
fig <- ggplotly(p)
fig

#Salary Hike 

table21 <-summary(data$hike,data$Attrition,"Salary_Hike", "Attrition")
table21

p <- ggplot(table21, aes(x = Salary_Hike, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Salary Hike Vs Attrition")
fig <- ggplotly(p)
fig

#Performance Rating 

table22 <-summary(data$PerformanceRate,data$Attrition,"Performance_Rating", "Attrition")
table22

p <- ggplot(table22, aes(x = Performance_Rating, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Performance Rating Vs Attrition")
fig <- ggplotly(p)
fig

#Relationship Satisfaction

table23 <-summary(data$RelationshipSatisfication,data$Attrition,"Relationship_Satisfaction", "Attrition")
table23

p <- ggplot(table23, aes(x = Relationship_Satisfaction, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Relationship Satisfaction Vs Attrition")
fig <- ggplotly(p)
fig

#Training Last Year 

table24 <-summary(data$TrainingLastYear,data$Attrition,"Training_Last_Year", "Attrition")
table24

p <- ggplot(table24, aes(x = Training_Last_Year, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Training Last Year Vs Attrition")
fig <- ggplotly(p)
fig

#Work Life Balance


table25 <-summary(data$WorkLIfeBalance,data$Attrition,"Work_Life_Balance", "Attrition")
table25

p <- ggplot(table25, aes(x = Work_Life_Balance, y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = "Work Life Balance Vs Attrition")
fig <- ggplotly(p)
fig

bivariate_charts <- function(table)
{
p <- ggplot(table, aes(x = category , y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = title_text)
fig <- ggplotly(p)
fig
}





table25 <-summary(data$WorkLIfeBalance,data$Attrition,"Work_Life_Balance", "Attrition")
names(table25) <- c("category","Attrition","Percent","Percentage")


summary <- function(indv,dv,title_text)
{
  t1 <- table(indv,dv)
  t2 <- data.frame(prop.table(t1, 1)*100)
  t2$freq <- paste0(round(t2$Freq,1),"%")
  names(t2) <- c("category","Attrition","Percent","Percentage")
  print(t2)
  p <- ggplot( t2, aes(x = category , y = Percent, fill = Attrition , label = Percentage)) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5))+
    labs(title = title_text)
  fig <- ggplotly(p)
  fig
}
names(data)

summary(data$WorkLIfeBalance,data$Attrition,title_text="test")
summary(data$Education,data$Attrition,title_text="test")
summary(data$Gender,data$Attrition,title_text="test")

summary(data$WorkLIfeBalance,data$Attrition,title_text="test")
summary(data$WorkLIfeBalance,data$Attrition,title_text="test")
summary(data$WorkLIfeBalance,data$Attrition,title_text="test")
summary(data$WorkLIfeBalance,data$Attrition,title_text="test")
summary(data$WorkLIfeBalance,data$Attrition,title_text="test")




myfun <- function(table,title_text)
{
p <- ggplot( t2, aes(x = category , y = Percent, fill = Attrition , label = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(title = title_text)
fig <- ggplotly(p)
fig
}

myfun(table25,title_text="Work_Life_Balance")



group_function <- function(variable)
{
  q1 <- quantile(variable,c(.20))
  q2 <- quantile(variable,c(.40))
  q3 <- quantile(variable,c(.60))
  q4 <- quantile(variable,c(.80))
  
  variable_group <-NULL
  
  variable_group[variable<=q1] <- paste0("1:<=",q1)
  variable_group[variable>q1 & variable <= q2] <- paste0("2:",q1,"-",q2)
  variable_group[variable>q2 & variable <= q3] <- paste("3:",q2,"-",q3)
  variable_group[variable>q3 & variable <= q4] <- paste("4:",q3,"-",q4)
  variable_group[variable>q4] <- paste("5: ",q4,"+")
  variable_group <- as.factor(variable_group)
  return (variable_group)
}

Age_group <- group_function(data$Age)
Age_group


q1 <- quantile(data$Age,c(.20))
q1
