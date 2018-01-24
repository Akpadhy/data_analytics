################################################################

require(MASS)
require(e1071)
require(car)
require(caret)
require(ggplot2)
require(cowplot)
require(caTools)
require(lubridate)
require(gridExtra)

########################################################################
#Data sourcing:
setwd("~/Downloads/hr")
emp<-read.csv("employee_survey_data.csv")
gen<-read.csv("general_data.csv")
manager<-read.csv("manager_survey_data.csv")
in_time<-read.csv("in_time.csv")
out_time<-read.csv("out_time.csv")

########################################################################
#Data preparation and cleaning:
f<-function(X){parse_date_time(X,orders="Ymd HMS",tz="Asia/Kolkata")}

in_time<-in_time[colSums(!is.na(in_time)) > 0]
in_time<-cbind(in_time[,1],data.frame(lapply(in_time[,-1],f)))

out_time<-out_time[colSums(!is.na(out_time)) > 0]
out_time<-cbind(out_time[,1],data.frame(lapply(out_time[,-1],f)))
setdiff(colnames(in_time),colnames(out_time))
#Looks like nas are same in both in and out time
setdiff(which(is.na(in_time)),which(is.na(out_time)))
#ids are same too:
setdiff(in_time$`in_time[, 1]`,out_time$`out_time[, 1]`)
#We can calculate duration and merge the dataframes along with duration
duration<-out_time[,-1]-in_time[,-1]

ave<-function(index,X){sum(as.numeric(X[index,]),na.rm = T)/sum(!is.na(X[index,]))}

jan_all<-duration[,which(grepl("X2015.01",colnames(duration)))]
feb_all<-duration[,which(grepl("X2015.02",colnames(duration)))]
mar_all<-duration[,which(grepl("X2015.03",colnames(duration)))]
apr_all<-duration[,which(grepl("X2015.04",colnames(duration)))]
may_all<-duration[,which(grepl("X2015.05",colnames(duration)))]
jun_all<-duration[,which(grepl("X2015.06",colnames(duration)))]
jul_all<-duration[,which(grepl("X2015.07",colnames(duration)))]
aug_all<-duration[,which(grepl("X2015.08",colnames(duration)))]
sep_all<-duration[,which(grepl("X2015.09",colnames(duration)))]
oct_all<-duration[,which(grepl("X2015.10",colnames(duration)))]
nov_all<-duration[,which(grepl("X2015.11",colnames(duration)))]
dec_all<-duration[,which(grepl("X2015.12",colnames(duration)))]
num_of_emp<-length(gen$EmployeeID)
janAvg<-sapply(c(1:num_of_emp),ave,jan_all)
febAvg<-sapply(c(1:num_of_emp),ave,feb_all)
marAvg<-sapply(c(1:num_of_emp),ave,mar_all)
aprAvg<-sapply(c(1:num_of_emp),ave,apr_all)
mayAvg<-sapply(c(1:num_of_emp),ave,may_all)
junAvg<-sapply(c(1:num_of_emp),ave,jun_all)
julAvg<-sapply(c(1:num_of_emp),ave,jul_all)
augAvg<-sapply(c(1:num_of_emp),ave,aug_all)
sepAvg<-sapply(c(1:num_of_emp),ave,sep_all)
octAvg<-sapply(c(1:num_of_emp),ave,oct_all)
novAvg<-sapply(c(1:num_of_emp),ave,nov_all)
decAvg<-sapply(c(1:num_of_emp),ave,dec_all)
average_hrs<-(janAvg+febAvg+marAvg+aprAvg+mayAvg+junAvg+julAvg+augAvg+sepAvg+octAvg+novAvg+decAvg)/12
all_emp<-merge(manager,emp,by="EmployeeID")
all_emp<-merge(all_emp,gen,by="EmployeeID")
all_emp<-cbind(all_emp,average_hrs)

#Removing the columns which contain only one value:
all_emp<-all_emp[,which(colnames(all_emp)!="StandardHours")]
all_emp<-all_emp[,which(colnames(all_emp)!="Over18")]
all_emp<-all_emp[,which(colnames(all_emp)!="EmployeeCount")]
all_emp$JobInvolvement<-as.factor(all_emp$JobInvolvement)
all_emp$PerformanceRating<-as.factor(all_emp$PerformanceRating)
all_emp$EnvironmentSatisfaction<-as.factor(all_emp$EnvironmentSatisfaction)
all_emp$JobSatisfaction<-as.factor(all_emp$JobSatisfaction)
all_emp$WorkLifeBalance<-as.factor(all_emp$WorkLifeBalance)
all_emp$Education<-as.factor(all_emp$Education)
all_emp$JobLevel<-as.factor(all_emp$JobLevel)
all_emp$StockOptionLevel<-as.factor(all_emp$StockOptionLevel)

#Check for duplicates
sum(duplicated(all_emp))

#Check if  Employee id is unique
setdiff(unique(all_emp$EmployeeID),all_emp$EmployeeID)
setdiff(all_emp$EmployeeID,unique(all_emp$EmployeeID))
#it is unique but has no statisticla significance. Hence makes sense to remove it
all_emp<-all_emp[,which(colnames(all_emp)!="EmployeeID")]

########################################################################
#Derived metrics:
income_levels<-c()
income_levels[all_emp$MonthlyIncome<=29110]<-"LOW"
income_levels[all_emp$MonthlyIncome>29110 & all_emp$MonthlyIncome<=49190]<-"MID"
income_levels[all_emp$MonthlyIncome>49190 & all_emp$MonthlyIncome<=83800]<-"HIGH"
income_levels[all_emp$MonthlyIncome>83800]<-"VERY HIGH"
income_levels<-factor(income_levels,levels = c("LOW","MID","HIGH","VERY HIGH"))
all_emp<-cbind(all_emp,income_levels)

########################################################################
#NAs imputation:
#for categorical variables, replacing nas with most common value:
all_emp[which(is.na(all_emp$EnvironmentSatisfaction)),]$EnvironmentSatisfaction<-3
all_emp[which(is.na(all_emp$JobSatisfaction)),]$JobSatisfaction<-4
all_emp[which(is.na(all_emp$WorkLifeBalance)),]$WorkLifeBalance<-3
all_emp[which(is.na(all_emp$NumCompaniesWorked)),]$NumCompaniesWorked<-1
all_emp[which(is.na(all_emp$TotalWorkingYears)),]$TotalWorkingYears<-10

#EDA
########################################################################
#Univariate and segmented analysis
box_plot_cont<-function(x)
{
  ggplot(all_emp,aes(colnames(all_emp)[x],all_emp[,x]))+geom_boxplot()+xlab(colnames(all_emp)[x])
}
plots_cont_box<-lapply(c(6,10,17,18,19,21,22,23,24,25,26),box_plot_cont)
grid.arrange(plots_cont_box[[1]],plots_cont_box[[2]],plots_cont_box[[3]],plots_cont_box[[4]],plots_cont_box[[5]],plots_cont_box[[6]],nrow=3)
grid.arrange(plots_cont_box[[7]],plots_cont_box[[8]],plots_cont_box[[9]],plots_cont_box[[10]],plots_cont_box[[11]],nrow=3)

########################################################################
#segmented analysis: numeric variables - histogram and barcharts
plot_seg_cont<-function(x)
{
  ggplot(all_emp,aes(all_emp[,x],fill=all_emp$Attrition))+geom_bar()+xlab(colnames(all_emp)[x]) + geom_density(alpha=.1, fill="blue") + scale_x_continuous(labels=function(n){format(n, scientific = FALSE)}) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + geom_vline(aes(xintercept=mean(all_emp[,x])),color="brown4", linetype="dashed", size=1.4)
}
plots_cont<-lapply(c(6,10,18,19,21,22,23,24,25),plot_seg_cont)
plot_avg_hrs<-ggplot(all_emp,aes(x=all_emp$average_hrs, fill=all_emp$Attrition))+geom_histogram(binwidth = 0.5);
grid.arrange(plots_cont[[1]],plots_cont[[2]],plots_cont[[3]],plots_cont[[4]],plots_cont[[5]],nrow=3)
grid.arrange(plots_cont[[7]],plots_cont[[8]],plots_cont[[9]],plots_cont[[6]],plot_avg_hrs,nrow=3)

########################################################################
#categorical variables - bar chart
plot_seg<-function(x)
{
  ggplot(all_emp,aes(all_emp[,x],fill=all_emp$Attrition))+geom_bar()+xlab(colnames(all_emp)[x])
}
plots<-lapply(c(1,2,3,4,5,8,9,11,12,13,14,15,16,20,27),plot_seg)
grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]],nrow=3)
grid.arrange(plots[[7]],plots[[8]],plots[[9]],plots[[10]],plots[[11]],plots[[12]],nrow=3)
grid.arrange(plots[[13]],plots[[14]],plots[[15]],nrow=2)

########################################################################
#Outlier treatment

age_limit <- quantile(all_emp$Age, 0.99)
income_limit <- quantile(all_emp$MonthlyIncome,0.99)
totalWorkingYears_limit <- quantile(all_emp$TotalWorkingYears,0.99)
yearsAtCompany_limit <- quantile(all_emp$YearsAtCompany,0.99)
yearsSincePromo_limit <- quantile(all_emp$YearsSinceLastPromotion,0.99)
yearsWithCurrManager_limit <- quantile(all_emp$YearsWithCurrManager,0.99)

emp_no_outliers <- subset(all_emp,all_emp$Age < age_limit &
                            all_emp$MonthlyIncome < income_limit &
                            all_emp$TotalWorkingYears < totalWorkingYears_limit &
                            all_emp$YearsAtCompany < yearsAtCompany_limit &
                            all_emp$YearsSinceLastPromotion < yearsSincePromo_limit &
                            all_emp$YearsWithCurrManager < yearsWithCurrManager_limit)

# We will be making the correlation matrix later after we create dummy variables.

########################################################################
#create dummies for levels:2
create_dummy<-function(x){
  if(class(x)=="factor"){
    levels(x)<-c(0:length(levels(x)))
    x <- as.numeric(levels(x))[x]
    return(x)
  }
}
emp_final<-emp_no_outliers
emp_final$PerformanceRating<-create_dummy(emp_no_outliers$PerformanceRating)
emp_final$Attrition<-create_dummy(emp_no_outliers$Attrition)
emp_final$Gender<-create_dummy(emp_no_outliers$Gender)

#JobInvolvement
dummy <- data.frame(model.matrix( ~JobInvolvement, data = emp_final))
dummy <- dummy[,-1]
emp_final <- cbind(emp_final[,-which(colnames(emp_final)=="JobInvolvement")], dummy)

#EnvironmentSatisfaction
dummy <- data.frame(model.matrix( ~EnvironmentSatisfaction, data = emp_final))
dummy <- dummy[,-1]
emp_final <- cbind(emp_final[,-which(colnames(emp_final)=="EnvironmentSatisfaction")], dummy)

#JobSatisfaction
dummy <- data.frame(model.matrix( ~JobSatisfaction, data = emp_final))
dummy <- dummy[,-1]
emp_final <- cbind(emp_final[,-which(colnames(emp_final)=="JobSatisfaction")], dummy)

#WorkLifeBalance
dummy <- data.frame(model.matrix( ~WorkLifeBalance, data = emp_final))
dummy <- dummy[,-1]
emp_final <- cbind(emp_final[,-which(colnames(emp_final)=="WorkLifeBalance")], dummy)

#BusinessTravel
dummy <- data.frame(model.matrix( ~BusinessTravel, data = emp_final))
dummy <- dummy[,-1]
emp_final <- cbind(emp_final[,-which(colnames(emp_final)=="BusinessTravel")], dummy)

#Department
dummy <- data.frame(model.matrix( ~Department, data = emp_final))
dummy <- dummy[,-1]
emp_final <- cbind(emp_final[,-which(colnames(emp_final)=="Department")], dummy)

#Education
dummy <- data.frame(model.matrix( ~Education, data = emp_final))
dummy <- dummy[,-1]
emp_final <- cbind(emp_final[,-which(colnames(emp_final)=="Education")], dummy)

#EducationField
dummy <- data.frame(model.matrix( ~EducationField, data = emp_final))
dummy <- dummy[,-1]
emp_final <- cbind(emp_final[,-which(colnames(emp_final)=="EducationField")], dummy)

#JobLevel
dummy <- data.frame(model.matrix( ~JobLevel, data = emp_final))
dummy <- dummy[,-1]
emp_final <- cbind(emp_final[,-which(colnames(emp_final)=="JobLevel")], dummy)

#JobRole
dummy <- data.frame(model.matrix( ~JobRole, data = emp_final))
dummy <- dummy[,-1]
emp_final <- cbind(emp_final[,-which(colnames(emp_final)=="JobRole")], dummy)

#MaritalStatus
dummy <- data.frame(model.matrix( ~MaritalStatus, data = emp_final))
dummy <- dummy[,-1]
emp_final <- cbind(emp_final[,-which(colnames(emp_final)=="MaritalStatus")], dummy)

#StockOptionLevel
dummy <- data.frame(model.matrix( ~StockOptionLevel, data = emp_final))
dummy <- dummy[,-1]
emp_final <- cbind(emp_final[,-which(colnames(emp_final)=="StockOptionLevel")], dummy)
emp_final<-emp_final[,-which(colnames(emp_final)=="income_levels")]

########################################################################
#scale continuous variables
emp_final$Age<-scale(emp_final$Age)
emp_final$MonthlyIncome<-scale(emp_final$MonthlyIncome)
emp_final$DistanceFromHome<-scale(emp_final$DistanceFromHome)
emp_final$PercentSalaryHike<-scale(emp_final$PercentSalaryHike)
emp_final$NumCompaniesWorked<-scale(emp_final$NumCompaniesWorked)
emp_final$TotalWorkingYears<-scale(emp_final$TotalWorkingYears)
emp_final$TrainingTimesLastYear<-scale(emp_final$TrainingTimesLastYear)
emp_final$YearsAtCompany<-scale(emp_final$YearsAtCompany)
emp_final$YearsSinceLastPromotion<-scale(emp_final$YearsSinceLastPromotion)
emp_final$YearsWithCurrManager<-scale(emp_final$YearsWithCurrManager)
emp_final$average_hrs<-scale(emp_final$average_hrs)

########################################################################
#correlation matrix for numeric
corrs = cor(emp_final)
View(corrs)

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(emp_final$Attrition, SplitRatio = 0.7)

train = emp_final[indices,]

test = emp_final[!(indices),]

########################################################################
#Logistic regression
#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
vif(model_2)

#removing:DepartmentResearch...Development, EducationFieldLife.Sciences, EducationFieldMarketing, EducationFieldMedical
#EducationFieldTechnical.Degree, JobRoleSales.Representative due to high vif and low significance
model_3<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               average_hrs + JobInvolvement3 + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
               JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
               WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
               BusinessTravelTravel_Rarely + DepartmentSales + Education2 + EducationFieldOther + 
               JobLevel5 + JobRoleHuman.Resources + JobRoleManager + 
               JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + StockOptionLevel1, 
             family = "binomial", data = train)

summary(model_3)
vif(model_3)

#Removing: BusinessTravelTravel_Rarely due to high vif and relatively low significance among the variables with high vif
model_4<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               average_hrs + JobInvolvement3 + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
               JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
               WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
               DepartmentSales + Education2 + EducationFieldOther + 
               JobLevel5 + JobRoleHuman.Resources + JobRoleManager + 
               JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + StockOptionLevel1, 
             family = "binomial", data = train)
summary(model_4)
vif(model_4)

#Removing DepartmentSales, Education2, JobLevel5, JobRoleHuman.Resources, JobRoleResearch.Director due to low significance.
model_5<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               average_hrs + JobInvolvement3 + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
               JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
               WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
               EducationFieldOther + JobRoleManager + JobRoleManufacturing.Director + 
               MaritalStatusSingle + StockOptionLevel1, 
               family = "binomial", data = train)
summary(model_5)
vif(model_5)

#Removing WorkLifeBalance4 due to high vif and lower significance compared to other variables with high vif.
model_6<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               average_hrs + JobInvolvement3 + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
               JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
               WorkLifeBalance3 + BusinessTravelTravel_Frequently + 
               EducationFieldOther + JobRoleManager + JobRoleManufacturing.Director + 
               MaritalStatusSingle + StockOptionLevel1, 
             family = "binomial", data = train)
summary(model_6)
vif(model_6)

#No more variables can be removed due to high vif.
#Removing EducationFieldOther due to low significance.
model_7<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               average_hrs + JobInvolvement3 + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
               JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
               WorkLifeBalance3 + BusinessTravelTravel_Frequently + 
               JobRoleManager + JobRoleManufacturing.Director + 
               MaritalStatusSingle + StockOptionLevel1, 
             family = "binomial", data = train)
summary(model_7)
vif(model_7)

#Removing StockOptionLevel1 due to relatively lower significance
model_8<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               average_hrs + JobInvolvement3 + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
               JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
               WorkLifeBalance3 + BusinessTravelTravel_Frequently + 
               JobRoleManager + JobRoleManufacturing.Director + MaritalStatusSingle, 
             family = "binomial", data = train)
summary(model_8)
vif(model_8)

#Removing: TrainingTimesLastYear, JobInvolvement3 due to lower significance.
model_9<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               YearsSinceLastPromotion + YearsWithCurrManager + average_hrs + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
               JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
               BusinessTravelTravel_Frequently + JobRoleManager + JobRoleManufacturing.Director + MaritalStatusSingle, 
             family = "binomial", data = train)
summary(model_9)
vif(model_9)
#Removing Age due to its low significance
model_10<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
               YearsSinceLastPromotion + YearsWithCurrManager + average_hrs + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
               JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
               BusinessTravelTravel_Frequently + JobRoleManager + JobRoleManufacturing.Director + MaritalStatusSingle, 
             family = "binomial", data = train)
summary(model_10)
vif(model_10)

#Removing JobRoleManager due to its low significance
model_11<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_hrs + EnvironmentSatisfaction2 + 
                EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                BusinessTravelTravel_Frequently + JobRoleManufacturing.Director + MaritalStatusSingle, 
              family = "binomial", data = train)
summary(model_11)
vif(model_11)

#Removing WorkLifeBalance2 due to its low significance
model_12<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_hrs + EnvironmentSatisfaction2 + 
                EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance3 + 
                BusinessTravelTravel_Frequently + JobRoleManufacturing.Director + MaritalStatusSingle, 
              family = "binomial", data = train)
summary(model_12)
vif(model_12)

#Removing JobRoleManufacturing.Director due to relatively lower significance
model_13<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_hrs + EnvironmentSatisfaction2 + 
                EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance3 + 
                BusinessTravelTravel_Frequently + MaritalStatusSingle, 
              family = "binomial", data = train)
summary(model_13)
vif(model_13)

#Removing job satisfaction2, worklifebalance as they are relatively less significant
model_14<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_hrs + 
                EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobSatisfaction3 + JobSatisfaction4 +  + EnvironmentSatisfaction2 +
                BusinessTravelTravel_Frequently + MaritalStatusSingle, 
              family = "binomial", data = train)
summary(model_14)
vif(model_14)

#Removing jobSatisfaction3 due to low significance.
model_15<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_hrs + 
                EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobSatisfaction4 +  + EnvironmentSatisfaction2 +
                BusinessTravelTravel_Frequently + MaritalStatusSingle, 
              family = "binomial", data = train)
summary(model_15)
vif(model_15)


final_model<- model_15

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)


#######################################################################
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.1616162 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1616162, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

graphics::plot(attrition_decile$bucket, attrition_decile$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")
graphics::plot(attrition_decile$bucket, attrition_decile$Gain, type="l", ylab="Gain", xlab="Bucket")
