require(ggplot2)
require(lubridate)
require(dplyr)
require(reshape2)
require(MASS)
require(scales)
require(gridExtra)
require(Hmisc)
#Step1: data sourcing
setwd("~/Documents/Data Analytics/Bank Case Study")
loan_orig<-read.csv("loan.csv")
length(unique(loan_orig$member_id))
length(loan_orig$member_id)
sum(duplicated(loan_orig$member_id))

# Step2: Data preparation.
# There are lot of columns in the data set and there is a need to minimize the number of columns relevant for data analsis
# member id and id (Column 1 and 2) are unique for each row, so, they dont have any statistical significance.
# All columns after 49 contain NA values for the most part therefore removing them.
loan<-loan_orig[,c(3:49)]

# Further there are many columns that can be removed.
drops<-c("pymnt_plan","url","desc","zip_code","title","initial_list_status","next_pymnt_d","out_prncp","out_prncp_inv","collection_recovery_fee","total_pymnt","total_pymnt_inv","total_rec_late_fee",
         "recoveries","last_pymnt_d","next_pymnt_d","last_credit_pull_d","funded_amnt","funded_amnt_inv","earliest_cr_line","last_pymnt_amnt","total_rec_prncp",
         "total_rec_int","mths_since_last_delinq","delinq_2yrs","mths_since_last_record","emp_title","pub_rec")
#Removing the following columns for the following reasons.
#pymnt_plan: all n
#url,desc,zip_code,title: Has values which dont make much statistical sense,we have another column purpose which is useful instead of this.
#intial_list_status: Contains only one value for all row. no uni/bi variate analysis possible.
#"out_prncp","out_prncp_inv","collection_recovery_fee","total_pymnt","total_pymnt_inv","total_rec_late_fee",
#"recoveries" is far less relevant compared to the other columns for analysis
#"last_pymnt_d","next_pymnt_d","last_credit_pull_d""last_pymnt_d","next_pymnt_d","last_credit_pull_d": Dates are not relevant to either.
#"emp_title": free text value with a lot of unique values which doesn't provide any signicant insight into the data
#out_prncp and out_prncp_inv is non-zero for only current ongoing loans and it is just in 1140 current accounts. we can remove this as we can't analyse about defaulters.
#total_pymnt is a sum of toal_recieved_int , total_recieved_prncp, late_fees
#collection_recovery_fees, late_fees, recoveries etc are not directly related to the objective of the case study and also 90% of the data is 0. So we will remove it.

loan<-loan[,!(names(loan) %in% drops)]

#----------------------------Data cleaning ----------------------------------------
#Remove string literal "months" in the loan term
loan$term<-gsub("[months| ]","",loan$term)

#Remove percent symbol and changing the column name to int_rate_percentage
loan$int_rate<-gsub("%","",loan$int_rate)
colnames(loan)[which(colnames(loan)=="int_rate")]<-"int_rate_percentage"
loan$revol_util<-gsub("%","",loan$revol_util)
colnames(loan)[which(colnames(loan)=="revol_util")]<-"revol_util_percentage"

#Remove "years" and other string literals in emp_length
loan$emp_length<-gsub("year|years| ","",loan$emp_length)
loan$emp_length<-gsub("<1","0",loan$emp_length)
loan$emp_length<-gsub("10\\+","10",loan$emp_length)
loan$emp_length<-gsub("n/a","na",loan$emp_length)

#loan$sub_grade<-gsub("[A-G]","",loan$sub_grade)
loan$issue_d<-parse_date_time(paste0("01-",loan$issue_d),orders="dby",tz="Asia/Kolkata")
loan$verification_status<-gsub("Source ","",loan$verification_status)

#Convert to factors/Numeric as required
loan$term<-as.factor(loan$term)
loan$sub_grade<-as.factor(loan$sub_grade)
loan$emp_length<-as.factor(loan$emp_length)
loan$verification_status<-as.factor(loan$verification_status)
loan$loan_status<-factor(loan$loan_status,levels=c("Fully Paid","Current","Charged Off"))

loan$int_rate_percentage<-as.numeric(loan$int_rate_percentage)
loan$revol_util_percentage<-as.numeric(loan$revol_util_percentage)
#imputing nas for revol util percentage, We can use a median value.
sum(is.na(loan$revol_util_percentage))
loan[which(is.na(loan$revol_util_percentage)),]$revol_util_percentage<-median(loan$revol_util_percentage,na.rm=T)

dbl_subscript <- function(x){
  val<-c()
  for(num in 1:length(x)){
    val[num]<-x[[num]][1]
  }
  return(val)
}

#derived metrics:
# Create 4 columns for each column with numeric value.
# "LOW" first Quartile
# "MID" second Quartile
# "HIGH" third Quartile
# "VERY HIGH" fourth Quartile
#By looking into the summary-we can find out how to divide the values into low, mid, high and very high.

income_levels<-c()
income_levels[loan$annual_inc<=40000]<-"LOW"
income_levels[loan$annual_inc>40000 & loan$annual_inc<=59000]<-"MID"
income_levels[loan$annual_inc>59000 & loan$annual_inc<=82300]<-"HIGH"
income_levels[loan$annual_inc>82300]<-"VERY HIGH"
income_levels<-factor(income_levels,levels = c("LOW","MID","HIGH","VERY HIGH"))
loan<-cbind(loan,income_levels)

loan_levels<-c()
loan_levels[loan$loan_amnt<=5500]<-"LOW"
loan_levels[loan$loan_amnt>5500 & loan$loan_amnt<=10000]<-"MID"
loan_levels[loan$loan_amnt>10000 & loan$loan_amnt<=15000]<-"HIGH"
loan_levels[loan$loan_amnt>15000]<-"VERY HIGH"
loan_levels<-factor(loan_levels,levels = c("LOW","MID","HIGH","VERY HIGH"))
loan<-cbind(loan,loan_levels)

summary(loan$int_rate_percentage)
rate_levels<-c()
rate_levels[loan$int_rate_percentage <= 9.25] <- "LOW"
rate_levels[loan$int_rate_percentage > 9.25 & loan$int_rate_percentage<=11.86] <- "MID"
rate_levels[loan$int_rate_percentage > 11.86 & loan$int_rate_percentage<=14.59] <- "HIGH"
rate_levels[loan$int_rate_percentage > 14.59] <- "VERY HIGH"
rate_levels<-factor(rate_levels,levels = c("LOW","MID","HIGH","VERY HIGH"))
loan<-cbind(loan,rate_levels)

summary(loan$dti)
dti_levels<-c()
dti_levels[loan$dti <= 8.17] <- "LOW"
dti_levels[loan$dti > 8.17 & loan$dti <= 13.40] <- "MID"
dti_levels[loan$dti > 13.40 & loan$dti <= 18.6] <- "HIGH"
dti_levels[loan$dti > 18.6] <- "VERY HIGH"
dti_levels<-factor(dti_levels,levels = c("LOW","MID","HIGH","VERY HIGH"))
loan<-cbind(loan,dti_levels)

summary(loan$installment)
installment_levels <- c()
installment_levels[loan$installment <= 167.02] <- "LOW"
installment_levels[loan$installment > 167.02 & loan$installment <= 280.22] <- "MID"
installment_levels[loan$installment > 280.22 & loan$installment <= 430.78] <- "HIGH"
installment_levels[loan$installment > 430.78] <- "VERY HIGH"
installment_levels<-factor(installment_levels,levels = c("LOW","MID","HIGH","VERY HIGH"))
loan <- cbind(loan,installment_levels)

summary(loan$revol_bal)
revol_bal_levels <- c()
revol_bal_levels[loan$revol_bal <= 3703] <- "LOW"
revol_bal_levels[loan$revol_bal > 3703 & loan$revol_bal <= 8850] <- "MID"
revol_bal_levels[loan$revol_bal > 8850 & loan$revol_bal <= 17058] <- "HIGH"
revol_bal_levels[loan$revol_bal > 17058] <- "VERY HIGH"
revol_bal_levels<-factor(revol_bal_levels,levels = c("LOW","MID","HIGH","VERY HIGH"))
loan <- cbind(loan,revol_bal_levels)

summary(loan$revol_util_percentage)
revol_util_levels <- c()
revol_util_levels[loan$revol_util <= 25.50] <- "LOW"
revol_util_levels[loan$revol_util > 25.50 & loan$revol_util <= 49.30] <- "MID"
revol_util_levels[loan$revol_util > 49.30 & loan$revol_util <= 72.30] <- "HIGH"
revol_util_levels[loan$revol_util > 72.30] <- "VERY HIGH"
revol_util_levels<-factor(revol_util_levels,levels = c("LOW","MID","HIGH","VERY HIGH"))
loan <- cbind(loan,revol_util_levels)

revol_emp_len_levels <- c()
revol_emp_len_levels[loan$emp_length == "0" | loan$emp_length == "1" | loan$emp_length == "2"] <- "LOW"
revol_emp_len_levels[loan$emp_length == "3" | loan$emp_length == "4" | loan$emp_length == "5"] <- "MID"
revol_emp_len_levels[loan$emp_length == "6" | loan$emp_length == "7"| loan$emp_length == "8"] <- "HIGH"
revol_emp_len_levels[loan$emp_length == "9" | loan$emp_length == "10"] <- "VERY HIGH"
revol_emp_len_levels<-factor(revol_emp_len_levels,levels = c("LOW","MID","HIGH","VERY HIGH"))
loan <- cbind(loan,revol_emp_len_levels)

#Step3- Univariate analysis
#Collecting metadata for all columns:
# Collecting data: column name, type of the variable, # of missing values, unique values, maximum, minimum, median, inter quartile range, 90th quartile, 95th quartile, quartile 99, quartile 995, quartile 999.
col<-c(colnames(loan))
metadata<-data.frame(col)
type<-lapply(loan,class)
type_val<-dbl_subscript(type)
type_val[which(type_val=="factor")]<-"Unordered Categorical"
type_val[which(type_val!="Unordered Categorical")]<-"Quantitative"
metadata<-cbind(metadata,type_val)
metadata$type_val<-as.character(metadata$type_val)
metadata[which(metadata$col %in% c("term","grade","sub_grade","emp_length","dti_levels","income_levels","installment_levels","loan_levels","rate_levels","revol_util_levels","revol_bal_levels")),2]<-"Ordered Categorical"
f<-function(x){ sum(is.na(x))}
missing<-sapply(loan,f)
missing<-dbl_subscript(missing)
metadata<-cbind(metadata,missing)
unique<-dbl_subscript(sapply(sapply(loan,unique),length))
metadata<-cbind(metadata,unique)
#max,min, median, inter quartile range for quantitative variables.
quant_col<-metadata[which(metadata$type_val=="Quantitative"),]$col
maximum<-dbl_subscript(sapply(loan[which(colnames(loan) %in% quant_col)],max,na.rm=T))
mininum<-dbl_subscript(sapply(loan[which(colnames(loan) %in% quant_col)],min,na.rm=T))
median<-dbl_subscript(sapply(loan[which(colnames(loan) %in% quant_col)],median,na.rm=T))
inter_quartile_range<-dbl_subscript(sapply(loan[which(colnames(loan) %in% quant_col)],IQR,na.rm=T))
quartile_90_range<-dbl_subscript(sapply(loan[which(colnames(loan) %in% quant_col)],quantile,probs = .9,na.rm=T))
quartile_95_range<-dbl_subscript(sapply(loan[which(colnames(loan) %in% quant_col)],quantile,probs = .95,na.rm=T))
quartile_99_range<-dbl_subscript(sapply(loan[which(colnames(loan) %in% quant_col)],quantile,probs = .99,na.rm=T))
quartile_995_range<-dbl_subscript(sapply(loan[which(colnames(loan) %in% quant_col)],quantile,probs = .995,na.rm=T))
quartile_999_range<-dbl_subscript(sapply(loan[which(colnames(loan) %in% quant_col)],quantile,probs = .999,na.rm=T))
#metadata<-merge(metadata,data.frame(quant_col,maximum,mininum,median,inter_quartile_range,quartile_90_range,quartile_95_range,quartile_99_range,quartile_999_range),by.x ="col",by.y="quant_col",all.x = T)
metadata<-merge(metadata,data.frame(quant_col,maximum,mininum,median,inter_quartile_range,quartile_90_range,quartile_95_range,quartile_99_range,quartile_995_range,quartile_999_range),by.x ="col",by.y="quant_col",all.x = T)
#Lets see how metadata came out to be.
View(metadata)

#Write Metadata into csv file for further manipulation.
file.remove("metadata.csv")
write.csv(metadata,file = "metadata.csv")

# As seen in the metadata table
# Annual Income
# median is 59,000
# 90% of them have less than 116,000.00
# 95% of them have less than 142,000.00
# 99% of them have less than 234,999.36
# 99.5% of them have less than 300,000
# 99.9% of them have less than 676,859


#subsets for annual income
#Subset data less that 32600 to get atleast 99.5% 
percentile_99dot5 <- subset(loan, loan$annual_inc<=300000)
boxplot(percentile_99dot5$annual_inc,xlab="annual income for 99.5%")

ggplot(percentile_99dot5,aes(percentile_99dot5$annual_inc,y=..density..)   )+geom_histogram(bins=100)+xlab("Annual Income 99.5 Percentile") + geom_density(alpha=.1, fill="blue") + scale_x_continuous(labels=function(n){format(n, scientific = FALSE)}) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + geom_vline(aes(xintercept=mean(percentile_99dot5$annual_inc)),color="brown4", linetype="dashed", size=1.4)


#annual income has lot of outliers so we need to plot them without outliers. 
#generally we take 1.5 times the interquartile range to remove outliers but in case of annual income even that did not result in a good box plot. 
#So we are changing it to 3 times IQR.
annualinc_wo_outliers <- subset(loan, loan$annual_inc<3*IQR(loan$annual_inc))
boxplot(annualinc_wo_outliers$annual_inc,xlab="annual income without outliers")

temp_annual_inc <- 5500
nrow(subset(loan,loan$annual_inc<temp_annual_inc & loan_status == "Current"))/nrow(subset(loan,loan$annual_inc<temp_annual_inc ))
nrow(subset(loan,loan$annual_inc<temp_annual_inc & loan_status == "Fully Paid"))/nrow(subset(loan,loan$annual_inc<temp_annual_inc ))
nrow(subset(loan,loan$annual_inc<temp_annual_inc & loan_status == "Charged Off"))/nrow(subset(loan,loan$annual_inc<temp_annual_inc ))


# The annual Income distribution is scewed towards 

#box plots- quantitative- univariate
quant<-loan[which(colnames(loan) %in% quant_col)]
boxplot_quant<-function(x)
{
  ggplot(quant,aes(colnames(quant)[x],quant[,x]))+geom_boxplot()+ylab(colnames(quant)[x])+xlab("")
}
quant_box_plots<-lapply(c(1:length(colnames(quant))),boxplot_quant)

grid.arrange(quant_box_plots[[1]],quant_box_plots[[2]],quant_box_plots[[3]],quant_box_plots[[6]],nrow=2)



#histograms quant variables
hist_quant<-function(x)
{
  ggplot(quant,aes(x=quant[,x],y=..density..))+geom_histogram(bins=100)+xlab(colnames(quant)[x]) + geom_density(alpha=.1, fill="blue") + scale_x_continuous(labels=function(n){format(n, scientific = FALSE)}) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + geom_vline(aes(xintercept=mean(quant[,x])),color="brown4", linetype="dashed", size=1.4)
}
quant_hist_plots<-lapply(c(1:length(colnames(quant))),hist_quant)
annualinc_hist<-ggplot(annualinc_wo_outliers,aes(annualinc_wo_outliers$annual_inc))+geom_histogram(bins = 70)+xlab("annual inc")

grid.arrange(quant_hist_plots[[1]],quant_hist_plots[[2]],quant_hist_plots[[3]],quant_hist_plots[[6]],quant_hist_plots[[9]],quant_hist_plots[[10]],nrow=3)

# univariate - unordered categorical
#rank frequency plots 
freq_addr<-as.data.frame(table(loan$addr_state))
freq_purpose<-as.data.frame(table(loan$purpose)) 
colnames(freq_addr)<-c("addr_state","count")
colnames(freq_purpose)<-c("purpose","count")
freq=list(freq_addr,freq_purpose)

rank_freq_plot<-function(f){
  xlabel<-colnames(f)[1]
  colnames(f)[1]<-"var"
  f$var <- factor(f$var, levels = f$var[order(f$count,decreasing = T)])
  ggplot(f,aes(x=factor(f$var),y=f$count))+geom_bar(stat="identity")+xlab(xlabel)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

plots<-lapply(freq,rank_freq_plot)

#plotting log-log plot for rank-frequency where range of frequency is huge
log_addr<-cbind(freq_addr[order(freq_addr$count,decreasing = T),],"rank"=c(1:length(freq_addr$count)))
log_purpose<-cbind(freq_purpose[order(freq_purpose$count,decreasing = T),],"rank"=c(1:length(freq_purpose$count)))

log_addr_plot<-ggplot(log_addr,aes(x=log_addr$rank, y=log_addr$count)) + geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_log10(limits = c(1, NA), labels = trans_format("log10", math_format(10^.x)), breaks=trans_breaks("log10", function(x) 10^x, n=6)) +xlab("state rank")
log_purpose_plot<-ggplot(log_purpose,aes(x=log_purpose$rank, y=log_purpose$count)) + geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_log10(limits = c(1, NA), labels = trans_format("log10", math_format(10^.x)), breaks=trans_breaks("log10", function(x) 10^x, n=6)) +xlab("purpose rank")
grid.arrange(plots[[1]],log_addr_plot,nrow=2)
grid.arrange(plots[[2]],log_purpose_plot,nrow=2)

#subgrade distribution
subgradeplot<-ggplot(loan,aes(loan$sub_grade)) +geom_bar()+xlab("sub grade")
loan$emp_length<-factor(loan$emp_length,levels=c(0:10,NA))
#employee length distribution
emp_lengthplot<-ggplot(loan,aes(loan$emp_length))+geom_bar()+xlab("emp length")
#ggplot(quant,aes(x=quant[,x],y=..density..))+geom_histogram(bins=100)+xlab(colnames(quant)[x]) + geom_density(alpha=.1, fill="blue") + scale_x_continuous(labels=function(n){format(n, scientific = FALSE)}) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + geom_vline(aes(xintercept=mean(quant[,x])),color="brown4", linetype="dashed", size=1.4)

grid.arrange(subgradeplot,emp_lengthplot,nrow=2)

#Step 4. Segmented univariate analysis. 
#We can clearly see the subsets of different loan statuses based on colors.

chargedoff<-subset(loan,loan$loan_status=="Charged Off")
chargedoff_annual<-subset(annualinc_wo_outliers,annualinc_wo_outliers$loan_status=="Charged Off")
segmented_hist_plots<-lapply(c(1:length(colnames(chargedoff))),hist_quant)

ann_plot<-ggplot(chargedoff_annual,aes(x=chargedoff_annual$annual_inc,y=..density..))+geom_histogram(bins=100)+xlab("annual income") + geom_density(alpha=.1, fill="blue") + scale_x_continuous(labels=function(n){format(n, scientific = FALSE)}) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + geom_vline(aes(xintercept=mean(chargedoff_annual$annual_inc)),color="brown4", linetype="dashed", size=1.4)
grid.arrange(segmented_hist_plots[[1]],segmented_hist_plots[[2]],segmented_hist_plots[[3]],segmented_hist_plots[[6]],ann_plot,segmented_hist_plots[[10]],nrow=3)
summary(chargedoff)

#We can see that CA is the state with maximum charged off. But we can make the conclusions only based on percentage.
plot1 <- ggplot(chargedoff,aes(addr_state))
plot1 <- plot1 + geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot1 <- plot1 + xlab("state of defaulters")

# we can see a lot of charged off in debt consolidation but we need to see percentage to make final conclusion
plot3 <- ggplot(chargedoff,aes(purpose))+geom_bar()
plot3 <- plot3 +  theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("purpose - defaulters")


#We can see a lot of charged off in B grade but we need to see percentage to make final conclusion
plot2 <- ggplot(chargedoff,aes(sub_grade))+geom_bar()
plot2 <- plot2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("sub grade - defaulters")


#There are very high number of rental properties.
plot4<-ggplot(chargedoff,aes(x=home_ownership))+geom_bar()

grid.arrange(plot1,plot2,nrow = 2)
grid.arrange(plot3,plot4,nrow = 2)


#segmented univariate - rank frequency plots 
freq_addr_seg<-as.data.frame(table(chargedoff$addr_state))
freq_purpose_seg<-as.data.frame(table(chargedoff$purpose)) 
colnames(freq_addr_seg)<-c("addr_state","count")
colnames(freq_purpose_seg)<-c("purpose","count")
freq_seg=list(freq_addr_seg,freq_purpose_seg)

plots_seg<-lapply(freq_seg,rank_freq_plot)

#plotting log-log plot for rank-frequency where range of frequency is huge
log_addr<-cbind(freq_addr_seg[order(freq_addr_seg$count,decreasing = T),],"rank"=c(1:length(freq_addr_seg$count)))
log_purpose<-cbind(freq_purpose_seg[order(freq_purpose_seg$count,decreasing = T),],"rank"=c(1:length(freq_purpose_seg$count)))

log_addr_plot<-ggplot(log_addr,aes(x=log_addr$rank, y=log_addr$count)) + geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_log10(limits = c(1, NA), labels = trans_format("log10", math_format(10^.x)), breaks=trans_breaks("log10", function(x) 10^x, n=6)) +xlab("state rank-defaulters")
log_purpose_plot<-ggplot(log_purpose,aes(x=log_purpose$rank, y=log_purpose$count)) + geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_log10(limits = c(1, NA), labels = trans_format("log10", math_format(10^.x)), breaks=trans_breaks("log10", function(x) 10^x, n=6)) +xlab("purpose rank-defaulters")
grid.arrange(plots_seg[[1]],log_addr_plot,nrow=2)
grid.arrange(plots_seg[[2]],log_purpose_plot,nrow=2)

#subgrade distribution - segmented
subgradeplot_seg<-ggplot(chargedoff,aes(sub_grade)) +geom_bar()+xlab("sub grade for defaulters")
chargedoff$emp_length<-factor(chargedoff$emp_length,levels=c(0:10,NA))
#employee length distribution
emp_lengthplot<-ggplot(chargedoff,aes(emp_length))+geom_bar()+xlab("emp length for defaulters")
grid.arrange(subgradeplot_seg,emp_lengthplot,nrow=2)


#Step 5: Bivariate
#Lets do more analysis based on percentage of different loan statuses. In each of the below graphs, we can see the percentage of charged off, current and paid for each of the other variables.

# subgrade
seg_sub_grade<- loan%>%group_by(sub_grade,loan_status)%>%summarise(count = n()) %>% mutate(perc=count/sum(count))
sg1<-ggplot(seg_sub_grade, aes(x = factor(sub_grade), y = perc*100, fill = factor(loan_status))) + geom_bar(stat="identity", width = 0.7) + 
  labs(x = "sub grade", y = "perc", fill = "status") + theme_minimal(base_size = 14)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
sg2<-ggplot(loan,aes(sub_grade,fill=loan_status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(sg1,sg2,nrow=2)

# dti_levels
seg_dti<- loan %>% group_by(dti_levels,loan_status) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))
dti1<-ggplot(seg_dti, aes(x = factor(dti_levels,levels = c("LOW","MID","HIGH","VERY HIGH")), y = perc*100, fill = factor(loan_status,levels = c("Fully Paid","Current","Charged Off")))) +  geom_bar(stat="identity", width = 0.7) +
  labs(x = "dti levels", y = "perc", fill = "status") +  theme_minimal(base_size = 14)
# dti and chances of defaulting are directly proportional. more the dti, more they default.
dti2<-ggplot(loan,aes(dti_levels,fill=loan_status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(dti1,dti2,ncol=2)

# interest rate levels
seg_rate<- loan %>% group_by(rate_levels,loan_status) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))
r1<-ggplot(seg_rate, aes(x = factor(rate_levels), y = perc*100, fill = factor(loan_status))) +  geom_bar(stat="identity", width = 0.7) +
  labs(x = "rate levels", y = "perc", fill = "status") +  theme_minimal(base_size = 14)
#We can see that when the rate levels are very high, more its more likely that borrowers default.
r2<-ggplot(loan,aes(rate_levels,fill=loan_status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(r1,r2,ncol=2)

# loan levels
seg_loan<- loan %>% group_by(loan_levels,loan_status) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))
l1<-ggplot(seg_loan, aes(x = factor(loan_levels,levels = c("LOW","MID","HIGH","VERY HIGH")), y = perc*100, fill = factor(loan_status,levels = c("Fully Paid","Current","Charged Off")))) +  geom_bar(stat="identity", width = 0.7) +
  labs(x = "loan levels", y = "perc", fill = "status") +  theme_minimal(base_size = 14)
#We can see that when the loan quantity amount is very high, chances of defaulting is higher
l2<-ggplot(loan,aes(loan_levels,fill=loan_status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(l1,l2,ncol=2)

# income_levels
seg_income<- annualinc_wo_outliers %>% group_by(income_levels,loan_status) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))
i1<-ggplot(seg_income, aes(x = factor(income_levels,levels = c("LOW","MID","HIGH","VERY HIGH")), y = perc*100, fill = factor(loan_status,levels = c("Fully Paid","Current","Charged Off")))) +  geom_bar(stat="identity", width = 0.7) +
  labs(x = "income", y = "perc", fill = "status") +  theme_minimal(base_size = 14)
i2<-ggplot(annualinc_wo_outliers,aes(income_levels,fill=loan_status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(i1,i2,ncol=2)
# as income levels increase, the chances of defaulting decreases.

# employment length
seg_emplen<- loan %>% group_by(revol_emp_len_levels,loan_status) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))
e1<-ggplot(seg_emplen, aes(x = factor(revol_emp_len_levels), y = perc*100, fill = factor(loan_status))) + geom_bar(stat="identity", width = 0.7) + 
  labs(x = "employment length", y = "perc", fill = "status") + theme_minimal(base_size = 14)
#Not much difference between different employment lengths
e2<-ggplot(loan,aes(revol_emp_len_levels,fill=loan_status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(e1,e2,nrow=1)
#Not much difference

# source verification
seg_verification<- loan %>% group_by(verification_status,loan_status) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))
v1<-ggplot(seg_verification, aes(x = factor(verification_status), y = perc*100, fill = factor(loan_status))) + geom_bar(stat="identity", width = 0.7) + 
  labs(x = "verification status", y = "perc", fill = "status") + theme_minimal(base_size = 14)
# Surprisingly, people are more likely to default when the income source is verified. May be the verification process is not good enough.
v2<-ggplot(loan,aes(verification_status,fill=loan_status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(v1,v2,nrow=1)

# home ownership
seg_home<- loan %>% group_by(home_ownership,loan_status) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))
h1<-ggplot(seg_home, aes(x = factor(home_ownership), y = perc*100, fill = factor(loan_status))) + geom_bar(stat="identity", width = 0.7) + 
  labs(x = "home ownership", y = "perc", fill = "status") + theme_minimal(base_size = 14)
h2<-ggplot(loan,aes(home_ownership,fill=loan_status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(h1,h2,ncol=2)
#Not much to say here - does not seem to affect.

#10. purpose
seg_purpose<- loan %>% group_by(purpose,loan_status) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))
p1<-ggplot(seg_purpose, aes(x = factor(purpose), y = perc*100, fill = factor(loan_status))) + geom_bar(stat="identity", width = 0.7) + 
  labs(x = "purpose", y = "perc", fill = "status") + theme_minimal(base_size = 14)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
p2<-ggplot(loan,aes(purpose,fill=loan_status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(p1,p2,ncol= 2)
#When we go by numbers, debt consolidation seems like a bigger driver however when we see percentages we see small business has highest percentage of defaulters.

# address state
seg_addr_st<- loan %>% group_by(addr_state,loan_status) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))
a1<-ggplot(seg_addr_st, aes(x = factor(addr_state), y = perc*100, fill = factor(loan_status))) + geom_bar(stat="identity", width = 0.7) + 
  labs(x = "state", y = "perc", fill = "status") + theme_minimal(base_size = 14)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
a2<-ggplot(loan,aes(addr_state,fill=loan_status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(a1,a2,nrow=2)
#At first look, when we analyse only the numbers and not percentage of defaulters, address state being CA-california is a driver to some extent however, since the number of loans from CA are high too, the percentage of defaulters in that region is not too high

# Term
seg_term<- loan %>% group_by(term,loan_status) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))
t1<-ggplot(seg_term, aes(x = factor(term), y = perc*100, fill = factor(loan_status))) + geom_bar(stat="identity", width = 0.7) + 
  labs(x = "term", y = "perc", fill = "status") + theme_minimal(base_size = 14)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
t2<-ggplot(loan,aes(term,fill=loan_status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(t1,t2,ncol=1)
# graph is a clear indicator that higher term leads to more chances of default.

# revol util
seg_revol_util_levels<- loan %>% group_by(revol_util_levels,loan_status) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))
ru1<-ggplot(seg_revol_util_levels, aes(x = factor(revol_util_levels), y = perc*100, fill = factor(loan_status))) + geom_bar(stat="identity", width = 0.7) + 
  labs(x = "revol_util_levels", y = "perc", fill = "status") + theme_minimal(base_size = 14)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ru2<-ggplot(loan,aes(revol_util_levels,fill=loan_status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(ru1,ru2,ncol=1)

#bivariate- correlation matrix
heat_mat<-c("loan_amnt","int_rate_percentage","installment","annual_inc","dti","inq_last_6mths","open_acc","total_acc","revol_bal","revol_util_percentage")
num_loan<-loan[,(names(loan) %in% heat_mat)]
cormat_loan<-round(cor(num_loan),2)
melted_cormat_loan<-melt(cormat_loan)
View(melted_cormat_loan)
ggplot(data = melted_cormat_loan, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+geom_text(aes(label=value))


#grade vs ver in segmented by charged off
default_grade_ver<- chargedoff %>% group_by(grade,verification_status) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))
ru1<-ggplot(default_grade_ver, aes(x = factor(grade), y = perc*100, fill = factor(verification_status))) + geom_bar(stat="identity", width = 0.7) + 
  labs(x = "grade", y = "perc", fill = "verification status") + theme_minimal(base_size = 14)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ru2<-ggplot(chargedoff,aes(x=grade,fill=verification_status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(ru1,ru2,ncol=2)
# we can see that as the grade decreases, even the verified  income sources default. There could be an issue with the verification process itself.

#Driver variables which lead to more chance of defaulting - not in this order though:
#Higher dti levels
#Higher revol util percentage
#Higher loan levels
#Higher interest rate levels
#Lower annual income
#Source verification: Verified source tends to default more : When we analyse grades and verification source, we see that as grades increase(from A to G), the verified sources tend to default more than unverified source. This clearly shows there is an issue with the verification process.
#Purpose: When we go by total number of defaulters instead of percentage, debt consolidation seems like a bigger driver however when we see percentages we see small business has highest percentage of defaulters.
#Address state: At first look, when we analyse only the numbers and not percentage of defaulters, address state being CA-california is a driver to some extent however, since the number of loans from CA are high too, the percentage of defaulters in that region is not too high
#Term: Longer term shows higher percentage of defaulters.
