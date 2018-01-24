library(ggplot2)
library("MASS")
library("car")

setwd("~/Downloads")
car_1 <- read.csv("CarPrice_Assignment.csv")
str(car_1)

# cleaning data

#cleaning the mispelt values in enginetype 
car_1$enginetype <- as.character(car_1$enginetype)
car_1[which(car_1$enginetype == "dohcv"),]$enginetype <-"dohc"
car_1$enginetype <- as.factor (car_1$enginetype)

#cleaning the mispelt values in fuelsystem
car_1$fuelsystem <- as.character(car_1$fuelsystem)
car_1 [which(car_1$fuelsystem == "mfi"),]$fuelsystem <- "mpfi"
car_1 [which(car_1$fuelsystem == "spdi"),]$fuelsystem <- "spfi"
car_1$fuelsystem <- as.factor( car_1$fuelsystem )

#Per requirement we should only use the company name and not the model name. So getting rid of it. 
car_1$CarName<-as.character(car_1$CarName)
car_1$CarName<-gsub(" .*","",car_1$CarName)

#cleaning the mispelt names in CarName:
car_1[which(car_1$CarName=="Nissan"),]$CarName <-"nissan"
car_1[which(car_1$CarName=="toyouta"),]$CarName <-"toyota"
car_1[which(car_1$CarName=="porcshce"),]$CarName <-"porsche"
car_1[which(car_1$CarName=="maxda"),]$CarName <- "mazda"
car_1[which(car_1$CarName=="vokswagen"),]$CarName<-"volkswagen"
car_1[which(car_1$CarName=="vw"),]$CarName<-"volkswagen"
car_1$CarName<-as.factor(car_1$CarName)

#converting symboling to a factor type variable. 
car_1$symboling<-as.factor(car_1$symboling)

#removing duplicate rows of rows
dedupcar_1 <- unique(car_1)
#there are no duplicates

#EDA to understand the dataset better:
#Creating derived metrics
price_levels<-c()
price_levels[car$price<=7788]<-"LOW"
price_levels[car$price>7788 & car$price<=10295]<-"MID"
price_levels[car$price>10295 & car$price<=16503]<-"HIGH"
price_levels[car$price>16503]<-"VERY HIGH"
price_levels<-factor(price_levels,levels = c("LOW","MID","HIGH","VERY HIGH"))
car<-cbind(car,price_levels)

#Univariate analysis
box_plot_cont<-function(x)
{
  ggplot(car,aes(colnames(car)[x],car[,x]))+geom_boxplot()+xlab(colnames(car)[x])
}
plots_cont_box<-lapply(c(10,11,12,13,14,17,19,20,21,22,23,24),box_plot_cont)
grid.arrange(plots_cont_box[[1]],plots_cont_box[[2]],plots_cont_box[[3]],plots_cont_box[[4]],plots_cont_box[[5]],plots_cont_box[[6]],nrow=3)
grid.arrange(plots_cont_box[[7]],plots_cont_box[[8]],plots_cont_box[[9]],plots_cont_box[[10]],plots_cont_box[[11]],plots_cont_box[[12]],nrow=3)
summary(car)
#segmented univariate analysis.
#categorical variables
plot_seg<-function(x)
{
  ggplot(car,aes(car[,x],fill=price_levels))+geom_bar()+xlab(colnames(car)[x])
}
plots<-lapply(c(2,4,5,6,7,8,9,15,16,18),plot_seg)
grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]],nrow=3)
grid.arrange(plots[[7]],plots[[8]],plots[[9]],plots[[10]],nrow=3)

#continuous variables
plot_seg_cont<-function(x)
{
  ggplot(car,aes(car[,x],car$price))+geom_smooth()+geom_line()+xlab(colnames(car)[x])
}
plots_cont<-lapply(c(10,11,12,13,14,17,19,20,21,22,23,24),plot_seg_cont)
grid.arrange(plots_cont[[1]],plots_cont[[2]],plots_cont[[3]],plots_cont[[4]],plots_cont[[5]],plots_cont[[6]],nrow=3)
grid.arrange(plots_cont[[7]],plots_cont[[8]],plots_cont[[9]],plots_cont[[10]],plots_cont[[11]],plots_cont[[12]],nrow=3)


#removing outliers:
nums <- sapply(car_1, is.numeric)
numeric_carData <- car_1[,nums]

summary(numeric_carData)
# anything beyond the 99 percentile range for the above mentioned columns can be considered as outliers

outlier_range <- .99

enginesize_limit <- quantile(numeric_carData$enginesize, c(outlier_range))

compressionratio_limit <- quantile(numeric_carData$compressionratio,c(outlier_range))

horsepower_limit <- quantile(numeric_carData$horsepower,c(outlier_range))

price_limit <- quantile(numeric_carData$price,c(outlier_range))

car_1_no_outliers <- subset(car_1, car_1$enginesize <= enginesize_limit & 
                                        car_1$compressionratio <= compressionratio_limit &
                                        car_1$horsepower <= horsepower_limit &
                                        car_1$price <= price_limit )

summary(car_1_no_outliers)

#Converting variables to appropriate types
car_1$CarName <- as.character(car_1$CarName)

#Creating dummy variables  for all factor variables
#fueltype, aspiration,doornumber, enginelocation have two levels
create_dummy<-function(x){
  if(class(x)=="factor"){
    levels(x)<-c(0:length(levels(x)))
    x <- as.numeric(levels(x))[x]
    return(x)
  }
}
car_1_no_outliers$fueltype<-create_dummy(car_1_no_outliers$fueltype)
car_1_no_outliers$aspiration<-create_dummy(car_1_no_outliers$aspiration)
car_1_no_outliers$doornumber<-create_dummy(car_1_no_outliers$doornumber)
car_1_no_outliers$enginelocation<-create_dummy(car_1_no_outliers$enginelocation)

str(car_1_no_outliers)

# Create "dummy" columns for factor variables.
# Fuel System 6 levels
dummy <- data.frame(model.matrix( ~fuelsystem, data = car_1_no_outliers))
#X.Intercept column will be removed from dummy dataframe created for "fuelsystem" 
dummy <- dummy[,-1]

# Remove the original categorical "fuelsystem" column
# Combine columnwise dummy variables to the main data set,
car_final <- cbind(car_1_no_outliers[,-which(colnames(car_1_no_outliers) == "fuelsystem")], dummy)

#Repeat this procedure for all columns which has more than 2 levels of categorical variables.
#carbody
dummy <- data.frame(model.matrix( ~carbody, data = car_final))
dummy <- dummy[,-1]
car_final <- cbind(car_final[,-which(colnames(car_final)=="carbody")], dummy)

#drivewheel
dummy <- data.frame(model.matrix( ~drivewheel, data = car_final))
dummy <- dummy[,-1]
car_final <- cbind(car_final[,-which(colnames(car_final)=="drivewheel")], dummy)


#enginetype
dummy <- data.frame(model.matrix( ~enginetype, data = car_final))
dummy <- dummy[,-1]
car_final <- cbind(car_final[,-which(colnames(car_final)=="enginetype")], dummy)

#CarName
dummy <- data.frame(model.matrix( ~CarName, data = car_final))
dummy <- dummy[,-1]
car_final <- cbind(car_final[,-which(colnames(car_final)=="CarName")], dummy)

#cylindernumber
dummy <- data.frame(model.matrix( ~cylindernumber, data = car_final))
dummy <- dummy[,-1]
car_final <- cbind(car_final[,-which(colnames(car_final)=="cylindernumber")], dummy)

#symboling
dummy <- data.frame(model.matrix( ~symboling, data = car_final))
dummy <- dummy[,-1]
car_final <- cbind(car_final[,-which(colnames(car_final)=="symboling")], dummy)

#Car id is unique and therefore it of no statistical significance. Remove it.
car_final <- car_final[,-1]


View(car_final)

car_final$price <- car_final$price/100
head(car_final$price)

#'price' is the dependent variable and all others are independent variables.

# Segregate the set into training and test data set
# Set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car_final), 0.7*nrow(car_final))
# generate the train data set
train = car_final[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = car_final[-trainindices,]

#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)

# Check the summary of model. 
summary(model_1)

# Check if the correlation matrix givessome insight.
corrs = cor(car_final)
str(car_final)

View(corrs)

step <- stepAIC(model_1, direction="both")

# now we need to know our model equation so lets write the Step command here. 

step

# Construct the model based on the out put of the StepAIC function.

model_2<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carlength + carwidth + carheight + curbweight + enginesize + 
              boreratio + stroke + peakrpm + highwaympg + fuelsystem2bbl + 
              fuelsystem4bbl + fuelsystemmpfi + fuelsystemspfi + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
              enginetypeohcf + enginetypeohcv + CarNamebmw + CarNamedodge + 
              CarNamehonda + CarNameisuzu + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNameporsche + 
              CarNamerenault + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
              cylindernumberfive + cylindernumberfour + cylindernumbersix, 
            data = train)

# Summarize the model
summary(model_2)
vif(model_2)

# Removing the  stroke, enginetypeohcf, CarNamehonda, CarNamenissan because of insignificance
model_3<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carlength + carwidth + carheight + curbweight + enginesize + 
              boreratio + peakrpm + highwaympg + fuelsystem2bbl + 
              fuelsystem4bbl + fuelsystemmpfi + fuelsystemspfi + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
              enginetypeohcv + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNameplymouth + CarNameporsche + 
              CarNamerenault + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
              cylindernumberfive + cylindernumberfour + cylindernumbersix, 
            data = train)
summary(model_3)
vif(model_3)


# Removing CarNamerenault, CarNamemazda, enginesize due to insignificance
model_4<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carlength + carwidth + carheight + curbweight + 
              boreratio + peakrpm + highwaympg + fuelsystem2bbl + 
              fuelsystem4bbl + fuelsystemmpfi + fuelsystemspfi + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
              enginetypeohcv + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamemercury + 
              CarNamemitsubishi + CarNameplymouth + CarNameporsche + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
              cylindernumberfive + cylindernumberfour + cylindernumbersix, 
            data = train)

summary(model_4)
vif(model_4)


# Removing CarNamemercury of insignificance
model_5<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carlength + carwidth + carheight + curbweight + 
              boreratio + peakrpm + highwaympg + fuelsystem2bbl + 
              fuelsystem4bbl + fuelsystemmpfi + fuelsystemspfi + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
              enginetypeohcv + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamemitsubishi + CarNameplymouth + CarNameporsche + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
              cylindernumberfive + cylindernumberfour + cylindernumbersix, 
            data = train)

# check the accuracy of this model
summary(model_5)
vif(model_5)


# Removing Carwidth due to insignificance
model_6<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carlength + carheight + curbweight + 
              boreratio + peakrpm + highwaympg + fuelsystem2bbl + 
              fuelsystem4bbl + fuelsystemmpfi + fuelsystemspfi + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
              enginetypeohcv + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamemitsubishi + CarNameplymouth + CarNameporsche + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
              cylindernumberfive + cylindernumberfour + cylindernumbersix, 
            data = train)
summary(model_6)
vif(model_6)

# Removing carbodysedan due to insignificance
model_7<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carlength + carheight + curbweight + 
              boreratio + peakrpm + highwaympg + fuelsystem2bbl + 
              fuelsystem4bbl + fuelsystemmpfi + fuelsystemspfi + carbodyhardtop + 
              carbodyhatchback  + carbodywagon + enginetypel + 
              enginetypeohcv + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamemitsubishi + CarNameplymouth + CarNameporsche + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
              cylindernumberfive + cylindernumberfour + cylindernumbersix, 
            data = train)

summary(model_7)

# Removing fuelsystemspfi as it is low significance
model_8<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carlength + carheight + curbweight + 
              boreratio + peakrpm + highwaympg + fuelsystem2bbl + 
              fuelsystem4bbl + fuelsystemmpfi + carbodyhardtop + 
              carbodyhatchback  + carbodywagon + enginetypel + 
              enginetypeohcv + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamemitsubishi + CarNameplymouth + CarNameporsche + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
              cylindernumberfive + cylindernumberfour + cylindernumbersix, 
            data = train)
summary(model_8)
# Removing CarNamevolvo due to its low significance.
model_9<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carlength + carheight + curbweight + 
              boreratio + peakrpm + highwaympg + fuelsystem2bbl + 
              fuelsystem4bbl + fuelsystemmpfi + carbodyhardtop + 
              carbodyhatchback  + carbodywagon + enginetypel + 
              enginetypeohcv + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamemitsubishi + CarNameplymouth + CarNameporsche + 
              CarNametoyota + CarNamevolkswagen + 
              cylindernumberfive + cylindernumberfour + cylindernumbersix, 
            data = train)

summary(model_9)

# Removing peakrpm low significance
model_10<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carlength + carheight + curbweight + 
              boreratio +  highwaympg + fuelsystem2bbl + 
              fuelsystem4bbl + fuelsystemmpfi + carbodyhardtop + 
              carbodyhatchback  + carbodywagon + enginetypel + 
              enginetypeohcv + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamemitsubishi + CarNameplymouth + CarNameporsche + 
              CarNametoyota + CarNamevolkswagen + 
              cylindernumberfive + cylindernumberfour + cylindernumbersix, 
            data = train)

summary(model_10)

vif(model_10)

#Removing carlength as it has a high vif and high correlation with the one having the highest VIF curbweight
model_11<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
               carheight + curbweight + boreratio +  highwaympg + fuelsystem2bbl + 
               fuelsystem4bbl + fuelsystemmpfi + carbodyhardtop + 
               carbodyhatchback  + carbodywagon + enginetypel + 
               enginetypeohcv + CarNamebmw + CarNamedodge + 
               CarNameisuzu + CarNamemitsubishi + CarNameplymouth + CarNameporsche + 
               CarNametoyota + CarNamevolkswagen + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix, 
             data = train)

summary(model_11)
vif(model_11)

#Removing the wheelbase, highwaympg, carhardtop, carbodyhatchback, CarNameisuzu due to low significance
model_12<-lm(formula = price ~ aspiration + enginelocation + 
               carheight + curbweight + boreratio + fuelsystem2bbl + 
               fuelsystem4bbl + fuelsystemmpfi + carbodywagon + enginetypel + 
               enginetypeohcv + CarNamebmw + CarNamedodge + 
               CarNamemitsubishi + CarNameplymouth + CarNameporsche + 
               CarNametoyota + CarNamevolkswagen + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix, 
             data = train)
summary(model_12)
vif(model_12)

#removing boreratio as it is highly correlated with curbweight and showing high vif value with lower p value compared to curbweight,
#removing fuelsystemmpfi due to low significance
model_13<-lm(formula = price ~ aspiration + enginelocation + 
               carheight + curbweight + fuelsystem2bbl + 
               fuelsystem4bbl + carbodywagon + enginetypel + 
               enginetypeohcv + CarNamebmw + CarNamedodge + 
               CarNamemitsubishi + CarNameplymouth + CarNameporsche + 
               CarNametoyota + CarNamevolkswagen + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix, 
             data = train)
summary(model_13)

vif(model_13)

#removing  aspiration,carheight,fuelsystem2bbl,carnamedodge,CarNameplymouth, CarNamevolkswagen
model_14<-lm(formula = price ~  enginelocation + 
               curbweight + 
               fuelsystem4bbl + carbodywagon + enginetypel + 
               enginetypeohcv + CarNamebmw  + 
               CarNamemitsubishi + CarNameporsche + 
               CarNametoyota  + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix, 
             data = train)
summary(model_14)
vif(model_14)

#removing  enginetypeohcv due to high vif
model_15<-lm(formula = price ~  enginelocation + 
               curbweight + fuelsystem4bbl + carbodywagon + enginetypel + 
               CarNamebmw  + CarNamemitsubishi + CarNameporsche + 
               CarNametoyota  + cylindernumberfive + cylindernumberfour + cylindernumbersix, 
             data = train)
summary(model_15)
vif(model_15)

#removing  cylinderfive due to high vif and relatively lower significance.
model_16<-lm(formula = price ~  enginelocation + 
               curbweight + fuelsystem4bbl + carbodywagon + enginetypel + 
               CarNamebmw  + CarNamemitsubishi + CarNameporsche + 
               CarNametoyota   + cylindernumberfour + cylindernumbersix, 
             data = train)
summary(model_16)
vif(model_16)

#removing  fuelsystem4bbl,carNamemitsibushi due to lower significance
model_17<-lm(formula = price ~  enginelocation + 
               curbweight  + carbodywagon + enginetypel + 
               CarNamebmw   + CarNameporsche + 
               CarNametoyota   + cylindernumberfour + cylindernumbersix, 
             data = train)
summary(model_17)
vif(model_17)

#removing  carNameToyota due to lower significance
model_18<-lm(formula = price ~  enginelocation + 
               curbweight  + carbodywagon + enginetypel + 
               CarNamebmw   + CarNameporsche + 
               cylindernumberfour + cylindernumbersix, 
             data = train)
summary(model_18)
vif(model_18)

#removing  carNameToyota due to lower significance
model_19<-lm(formula = price ~  enginelocation + 
               curbweight  + carbodywagon + enginetypel + 
               CarNamebmw + 
               cylindernumberfour + cylindernumbersix, 
             data = train)
summary(model_19)
vif(model_19)
#Multiple R-squared:  0.9107,	Adjusted R-squared:  0.9059 


# Predict the house prices in the testing dataset
Predict_1 <- predict(model_18,test[,-1])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared

#rsquared = 0.87 shows that the test data is performing well.
# We can see that some important variables are: enginelocation, curbweight, carbodywagon, enginetypel,CarNamebmw, cylindernumberfour, cylindernumbersix
# This means that if we want the price to increase we need to mainly make suitable adjustments to the variables in the model to get good results.
