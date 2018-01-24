setwd("~/Downloads")
require(gridExtra)
require("lubridate")
require(ggplot2)
require(gridExtra)

uber<-read.csv("Uber Request Data.csv")
uber$Request.timestamp<-as.character(uber$Request.timestamp)
uber$Drop.timestamp<-as.character(uber$Drop.timestamp)

#Checking which of the columns have na values.

uber[which(is.na(uber$Drop.timestamp)),] #NA values are present 
uber[which(is.na(uber$Request.timestamp)),] #no NA values
uber[which(is.na(uber$Request.id)),] #no NA values
uber[which(is.na(uber$Pickup.point)),] #no NA values
uber[which(is.na(uber$Driver.id)),] # There are NA values.
uber[which(is.na(uber$Status)),] #no NA values

#Summary of data quality issues:
# NA values are present in Drop.timestamp and Driver.id which we replaced with approprioate values.
# time stamps have the value of integer. Need to convert to POSIXlt/POSIXct type
# some dates are in different format with / as separator and in some dates have missing seconds value.
# Need to accomodate those using parse_date_time function as shown.
vec_req_datetime<-sapply(uber$Request.timestamp,function(y) gsub("/","-",y))
uber$Request.timestamp<-parse_date_time(vec_req_datetime,orders=c("dmy HMS"),truncated=3,tz="Asia/Kolkata")
requestDay<-wday(as.Date(uber$Request.timestamp),label = TRUE)
#Extracting just the hour to get a feel of traffic during different times
requestHour<-format(uber$Request.timestamp, "%H") 
vec_drop_datetime<-sapply(uber$Drop.timestamp,function(y) gsub("/","-",y))
uber$Drop.timestamp<-parse_date_time(vec_drop_datetime,orders=c("dmy HMS"),truncated=3,tz="Asia/Kolkata")

#Derived metrics
#We need to calculate duration as a derived metric to see if that contributes to the gaps.
# we also need to separate time and date, and come up with the time slots.
# We are fixing the time slots as: 
# Morning(5AM-10AM), afternoon(11AM-4PM), evening(5PM-11PM), night(12PM-4AM)

getTimeSlot<-function(time){
if(time>=5 && time<=10){return("Morning")}
else if (time>10 && time<17) {return("Afternoon")}
else if (time>=17 && time<=22) {return("Evening")}
else {return("Night")}}
timeSlot<-sapply(as.integer(requestHour),getTimeSlot)
timeSlot<-factor(timeSlot,levels=c("Morning","Afternoon","Evening","Night"))
duration<-uber$Drop.timestamp-uber$Request.timestamp

# Final data frame.
uber_proper<-cbind(uber,requestDay,requestHour,timeSlot,duration)

#From below plots it is clear that the requests from airport in the evening is very high and requests from city to airport in the morning is very high
hourly_requests<-ggplot(uber_proper,aes(x=uber_proper$requestHour,fill=uber_proper$Pickup.point))+geom_bar()+xlab("Request hour")+ ylab("Request count")+scale_fill_discrete(name="Pickup location")
slot_requests<-ggplot(uber_proper,aes(x=uber_proper$timeSlot,fill=uber_proper$Pickup.point))+geom_bar()+xlab("Time Slot")+ylab("Number of requests")+scale_fill_discrete(name="Pickup location")+scale_x_discrete(labels=c("Morning\n[5.00-10.00]","Afternoon\n[11.00-16.00]","Evening\n[17.00-22.00]","Night\n(23.00-4.00)"))
grid.arrange(hourly_requests,slot_requests,nrow=2)

city_subset<-subset(uber_proper,uber_proper$Pickup.point=="City")
airport_subset<-subset(uber_proper,uber_proper$Pickup.point=="Airport")

#Univariate analysis:
summary(uber_proper$Pickup.point)
summary(airport_subset$Status)
summary(city_subset$Status)

summary(as.numeric(uber_proper$requestHour),na.rm = T)
#There are overall 14 requests per hour on average.
summary(uber_proper$Status)
#Its interesting to see No cars available(2650) is close to number of trips completed(2831), which means there is a huge deficit in cars during the required time.
summary(as.numeric(uber_proper$duration))
# when we see the spread, mean, median of duration we can say that duration does not vary much through out the day.
# Below plot shows that there is no significant difference in duration of travel during different times of the day. So the traffic is not affecting the duration 
ggplot(city_subset,aes(as.factor(timeSlot),as.numeric(duration)))+geom_boxplot()+xlab("Time slot")+ylab("Duration of travel")
# and we can conclude the time taken during peak hours is not the reason for cancellations from drivers.
#for city trips lets see the spread of different trip status through out the day:
city_spread<-ggplot(city_subset,aes(as.factor(Status),as.numeric(requestHour)))+geom_boxplot()+xlab("Status")+ylab("Request hour")+ggtitle("Requests from city")
#we can see from the above box plot for trips from city that the number of successful trips are spread through out the day 
#but cancelled is more concentrated towards the morning hours.
airport_spread<-ggplot(airport_subset,aes(as.factor(Status),as.numeric(requestHour)))+geom_boxplot()+xlab("Status")+ylab("Request hour")+ggtitle("Requests from Airport")
#similarly for trips from airport, unavailability is more concentrated towards the evening hours.
grid.arrange(city_spread,airport_spread,nrow=2)
#Segmented analysis:
# Whats the percentage of trips successful cancelled and unavailable in the 2 pick up points during different timeslots identified?
# First lets see for city:
aggregate(city_subset$Status,by=list(city_subset$timeSlot,city_subset$Status),FUN=length)
#What stands out is: Out of 1845 requests in the morning, 873 are cancelled(47%) and 437(23%) didnt have cars available.
#Now lets see for airport:
aggregate(airport_subset$Status,by=list(airport_subset$timeSlot,airport_subset$Status),FUN=length)
# What stands out is: Out of 1983 requests in the evening(Which is the timeslot with highest number of requests)
# 1421 have no cars available(71.5%) which is a pretty huge demand unsatisfied.
# Overall, including cancellations and unavailability 77% of the requests are not satisfied. 
#We will look into the exact numbers of supply demand gap little later. Lets plot the graphs to depict the above.

#From the below plot we see that cancellation is very high for requests from city in the morning and no cars available is very high for requests from airport in the evening.
airport_time_status<-ggplot(airport_subset, aes(x = timeSlot,fill =factor(Status))) + geom_bar(alpha = 0.7,  position = position_dodge(width=0.7))+xlab("Time slots")+ylab("Requests from Airport")+ggtitle("Airport")+scale_fill_discrete(name="Status")
city_time_status<-ggplot(city_subset, aes(x = timeSlot,fill =factor(Status))) + geom_bar(alpha = 0.7, position = position_dodge(width=0.7)) +xlab("Time slots")+ylab("Requests from City")+ggtitle("City")+scale_fill_discrete(name="Status")
grid.arrange( city_time_status,airport_time_status, nrow=2)

# Lets see how the trips vary based on different dates:
city_date<-ggplot(city_subset,aes(x=requestDay,fill=Status))+geom_bar()+xlab("")+ ylab("Total requests from city")+scale_fill_hue(l=75)+ggtitle("City")
airport_date<-ggplot(airport_subset,aes(x=requestDay,fill=Status))+geom_bar()+xlab("Request day")+ ylab("Total requests from Airport")+scale_fill_hue(l=75)+ggtitle("Airport")
grid.arrange( city_date, airport_date, nrow=2)
#As expected since a sunday is weekend, we can see low number of requests but we still see the percentage of no cars available and cancelled as same as weekdays. Greater incentive to drivers for weekend, may increase the availability of cars on weekend. 
#More discounts on weekend will encourage the passengers to travel on a weekend which can ease the demand on weekdays.

#Analysing the gap between supply and demand.
#Lets see the demand for each timeslot for 2 pickup points: 

demand_city<-aggregate(city_subset$Pickup.point,by=list(city_subset$timeSlot),FUN=length)
#City total demand
#   Morning 1845
# Afternoon  578
#   Evening  663
#     Night  421
demand_airport<-aggregate(airport_subset$Pickup.point,by=list(airport_subset$timeSlot),FUN=length)
#airport total demand
#   Morning  501
# Afternoon  403
#   Evening 1983
#     Night  351

#Now lets see the successful supply at both pickup points during different timeslots:
completed_airport_subset<-subset(airport_subset,airport_subset$Status=="Trip Completed")
completed_city_subset<-subset(city_subset,city_subset$Status=="Trip Completed")

#Supply at city
supply_city<-aggregate(completed_city_subset$Pickup.point,by=list(completed_city_subset$timeSlot),FUN=length)
#   Morning 535
# Afternoon 332
#   Evening 485
#     Night 152
supply_airport<-aggregate(completed_airport_subset$Pickup.point,by=list(completed_airport_subset$timeSlot),FUN=length)
#Supply at airport
#   Morning 435
# Afternoon 274
#   Evening 453
#     Night 165

#Percentage of unsatisfied demand can be defined as the gap in supply and demand and that can be calculated as follows:
# percentage of unsatisfied demand:- ((demand in each timeslot - supply in each timeslot)/demand in each timeslot)*100
gap<-data.frame(demand_city$Group.1,((demand_city$x-supply_city$x)/demand_city$x)*100,((demand_airport$x-supply_airport$x)/demand_airport$x)*100)
colnames(gap)<-c("Time slot","Gap %(City)", "Gap %(Airport)")
View(gap)

#Timeslot      Gap from City        Gap from Airport
#Morning       71%                  13%
#Afternoon     42.5%                32%
#Evening       26.8%                77%
#Night         64%                  53%

# Visualizing supply and demand gap for hourly basis. the reddish area shows the 'gap'
city_gap_hourly<-ggplot(city_subset)+geom_area(aes(requestHour,fill=Pickup.point),stat="count",group=1)+geom_area(data = subset(city_subset,city_subset$Status=="Trip Completed"),aes(requestHour,fill=Status),stat="count",group=1)+scale_fill_discrete(labels=c("Total Requests","Trip Completed"),name="From City:\nDemand-Supply")+xlab("Hour of request")+ylab("Requests from City")
airport_gap_hourly<-ggplot(airport_subset)+geom_area(aes(requestHour,fill=Pickup.point),stat="count",group=1)+geom_area(data = subset(airport_subset,airport_subset$Status=="Trip Completed"),aes(requestHour,fill=Status),stat="count",group=1)+scale_fill_discrete(labels=c("Total Requests","Trip Completed"),name="From Airport:\nDemand-Supply")+xlab("Hour of request")+ylab("Requests from Airport")
grid.arrange(city_gap_hourly,airport_gap_hourly,nrow=2)

# Visualizing based on time slots, again the red area shows the gap..
city_gap_slot<-ggplot(city_subset)+geom_bar(aes(timeSlot,fill=Pickup.point),stat="count",group=1)+geom_bar(data = subset(city_subset,city_subset$Status=="Trip Completed"),aes(timeSlot,fill=Status),stat="count",group=1)+scale_fill_discrete(labels=c("Total Requests","Trip Completed"),name="From City:\nDemand-Supply")+xlab("Request timeslot")+ylab("Requests from City")
airport_gap_slot<-ggplot(airport_subset)+geom_bar(aes(timeSlot,fill=Pickup.point),stat="count",group=1)+geom_bar(data = subset(airport_subset,airport_subset$Status=="Trip Completed"),aes(timeSlot,fill=Status),stat="count",group=1)+scale_fill_discrete(labels=c("Total Requests","Trip Completed"),name="From City:\nDemand-Supply")+xlab("Request timeslot")+ylab("Requests from Airport")
grid.arrange(city_gap_slot,airport_gap_slot,nrow=2)
