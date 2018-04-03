#Load library
library(dplyr)
library(ggplot2)
library(stringr)

#Load Dataset
uber <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)

######################################################################################
############## Data Cleaning
#####################################################################################

## 1: Check for missing values
any(is.na(uber))

apply(uber, 2, function(x)
  any(is.na(x)))


#driver id and drop timestamp contains NA which is OK as they represent no driver assigned requests

## 2: Timestamp columns are in different format
# Convert both timestamp columns to one format as to convert them to date time "%d/%m/%Y %H:%M"

uber$Request.timestamp <- gsub("-", "/", uber$Request.timestamp)
uber$Drop.timestamp <- gsub("-", "/", uber$Drop.timestamp)

#Add default Seconds wherever missing
uber$Request.timestamp <-
  sapply(
    uber$Request.timestamp,
    FUN = function(x) {
      ifelse(str_count(x, "[:]") == 1, paste(x, ":00", sep = ""), x)
    }
  )
uber$Drop.timestamp <-
  sapply(
    uber$Drop.timestamp,
    FUN = function(x) {
      ifelse(str_count(x, "[:]") == 1, paste(x, ":00", sep = ""), x)
    }
  )

#Convert to POSIXct
uber$Request.timestamp <-
  as.POSIXct(uber$Request.timestamp, format = "%d/%m/%Y %H:%M:%S")
uber$Drop.timestamp <-
  as.POSIXct(uber$Drop.timestamp, format = "%d/%m/%Y %H:%M:%S")

## 3: Add derived columns for further analysis

#Additional columns from requested timestamp
uber$ReqHour <- format(uber$Request.timestamp, "%H")
uber$ReqDate <- as.Date(uber$Request.timestamp)
uber$ReqDay <- format(uber$Request.timestamp, "%d")
uber$ReqMonth <- format(uber$Request.timestamp, "%m")
uber$ReqWeekday <- weekdays(uber$Request.timestamp)

#Additional columns from drop timestamp
uber$DropHour <- format(uber$Drop.timestamp, "%H")
uber$DropDate <- as.Date(uber$Drop.timestamp)
uber$DropDay <- format(uber$Drop.timestamp, "%d")
uber$DropMonth <- format(uber$Drop.timestamp, "%m")
uber$DropWeekday <- weekdays(uber$Drop.timestamp)

#Ride time for completed rides
uber$RideTime <- uber$Drop.timestamp - uber$Request.timestamp

uber$RideType <-
  sapply(
    uber$Pickup.point,
    FUN = function(x) {
      ifelse(x == "City", "City to Airport", "Airport to City")
    }
  )

#Divide the day into diffferent timeslots based on requests raised
#Plot the frequency of requests at each hour to define timeslots
ggplot(uber, aes(x = ReqHour)) +
  geom_bar(stat = "count") +
  labs(title = "Spread of requests throughout the day", x = "Cab Requested Time", y =
         "No Of Rides")

TimeSlot <- function(time) {
  if (time %in% c("22", "23", "00", "01", "02", "03", "04")) {
    slot <- "Night"
  } else if (time %in% c("05", "06", "07", "08", "09")) {
    slot <- "Early Morning"
  } else if (time %in% c("10", "11", "12")) {
    slot <- "Late Morning"
  } else if (time %in% c("13", "14", "15", "16")) {
    slot <- "Afternoon"
  } else
    slot <- "Evening"
  return(slot)
}

#Lets change the order of levels to correctly view them on graphs
uber$TimeSlot <- as.factor(sapply(uber$ReqHour, FUN = TimeSlot))

uber$TimeSlot <-
  factor(
    uber$TimeSlot,
    levels = c("Early Morning", "Late Morning", "Afternoon", "Evening", "Night")
  )

levels(uber$TimeSlot)

######################################################################################
############## Analysis
#####################################################################################

#request id is the primary key

#Trips done by drivers
length(unique(uber$Request.id))

length(unique(uber$Driver.id)) - 1
#Removing the rows that contain NA there are 300 drivers

#Hourly Ride Requests
#The plot gives us a picture of number of rides at each hour starting at the two given pickup points
ggplot(uber, aes(x = ReqHour, fill = RideType)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Frequency of Requests", x = "Cab Requested Time", y = "No Of Rides")

#Ride request demand thrroughout timeslots
#The plot gives us a picture of number of rides at our divided timeslots starting at the two given pickup points defined
#The plot gives a concise depth of problem with numbers.
ggplot(uber, aes(x = TimeSlot, fill = RideType)) +
  geom_bar(stat = "count", position = "dodge") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(0.9),
    vjust = -0.4
  )
labs(title = "Frequency of Requests", x = "Cab Requested Time", y = "No Of Rides")

#Trips completed throughout the day
#Lets drill down at the above plot for only non completed trips for different status
ggplot(subset(uber, Status != "Trip Completed"),
       aes(x = TimeSlot, fill = Status)) +
  geom_bar(stat = "count", position = "dodge") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(0.9),
    vjust = -0.4
  )
labs(title = "Completed Rides Analysis", x = "Cab Requested Time", y =
       "No Of Rides")

#Result: There is a peak in requests b/w 5 to 9 am early morning and 5 to 9 pm in the evening

#Rides Status analysis
ggplot(uber, aes(x = Status, fill = RideType)) +
  geom_bar(stat = "count") +
  labs(title = "Frequency of Request Status", x = "Cab Requested Time", y =
         "No Of Rides")

#Percentage of requests raised
uber %>%
  group_by(Status) %>%
  summarise(NoOfRequests = n()) %>%
  mutate(PercentOfTotal = NoOfRequests / nrow(uber) * 100)

#Percentage of requests raised from each type of pickup points
uber %>%
  group_by(RideType, Status) %>%
  summarise(NoOfRequests = n()) %>%
  mutate(PercentOfTotal = NoOfRequests / nrow(uber) * 100)

#Cab requests
ggplot(uber, aes(x = ReqHour, fill = Status)) +
  geom_bar(stat = "count") +
  labs(title = "Frequency of requests with different status", x = "Cab Requested Time", y =
         "No Of Rides")

#Average ride time for Airport to city and City to Airport
AvgRideTime <- uber %>%
  filter(Status == "Trip Completed") %>%
  group_by(RideType) %>%
  summarise(AverageRideTime = round(mean(Drop.timestamp - Request.timestamp)))

AvgRideTime
#Result: Average time is nearly same for both type of rides

#Time taken to complete the ride througout the days
#Looking at the time taken to reach airport and vice versa at each hour
Plot.AverageRideTime <-
  ggplot(uber, aes(
    x = ReqHour,
    y = as.numeric(RideTime),
    fill = RideType
  )) +
  geom_boxplot() +
  geom_hline(
    aes(yintercept = AverageRideTime),
    subset(AvgRideTime, RideType == "City to Airport"),
    size = 1,
    colour = "blue"
  ) +
  labs(title = "Time taken by Rides", x = "Cab Requested Hour", y = "Ride Time")

#Time taken to complete the ride througout the weekdays
#
ggplot(uber, aes(
  x = TimeSlot,
  y = as.numeric(RideTime),
  fill = RideType
)) +
  geom_boxplot() +
  labs(title = "Time taken by Rides", x = "Cab Requested Time Slot", y =
         "Ride Time")

#Time taken to complete the ride througout the weekdays
ggplot(uber, aes(x = ReqWeekday, y = RideTime, fill = RideType)) +
  geom_boxplot() +
  labs(title = "Time taken by Rides throughout the weekdays", x = "Cab Requested day", y =
         "Ride Time")


######################################################################################
############## Most pressing problems for Uber
#####################################################################################

#Request that dont get completed
uber %>%
  filter(Status != "Trip Completed" &
           RideType == "City to Airport") %>%
  group_by(Status, TimeSlot) %>%
  summarise(NoOfRequests = n()) %>%
  arrange(desc(NoOfRequests)) %>%
  head(1)

uber %>%
  filter(Status != "Trip Completed" &
           RideType == "Airport to City") %>%
  group_by(Status, TimeSlot) %>%
  summarise(NoOfRequests = n()) %>%
  arrange(desc(NoOfRequests)) %>%
  head(1)

#Plotting the above 2
#Trips not completed for various ride types
ggplot(subset(uber, Status != "Trip Completed"),
       aes(x = TimeSlot, fill = RideType)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Trips Not Completed" , x = "Cab Requested at", y = "No of Trips Not completed")

#Trips not completed for different type of status
ggplot(subset(uber, Status != "Trip Completed"),
       aes(x = TimeSlot, fill = Status)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Trips Not Completed" , x = "Cab Requested at", y = "No of Trips Not completed")

#peak hours when requests are not completed are 5 to 9 in the morning and 5 to 9 in the evening


#The above query and the plot identifies 2 issues
# 1: Most of the "No Cars Available" happen for "Airport to city" rides during the evening (5 to 9 pm)
# 2: Most of the request cancellations occur for "City to airport" rides during the early morning (5 to 9 am)

######################################################################################
############## Gap between supply and demand and show the same using plots
######################################################################################

#Aggregating Supply and demands

#Demand = No of total request for each status and type of ride
#Supply = No of trips completed
#Gap = Demand - Supply

#Create a subset for Demand
Gap.Demand <- uber %>%
  filter(TimeSlot %in% c("Evening", "Early Morning")) %>%
  group_by(RideType, Status) %>%
  summarise(Demand = n())

#Create a subset for Supply
Gap.Supply <- uber %>%
  filter(
    Status == "Trip Completed" &
      is.na(Driver.id) == F &
      TimeSlot %in% c("Evening", "Early Morning")
  ) %>%
  group_by(RideType, Status) %>%
  summarise(Supply = n())

#Merging the Demand and Supply
Gap.SupplyDemand <- Gap.Demand %>%
  full_join(Gap.Supply, by = c("RideType" = "RideType", "Status" = "Status")) %>%
  mutate(Supply = ifelse(is.na(Supply) == T, 0, Supply))

Gap.SupplyDemand

ggplot(Gap.SupplyDemand, aes(x = RideType, y = Demand, fill = Supply)) +
  geom_bar(stat = "identity") +
  labs(title = "Demand vs Supply" , x = "Ride Type", y = "Supply and Demand") +
  scale_fill_gradient(low = "green", high = "red")

##Gap analysis for City to Airport rides between 5 to 9 am
## 1: "High number of cancellations" for "City to airport" rides in early morning (5 to 9 am)

#Total gap during the early morning 5 to 9 am time
Gap.SupplyDemand %>%
  filter(RideType == "City to Airport") %>%
  summarise(Demand = sum(Demand), Supply = sum(Supply)) %>%
  mutate(Gap = Demand - Supply,
         Percent_Of_Rides_Completed = round(Supply / Demand * 100))

#Looking at the time taken for rides for City to Airport
ggplot(
  subset(uber, RideType == "City to Airport" &
           Status == "Trip Completed"),
  aes(
    x = TimeSlot,
    y = as.numeric(RideTime),
    fill = RideType
  )
) +
  geom_boxplot() +
  geom_hline(
    aes(yintercept = AverageRideTime),
    subset(AvgRideTime, RideType == "City to Airport"),
    size = 1,
    colour = "blue"
  ) +
  labs(title = "Average time taken for cabs", x = "Cab Requested Time", y =
         "Ride Time")

##Gap analysis for City to Airport rides between 5 to 9 am
## 2: "High number of no cars available" for "Airport to City" rides in Evening (5 to 9 pm)

#Total gap during the evening 5 to 9 pm time
Gap.SupplyDemand %>%
  filter(RideType == "Airport to City") %>%
  summarise(Demand = sum(Demand), Supply = sum(Supply)) %>%
  mutate(Gap = Demand - Supply,
         Percent_Of_Rides_Completed = round(Supply / Demand * 100))

#Export to csv for Tableau
write.csv(uber, "UberExport.csv", row.names = FALSE)
write.csv(Gap.SupplyDemand, "UberSupplyDemand.csv", row.names = FALSE)
