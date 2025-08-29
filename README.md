# SALES-ANALYSIS-R--LANGUAGE-
GENERATED INSIGHTS WHICH HELPED IN SALES OPTIMIZATION OF A BIKE COMPANY 
#importing the data set
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(janitor)
library(lubridate)
library(readr)
library(readxl)
library(data.table)
library(ggmosaic)
library(lubridate)


#Assigning my data to a variable for reference 
FIRST_DATA = Divvy_Trips_2019_Q1_Divvy_Trips_2019_Q1
SECOND_DATA = Divvy_Trips_2020_Q1_Divvy_Trips_2020_Q1

### Clean the Variable Names 
clean_names(FIRST_DATA)
clean_names(SECOND_DATA)

### created an artificial id for merging  purpose
FIRST_DATA$ID = 1:nrow(FIRST_DATA)
SECOND_DATA$ID = 1:nrow(SECOND_DATA)
View(FIRST_DATA)
View(SECOND_DATA)

#merge the two Dataset
Main_Data <- merge(FIRST_DATA, SECOND_DATA, by = "ID")

#Check for Nulls  NO NULLS 
colSums(is.na(Main_Data))

#Drop Nulls & Remove Duplicates
Main_Data = na.omit(Main_Data)
Main_Data = distinct(Main_Data)

#Making Sure Columns are in date time format 
Main_Data$started_at = as.POSIXct(Main_Data$started_at, format = "%Y-%m-%d %H:%M: %S")
Main_Data$ended_at = as.POSIXct(Main_Data$ended_at, format = "%Y-%m-%d %H:%M: %S")

#Calculate the ride length 
Main_Data$Ride_Length = Main_Data$ended_at - Main_Data$started_at

#Calculate the ride length in hours 
Main_Data$Ride_Length_hr = as.numeric(difftime(Main_Data$ended_at, Main_Data$started_at, units = "hours"))
View(Main_Data)

# Make sure started_at is a Date or POSIXct type
Main_Data$started_at <- as.POSIXct(Main_Data$started_at, format="%Y-%m-%d %H:%M:%S")

# Create a column for days of the week
Main_Data$day_of_week <- weekdays(Main_Data$started_at)
View(Main_Data)

summary(Main_Data$Ride_Length)
summary(Main_Data$Ride_Length_hr)


#Number Of Rides per day (The highest is Tuesday while the least is Saturday) 
ggplot(Main_Data, aes(x = day_of_week)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Number of Rides per Day",
       x = "Day of Week",
       y = "Number of Rides") +
  theme_minimal()

Main_Data %>% distinct(member_casual) # distinct members 

#Checking the ride length between ANNual Members and Casuals (Casual>Annual)
avg_ride_type = Main_Data %>% 
  group_by(member_casual) %>% 
  summarise(avg_ride_length = mean(Ride_Length_hr, na.rm = TRUE))

#Checking the AVG trip duration  between ANNual Members and Casuals (Annual>Casual)
avg_trip_duration = Main_Data %>% 
  group_by(member_casual) %>% 
  summarise(avg_trip_duration = mean(tripduration, na.rm = TRUE))

#This Mean Casual Spend more MINUTES or time on the Road While Members Spend less time but Work often 
#Maybe Casuals Use their bike for leisure while Annual Members use theirs For work,School.


#Average Ride length per day 
avg_ride_length_per_day <- Main_Data %>%
  group_by(day_of_week) %>%
  summarise(avg_ride_length = mean(Ride_Length_hr, na.rm = TRUE)) %>%
  arrange(match(day_of_week, 
                c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))
avg_ride_length_per_day

ggplot(avg_ride_length_per_day, aes(x = day_of_week, y = avg_ride_length))+
  geom_col(fill = "darkblue") +
  labs(title = "Average Ride length Per day ",
       x = "DAYS",
       y = "Ride Length (HOUR)")+
        theme_minimal( )


#Saturdays and Sundays has the highest Ride length Daily Confirming that our Highest ride length are from those 
#using the bike for leisure 

#Average trip duration  per day (Thursday highest)
avg_trip_duration_per_day <- Main_Data %>%
  group_by(day_of_week) %>%
  summarise(avg_trip_duration= mean(tripduration, na.rm = TRUE)) %>%
  arrange(match(day_of_week, 
                c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))

avg_trip_duration_per_day
ggplot(avg_trip_duration_per_day, aes(x = day_of_week, y = avg_trip_duration))+
  geom_col(fill = "darkblue") +
  labs(title = "Average Trip Duration Per day ",
       x = "DAYS",
       y = "Trip Duration")+
  theme_minimal( )

#Plot of Gender VS Customer_type
ggplot(Main_Data, aes(x = gender, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Rider Type by Gender",
       x = "Gender",
       y = "Number of Riders",
       fill = "Rider Type") +
  theme_minimal()

#📊 Insights from the Bike Share Analysis (R Project)
#1.	Ride Duration
#•	Casual riders take longer trips on average than annual members.
#•	Annual members tend to have shorter, consistent ride lengths.
#2.	Rider Purpose
#•	Annual members mostly use bikes for commuting (short, weekday rides).
#•	Casual riders use bikes more for leisure and exploration, which explains the longer ride times.
#3.	Day-of-Week Patterns
#•	Casual riders are most active on weekends (Saturday & Sunday).
#•	Annual members dominate weekdays, aligning with work commutes.
#4.	Usage Patterns
#•	Casual riders contribute heavily to occasional spikes in ride duration, while members show steady daily usage.
#•	This suggests different motivations and behaviors between the two groups.



🎯 Recommendations for the Bike Company
#1.	Weekend Promotions for Casual Riders
#•	Offer discounted annual memberships or trial passes on Saturdays and Sundays when casual usage peaks.
#2.	Targeted Marketing Campaigns
#•	Use personalized messages like: “You rode 8 times this month. An annual membership saves you more!”
#•	Highlight cost savings + convenience to casual riders who ride often.
#3.	Commuter Benefits for Members
#•	Add perks for annual members, such as faster checkouts, rewards for frequent commuting, or partner discounts with workplaces.
#4.	Leverage Seasonality
#•	If casual riders peak in summer, introduce seasonal-to-annual upgrade offers to convert them when interest is highest.
#5.	Data-Driven Segmentation
#•	Continue analyzing ride length, time of day, and frequency to build targeted rider personas (e.g., “Weekend Explorer” vs. “WeekdayCommuter”).
