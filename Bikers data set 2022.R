# OBJECTIVE

# Design marketing strategies aimed at converting casual riders into annual members.
# In order to do that, however, the marketing analyst team needs to better understand 
# how annual members and casual riders differ, why casual riders would buy a membership,
# and how digital media could affect their marketing tactics.


# Source of data from Divvy trip data set.



# Installing and loading relevant packages used for analysing this data set

install.packages("janitor")
install.packages("lubridate")
install.packages("here")
install.packages("skimr")
install.packages("tidyverse")
install.packages("ggplot2")

library(tidyverse)
library(janitor)
library(lubridate)
library(here)
library(skimr)
library(dplyr)
library(ggplot2)

#importing the data set

X202201_divvy_tripdata <- read_csv("Downloads/Divvy-trip data set/202201-divvy-tripdata.csv")
X202202_divvy_tripdata <- read_csv("Downloads/Divvy-trip data set/202202-divvy-tripdata.csv")
X202203_divvy_tripdata <- read_csv("Downloads/Divvy-trip data set/202203-divvy-tripdata.csv")
X202204_divvy_tripdata <- read_csv("Downloads/Divvy-trip data set/202204-divvy-tripdata.csv")
X202205_divvy_tripdata <- read_csv("Downloads/Divvy-trip data set/202205-divvy-tripdata.csv")
X202206_divvy_tripdata <- read_csv("Downloads/Divvy-trip data set/202206-divvy-tripdata.csv")
X202207_divvy_tripdata <- read_csv("Downloads/Divvy-trip data set/202207-divvy-tripdata.csv")
X202208_divvy_tripdata <- read_csv("Downloads/Divvy-trip data set/202208-divvy-tripdata.csv")
X202209_divvy_tripdata <- read_csv("Downloads/Divvy-trip data set/202209-divvy-publictripdata.csv")
X202210_divvy_tripdata <- read_csv("Downloads/Divvy-trip data set/202210-divvy-tripdata.csv")
X202211_divvy_tripdata <- read_csv("Downloads/Divvy-trip data set/202211-divvy-tripdata.csv")
X202212_divvy_tripdata <- read_csv("Downloads/Divvy-trip data set/202212-divvy-tripdata.csv")

#Comparing different columns
 
colnames1 <- colnames(X202201_divvy_tripdata)
colnames2 <- colnames(X202202_divvy_tripdata)
colnames3 <- colnames(X202203_divvy_tripdata)
colnames4 <- colnames(X202204_divvy_tripdata)
colnames5 <- colnames(X202205_divvy_tripdata)
colnames6 <- colnames(X202206_divvy_tripdata)
colnames7 <- colnames(X202207_divvy_tripdata)
colnames8 <-colnames(X202208_divvy_tripdata)
colnames9 <- colnames(X202209_divvy_tripdata)
colnames10 <- colnames(X202210_divvy_tripdata)
colnames11 <- colnames(X202211_divvy_tripdata)
colnames12 <- colnames(X202212_divvy_tripdata)

results <- all.equal(colnames1,colnames2,colnames3,colnames4,colnames5,colnames6,colnames7,
                     colnames8,colnames9,colnames10,colnames11,colnames12)
print(results)

#Returns "TRUE" because all column names within this data set are the same 

# Merging the data sets to tone big data set

combined_2022_bikersdata <- rbind(X202201_divvy_tripdata,X202202_divvy_tripdata,X202203_divvy_tripdata,
                                  X202204_divvy_tripdata,X202205_divvy_tripdata,X202206_divvy_tripdata,
                                  X202207_divvy_tripdata,X202208_divvy_tripdata,X202209_divvy_tripdata,
                                  X202210_divvy_tripdata,X202211_divvy_tripdata,X202212_divvy_tripdata)

# Viewing and checking the new data set
colnames(combined_2022_bikersdata)
nrow(combined_2022_bikersdata)
dim(combined_2022_bikersdata)
head(combined_2022_bikersdata)
summary(combined_2022_bikersdata)

# Cleaning the data 

# Finding and removing duplicates 
duplicated_rows <- duplicated(combined_2022_bikersdata)
sum(duplicated_rows) # returned 0, no duplicates were found

# Renaming columns to make the data sets easier to understand

combined_2022_bikersdata_rename <- rename(combined_2022_bikersdata,trip_id = ride_id, bike_type = rideable_type,
                                          start_time = started_at, end_time = ended_at, user_type = member_casual)

# Checking for missing values

sum(is.na("trip_id"))
sum(is.na("bike_type"))
sum(is.na("start_time"))
sum(is.na("end_time"))
sum(is.na("user_type"))

# Adding columns listing the date, year, month and day

bikers_date_sorted <- combined_2022_bikersdata_rename %>% 
  mutate(date =format(as.Date(start_time))) %>% 
  mutate(year = format(as.Date(start_time), "%Y")) %>%
  mutate(month = format(as.Date(start_time), "%m")) %>%
  mutate(day = format(as.Date(start_time), "%d")) %>% 
  mutate(day_of_week = format(as.Date(start_time), "%A"))
  
head(bikers_date_sorted)
View(bikers_date_sorted)


# Calculating each trip duration and adding new column "ride_duration"
 bikers_ride_duration <- bikers_date_sorted %>%
   mutate(ride_duration = difftime(end_time, start_time))
View(bikers_ride_duration)

is.factor(bikers_ride_duration$ride_duration)
bikers_ride_duration <- bikers_ride_duration %>% 
  mutate(ride_duration = as.numeric(as.character(ride_duration)))
is.numeric (bikers_ride_duration$ride_duration)

# Removing insignificant rows 
bikers_clean <- bikers_ride_duration[!(bikers_ride_duration$ride_duration < 0),]

View(bikers_clean)
# Comparing the user types (Values are in seconds)
 aggregate(bikers_clean$ride_duration ~ bikers_clean$user_type, FUN = mean) # Average ride length 
 aggregate(bikers_clean$ride_duration ~bikers_clean$user_type, FUN = median) # Midpoint number
 aggregate(bikers_clean$ride_duration ~bikers_clean$user_type, FUN = max) # Longest ride
 aggregate(bikers_clean$ride_duration ~bikers_clean$user_type, FUN = min) # Shortest ride

 # Order days of the week before calculating average ride time by each day for different user types
 
 bikers_clean$day_of_week <- ordered(bikers_clean$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
 
 # Calculate average ride time by each day for different user types
 
 aggregate(bikers_clean$ride_duration ~ bikers_clean$user_type + bikers_clean$day_of_week, FUN = mean)
 
 # analyze ridership data by type and weekday
 bikers_clean %>% 
   mutate(weekday = wday(start_time, label = TRUE)) %>%  
   group_by(user_type, weekday) %>% 
   summarise(number_of_rides = n(),average_duration = mean(ride_duration)) %>% 
   arrange(user_type, weekday) 
 
 
 # visualize the total rides by members and casual users
 ggplot(data = bikers_clean) + 
   geom_bar(mapping = aes(x = user_type, fill = user_type)) +
   labs(title = "Number of Rides by Users")
 
 
 # visualize the number of rides by users based on bike type
 ggplot(data = bikers_clean) +
   geom_bar(mapping = aes(x = bike_type, fill = user_type), position = "dodge") +
   labs(title = "Number of Rides by Users based on Bike Type")
 
 
 # Visualize the number of rides by month
 bikers_clean %>% 
   mutate(month = month(start_time, label = TRUE)) %>%  
   group_by(user_type, month) %>% 
   summarise(number_of_rides = n()) %>% 	
   arrange(user_type, month)	%>% 
   ggplot() + geom_col(mapping = aes(x = month, y = number_of_rides, 
                                     fill = user_type), position = "dodge") +
   labs(title = "Number of Rides by Month")
 
 # Visualize average trip duration by days of week
 bikers_clean %>% 
   mutate(weekday = wday(start_time, label = TRUE)) %>% 
   group_by(user_type, weekday) %>% 
   summarise(average_duration = mean(ride_duration)) %>% 
   arrange(user_type, weekday)  %>% 
   ggplot() + geom_col(mapping = aes(x = weekday, y = average_duration, fill = user_type), position = "dodge") +
   labs(title = "Average Trip Duration by Days of Week")
 
 
 # STRATEGIES AND ACTIONS
 
 # This data proves the number of rides by members outnumbers the number of rides by casual members
 # To bridge this gap the following strategies could be put in place.
 
 #Offer incentives: One method to persuade casual riders to switch is by 
 #                  providing annual members with discounts, rewards, or unique benefits. 
 #                  This could be special access to particular bikes or rental sites,
 #                  reductions on bike rental rates, or a loyalty program with incentives for regular use.
 
 # Incentives should be targeted and promoted more during January, February and December
 # as they record the lowest number of rides.
 
 #Personalization: Bike sharing firms can establish personalized marketing efforts 
 #                 that are catered to individual riders by gathering information on 
 #                 their preferences and usage habits. 
 #                 This could take the form of push notifications that
 #                 highlight the advantages of becoming an annual member,
 #                 personalized discounts, or targeted email marketing. 
 
# Utilize social media: Bike sharing companies can communicate with 
 #                      riders through social media, increasing brand recognition and engagement.
 #                      Social media platforms such as Facebook, Instagram,
 #                      and Twitter can be used to create communities of riders,
 #                      share user-generated content, and promote special offers and discounts.
 
 
 # Docked bikes are the least used type of bikes because there is less preference for it.
 # Companies are advised to provide and maintain more classic bikes as its the most preffered.