library(tidyverse)
library(lubridate)
library(ggplot2)
getwd()
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
read_csv("Divvy_Trips_2019_Q1.csv")
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
View(q1_2019)
read_csv("Divvy_Trips_2019_Q2.csv")
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
read_csv("Divvy_Trips_2019_Q3.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
read_csv("Divvy_Trips_2019_Q4.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")

read_csv("Divvy_Trips_2020_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

read_csv("202004-divvy-tripdata.csv")
q2_04 <- read_csv("202004-divvy-tripdata.csv")
read_csv("202005-divvy-tripdata.csv")
q2_05 <- read_csv("202005-divvy-tripdata.csv")
read_csv("202006-divvy-tripdata.csv")
q2_06 <- read_csv("202006-divvy-tripdata.csv")
?bind_rows
bind_rows(q2_04, q2_05, q2_06)
q2_2020 <- bind_rows(q2_04, q2_05, q2_06)
View(q2_2020)

read_csv("202007-divvy-tripdata.csv")
q3_07 <- read_csv("202004-divvy-tripdata.csv")
read_csv("202008-divvy-tripdata.csv")
q3_08 <- read_csv("202008-divvy-tripdata.csv")
read_csv("202009-divvy-tripdata.csv")
q3_09 <- read_csv("202009-divvy-tripdata.csv")
bind_rows(q3_07, q3_08, q3_09)
q3_2020 <- bind_rows(q3_07, q3_08, q3_09)
View(q3_2020)

read_csv("202010-divvy-tripdata.csv")
q4_10 <- read_csv("202010-divvy-tripdata.csv")
read_csv("202011-divvy-tripdata.csv")
q4_11 <- read_csv("202011-divvy-tripdata.csv")
read_csv("202012-divvy-tripdata.csv")
q4_12 <- read_csv("202012-divvy-tripdata.csv")
colnames(q4_10)
colnames(q4_11)
spec(q4_10)
spec(q4_11)
spec(q4_12)
bind_rows(q4_10, q4_11, q4_12)
mutate(q4_12, start_station_id = as.double(start_station_id))
spec(q4_12)
View(q4_12)
bind_cols(q4_10, q4_11, q4_12)
rbind(q4_10, q4_11, q4_12)
q4_2020 <- rbind(q4_10, q4_11, q4_12)
View(q4_2020)

colnames(q1_2019)
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)

rename(q1_2019
       ,ride_id = trip_id
       ,rideable_type = bikeid 
       ,started_at = start_time  
       ,ended_at = end_time  
       ,start_station_name = from_station_name 
       ,start_station_id = from_station_id 
       ,end_station_name = to_station_name 
       ,end_station_id = to_station_id
       ,member_casual = usertype)

q1_2019 <- rename(q1_2019
             ,ride_id = trip_id
             ,rideable_type = bikeid 
             ,started_at = start_time  
             ,ended_at = end_time  
             ,start_station_name = from_station_name 
             ,start_station_id = from_station_id 
             ,end_station_name = to_station_name 
             ,end_station_id = to_station_id
             ,member_casual = usertype)

View(q1_2019)

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"
                   ,gender = "Member Gender"
                   ,birthyear = "05 - Member Details Member Birthday Year"
                   ,tripduration = "01 - Rental Details Duration In Seconds Uncapped"
                   ))
View(q2_2019)
rename(q2_2019
       ,gender = "Member Gender"
       ,birthyear = "05 - Member Details Member Birthday Year"
       ,tripduration = "01 - Rental Details Duration In Seconds Uncapped")
q2_2019 <- rename(q2_2019
                  ,gender = "Member Gender"
                  ,birthyear = "05 - Member Details Member Birthday Year"
                  ,tripduration = "01 - Rental Details Duration In Seconds Uncapped")
View(q2_2019)

q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype)
View(q3_2019)  

q4_2019 <- rename(q4_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid 
                  ,started_at = start_time  
                  ,ended_at = end_time  
                  ,start_station_name = from_station_name 
                  ,start_station_id = from_station_id 
                  ,end_station_name = to_station_name 
                  ,end_station_id = to_station_id 
                  ,member_casual = usertype)
View(q4_2019)
str(q1_2020)
View(q1_2020)

colnames(q1_2020)
colnames(q2_2020)
colnames(q3_2020)
colnames(q4_2020)

str(q1_2020)
str(q2_2020)
str(q3_2020)
str(q4_2020)

str(q1_2019)
str(q2_2019)
str(q3_2019)
str(q4_2019)

mutate(q1_2019, ride_id = as.character(ride_id)
       , rideable_type = as.character(rideable_type)
       , start_station_id = as.character(start_station_id)
       , end_station_id = as.character(end_station_id))
q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id)
                  , rideable_type = as.character(rideable_type)
                  , start_station_id = as.character(start_station_id)
                  , end_station_id = as.character(end_station_id))
str(q1_2019)

mutate(q2_2019, ride_id = as.character(ride_id)
       , rideable_type = as.character(rideable_type)
       , start_station_id = as.character(start_station_id)
       , end_station_id = as.character(end_station_id))
q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id)
                  , rideable_type = as.character(rideable_type)
                  , start_station_id = as.character(start_station_id)
                  , end_station_id = as.character(end_station_id))
str(q2_2019)

mutate(q3_2019, ride_id = as.character(ride_id)
       , rideable_type = as.character(rideable_type)
       , start_station_id = as.character(start_station_id)
       , end_station_id = as.character(end_station_id))
q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id)
                  , rideable_type = as.character(rideable_type)
                  , start_station_id = as.character(start_station_id)
                  , end_station_id = as.character(end_station_id))
str(q3_2019)

mutate(q4_2019, ride_id = as.character(ride_id)
       , rideable_type = as.character(rideable_type)
       , start_station_id = as.character(start_station_id)
       , end_station_id = as.character(end_station_id))
q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id)
                  , rideable_type = as.character(rideable_type)
                  , start_station_id = as.character(start_station_id)
                  , end_station_id = as.character(end_station_id))
str(q4_2019)

mutate(q1_2020, ride_id = as.character(ride_id)
       , rideable_type = as.character(rideable_type)
       , start_station_id = as.character(start_station_id)
       , end_station_id = as.character(end_station_id))
q1_2020 <- mutate(q1_2020, ride_id = as.character(ride_id)
                  , rideable_type = as.character(rideable_type)
                  , start_station_id = as.character(start_station_id)
                  , end_station_id = as.character(end_station_id))
str(q1_2020)

mutate(q2_2020, ride_id = as.character(ride_id)
       , rideable_type = as.character(rideable_type)
       , start_station_id = as.character(start_station_id)
       , end_station_id = as.character(end_station_id))
q2_2020 <- mutate(q2_2020, ride_id = as.character(ride_id)
                  , rideable_type = as.character(rideable_type)
                  , start_station_id = as.character(start_station_id)
                  , end_station_id = as.character(end_station_id))
str(q2_2020)

mutate(q3_2020, ride_id = as.character(ride_id)
       , rideable_type = as.character(rideable_type)
       , start_station_id = as.character(start_station_id)
       , end_station_id = as.character(end_station_id))
q3_2020 <- mutate(q3_2020, ride_id = as.character(ride_id)
                  , rideable_type = as.character(rideable_type)
                  , start_station_id = as.character(start_station_id)
                  , end_station_id = as.character(end_station_id))
str(q3_2020)

mutate(q4_2020, ride_id = as.character(ride_id)
       , rideable_type = as.character(rideable_type)
       , start_station_id = as.character(start_station_id)
       , end_station_id = as.character(end_station_id))
q4_2020 <- mutate(q4_2020, ride_id = as.character(ride_id)
                  , rideable_type = as.character(rideable_type)
                  , start_station_id = as.character(start_station_id)
                  , end_station_id = as.character(end_station_id))
str(q4_2020)

bind_rows(q1_2019, q2_2019, q3_2019, q4_2019
          , q1_2020, q2_2020, q3_2020, q4_2020)

all_trips <- bind_rows(q1_2019, q2_2019, q3_2019, q4_2019
                       , q1_2020, q2_2020, q3_2020, q4_2020)
colnames(all_trips)
View(all_trips)

select(all_trips, -c(start_lat, start_lng, end_lat
                     , end_lng, gender, birthyear))
all_trips <- select(all_trips, -c(start_lat, start_lng, end_lat
                                  , end_lng, gender, birthyear))

head(all_trips)
nrow(all_trips)
dim(all_trips)
str(all_trips)
summary(all_trips)

# In the "member_casual" column, replace "Subscriber" with 
#"member" and "Customer" with "casual"

table(all_trips$member_casual)
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

table(all_trips$member_casual)
View(all_trips)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year 
# before completing these operations we could only aggregate at the ride level

all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)
View(all_trips)
colnames(all_trips)

# Convert "ride_length" from Factor to numeric 
# so that we can run calculations on the data is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out 
# of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

View(all_trips_v2)
summary(all_trips_v2)

all_trips_v2$ride_length <- as.numeric(as.character(all_trips_v2$ride_length))
is.numeric(all_trips_v2$ride_length)

#  Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length, na.rm = TRUE)#straight average (total ride length / rides)
median(all_trips_v2$ride_length, na.rm = TRUE)#midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length, na.rm = TRUE)#longest ride
min(all_trips_v2$ride_length, na.rm = TRUE)#shortest_ride

# condensing all the four lines above into one using the summary() function
summary(all_trips_v2$ride_length)

# compare members and casual user
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# see the average ride time by each day for member vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual 
          + all_trips_v2$day_of_week
          , FUN = mean)

# notice that the above code result didn't give us a well ordered days of the week
# now let's put it in a well arranged order
ordered(all_trips_v2$day_of_week, levels=c("Sunday"
                                           , "Monday"
                                           , "Tuesday"
                                           , "Wednesday"
                                           , "Thursday"
                                           , "Friday"
                                           , "Saturday"))
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday"
                                                                       , "Monday"
                                                                       , "Tuesday"
                                                                       , "Wednesday"
                                                                       , "Thursday"
                                                                       , "Friday"
                                                                       , "Saturday"))
View(all_trips_v2$day_of_week)
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#now we'd rerun the average time ride by each day for members vs casual users
# so as to see if the ordered code we entered would work
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual 
          + all_trips_v2$day_of_week
          , FUN = mean)

all_trips_v2 %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups="drop") %>% 
  arrange(member_casual, day_of_week)

# let us visualize the average time ride by each day for members vs casual
all_trips_v2 %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups="drop") %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides of Members and Casual riders Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# analyze ridership data by type and weekday
#creates weekday field using wday()
mutate(all_trips_v2, weekday = wday(started_at, label = TRUE)) # creates weekday field using wday()
all_trips_v2 <- mutate(all_trips_v2, weekday = wday(started_at, label = TRUE))
View((all_trips_v2))

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)	

# Let's visualize the number of rides and average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)	%>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title ="Total rides of Members and Casual riders Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Let's create a visualization for average duration 
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title ="Average duration of Members and Casual riders Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


## Let's create a visualization for Total rides by members and casual riders by month
all_trips_v2 %>%  
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),.groups="drop") %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides by Members and Casual riders by Month") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


## Let's compare Members and Casual riders depending on ride distance
all_trips_v2 %>% 
  group_by(member_casual) %>% drop_na() %>%
  summarise(average_ride_length = mean(ride_length)) %>%
  ggplot() + 
  geom_col(mapping= aes(x= member_casual,y= average_ride_length,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean distance traveled by Members and Casual riders")

## Let's visualize members and casuals by the total ride taken (ride count)
all_trips_v2 %>% 
  group_by(member_casual) %>% 
  summarise(ride_count = length(ride_id)) %>%
  ggplot(aes(x = member_casual, y = ride_count, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title ="Total rides taken (ride_count) of Members and Casual riders") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

 
install.packages("rmarkdown")
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

count <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual 
                   + all_trips_v2$day_of_week
                   , FUN = mean)
View(count)
write.csv(count, file = '~/Desktop/Bike_share/avg_ride_length.csv')

# create a directory
dir.create("Bike_share/")


# check if the directory created exists
print(dir.exists("Bike_share"))

# now try saving the data as csv file again
write.csv(count, file = "Bike_share/avg_ride_length.csv")
