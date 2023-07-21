#--------------------------------------------GOOGLE CAPSTONE PROJECT------------------------------------------

#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame

#load original .csv files, a years worth of data from December 2021 to November 2022
jan_01 <- read_csv("202201-divvy-tripdata.csv") 
feb_02 <- read_csv("202202-divvy-tripdata.csv") 
mar_03 <- read_csv("202203-divvy-tripdata.csv")
apr_04 <- read_csv("202204-divvy-tripdata.csv")
may_05 <- read_csv("202205-divvy-tripdata.csv") 
jun_06 <- read_csv("202206-divvy-tripdata.csv") 
jul_07 <- read_csv("202207-divvy-tripdata.csv")
aug_08 <- read_csv("202208-divvy-tripdata.csv") 
sep_09 <- read_csv("202209-divvy-tripdata.csv") 
oct_10 <- read_csv("202210-divvy-tripdata.csv")
nov_11 <- read_csv("202211-divvy-tripdata.csv") 
dec_12 <- read_csv("202112-divvy-tripdata.csv")
 

#merge all of the data frames into one year view
cyclistic_merge <- rbind (jan_01, feb_02, mar_03, apr_04, may_05, jun_06, jul_07, aug_08, sep_09, oct_10, nov_11, dec_12)

#remove individual month data frames to clear up space in the environment 
remove (jan_01, feb_02, mar_03, apr_04, may_05, jun_06, jul_07, aug_08, sep_09, oct_10, nov_11, dec_12)

#create new data frame to contain new columns
cyclistic_data <- cyclistic_merge

#calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
cyclistic_data$ride_length <- difftime(cyclistic_merge$ended_at, cyclistic_merge$started_at, units = "mins")

#create columnds for: day of week, month, day, year, time, hour
cyclistic_data$date <- as.Date(cyclistic_data$started_at) #default format is yyyy-mm-dd, use start date
cyclistic_data$day_of_week <- wday(cyclistic_merge$started_at) #calculate the day of the week 
cyclistic_data$day_of_week <- format(as.Date(cyclistic_data$date), "%A") #create column for day of week
cyclistic_data$month <- format(as.Date(cyclistic_data$date), "%m")#create column for month
cyclistic_data$day <- format(as.Date(cyclistic_data$date), "%d") #create column for day
cyclistic_data$year <- format(as.Date(cyclistic_data$date), "%Y") #create column for year
cyclistic_data$time <- format(as.Date(cyclistic_data$date), "%H:%M:%S") #format time as HH:MM:SS
cyclistic_data$time <- as_hms((cyclistic_merge$started_at)) #create new column for time
cyclistic_data$hour <- hour(cyclistic_data$time) #create new column for hour

#create column for different seasons: Spring, Summer, Fall, Winter
cyclistic_data <-cyclistic_data %>% mutate(season = 
                                             case_when(month == "03" ~ "Spring",
                                                       month == "04" ~ "Spring",
                                                       month == "05" ~ "Spring",
                                                       month == "06"  ~ "Summer",
                                                       month == "07"  ~ "Summer",
                                                       month == "08"  ~ "Summer",
                                                       month == "09" ~ "Fall",
                                                       month == "10" ~ "Fall",
                                                       month == "11" ~ "Fall",
                                                       month == "12" ~ "Winter",
                                                       month == "01" ~ "Winter",
                                                       month == "02" ~ "Winter")
)

#create column for different time_of_day: Night, Morning, Afternoon, Evening
cyclistic_data <-cyclistic_data %>% mutate(time_of_day = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening")
)


#clean the data
cyclistic_data <- na.omit(cyclistic_data) #remove rows with NA values
cyclistic_data <- distinct(cyclistic_data) #remove duplicate rows 
cyclistic_data <- cyclistic_data[!(cyclistic_date$ride_length <=0),] #remove where ride_length is 0 or negative
cyclistic_data <- cyclistic_data %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) 

#view the final data
View(cyclistic_data)

#-----------------------------------------TOTAL AMOUNT OF RIDES--------------------------------------

#total number of rides
nrow(cyclistic_data)

#-----------------MEMBER TYPE---------------------
cyclistic_data %>%
  group_by(member_casual) %>% 
  count(member_casual)

#----------------TYPE OF BIKE---------------------

#total rides by member type 
cyclistic_data %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

#total rides 
cyclistic_data %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

#-------------------HOUR--------------------------

#total rides by member type 
cyclistic_data %>%
  group_by(member_casual) %>% 
  count(hour) %>% 
  print(n = 48) #lets you view the entire tibble

#total rides
cyclistic_data %>%
  count(hour) %>% 
  print(n = 24) #lets you view the entire tibble

#----------------------TIME OF DAY-----------------------

#-----morning-------
#total rides by member type 
cyclistic_data %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#total rides
cyclistic_data %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#-----afternoon-------
#total rides by member type 
cyclistic_data %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#total rides 
cyclistic_data %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#-----evening-------
#total rides by member type
cyclistic_data %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#total rides
cyclistic_data %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#-----night-------
#number of rides by member type
cyclistic_data %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#number of rides 
cyclistic_data %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#---all times of day----
#total rides by member type 
cyclistic_data %>%
  group_by(member_casual) %>% 
  count(time_of_day)

#number of rides
cyclistic_data %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

#----------------DAY OF THE WEEK------------------

#total rides by member type
cyclistic_data %>%
  group_by(member_casual) %>% 
  count(day_of_week)

#total rides 
cyclistic_data %>%
  count(day_of_week)

#----------------DAY OF THE MONTH-----------------

#total rides by member type
cyclistic_data %>%
  group_by(member_casual) %>% 
  count(day) %>% 
  print(n = 62) #lets you view the entire tibble

#total rides
cyclistic_data %>%
  count(day) %>% 
  print(n = 31) #lets you view the entire tibble

#---------------------MONTH-----------------------

#total rides by member type 
cyclistic_data %>%
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24) #lets you view the entire tibble

#total rides
cyclistic_data %>%
  count(month) 

#--------------------SEASON-----------------------

#-----spring-------

#total rides by member type 
cyclistic_data %>%
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  count(season)

#total rides
cyclistic_data %>%
  filter(season == "Spring") %>% 
  count(season)

#-----summer-------

#total rides by member type
cyclistic_data %>%
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  count(season)

#total rides
cyclistic_data %>%
  filter(season == "Summer") %>% 
  count(season)

#-----fall-------

#total rides by member type
cyclistic_data %>%
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  count(season)

#total rides
cyclistic_data %>%
  filter(season == "Fall") %>% 
  count(season)

#-----winter-------

#total rides by member type
cyclistic_data %>%
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  count(season)

#total rides 
cyclistic_data %>%
  filter(season == "Winter") %>% 
  count(season)

#-----all seasons-------

#total rides by member type
cyclistic_data %>%
  group_by(season, member_casual) %>% 
  count(season)

#total rides
cyclistic_data %>%
  group_by(season) %>% 
  count(season)

#------------------------------------AVERAGE RIDE LENGTH-----------------------------------

#average of ride_length
cyclistic_avgRide <- mean(cyclistic_data$ride_length)
print(cyclistic_avgRide)

#------------------MEMBER TYPE--------------------

#average ride_length
cyclistic_data %>% group_by( member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----------------TYPE OF BIKE---------------------

#total rides by member type 
cyclistic_data %>% group_by(member_casual, rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length
cyclistic_data %>% group_by(rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------------HOUR-------------------------

#average ride_length by member type
cyclistic_data %>% group_by(hour, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=48) #lets you view entire tibble

#average ride_length
cyclistic_data %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24) #lets you view entire tibble

#--------------------TIME OF DAY---------------------

#----morning----

#average ride length by member type
cyclistic_data %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_data %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----afternoon----

#average ride length by member type
cyclistic_data %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_data %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----evening----

#average ride length by member type
cyclistic_data %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_data %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----night----

#average ride length by member type 
cyclistic_data %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_data %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#---all times of day---

#average ride length by member type
cyclistic_data %>% 
  group_by(time_of_day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_data %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-------------------DAY OF THE WEEK-----------------

#average ride_length by member type
cyclistic_data %>% group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length 
cyclistic_data %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------DAY OF THE MONTH------------------

#average ride_length by member type
cyclistic_data %>% group_by(day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=62)  #lets you view entire tibble

#average ride_length
cyclistic_data %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=31)  #lets you view entire tibble

#---------------------MONTH--------------------------

#average ride_length by member type
cyclistic_data %>% group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)  #lets you view entire tibble

#average ride_length
cyclistic_data %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----------------------SEASON-------------------------

#-----spring------

#average ride length by member type
cyclistic_data %>% 
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_data %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----summer------

#average ride length by member type for summer 
cyclistic_data %>% 
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for summer 
cyclistic_data %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----fall------

#average ride length by member type
cyclistic_data %>% 
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_data %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----winter-----

#average ride length by member type
cyclistic_data %>% 
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_data %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----all seasons----

#average ride length by member type
cyclistic_data %>% 
  group_by(season, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length 
cyclistic_data %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
