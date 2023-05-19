
# Cyclistic project
## Data analysis to solve a specific business problem: 
### The Cyclistic marketing team wants to know better about their clients and how each client type use Cyclistic bikes


# Step 1 Loading the packages I need to work with
library(tidyverse)  #helps wrangle data
library(aws.s3)
getwd() #displays your working directory

# Setp 2 Collect data from AWS bucket
##### Before the data collection I inspected the files mannualy. Luckily for me all .csv have the same columns in the same order. Knowing that, I bind all together and then proceed with the data cleaning
#### Set your AWS access key and secret access key
# Access sensitive keys
api_key <- Sys.getenv("AWS_ACCESS_KEY")
password <- Sys.getenv("AWS_SECRET_ACCESS_KEY")

#### Set the S3 bucket name and region
bucket_name <- "divvy-tripdata"

#### List of file names to download and process
file_names <- c(
  "202204-divvy-tripdata.zip",
  "202205-divvy-tripdata.zip",
  "202206-divvy-tripdata.zip",
  "202207-divvy-tripdata.zip",
  "202208-divvy-tripdata.zip",
  "202209-divvy-tripdata.zip",
  "202210-divvy-tripdata.zip",
  "202211-divvy-tripdata.zip",
  "202212-divvy-tripdata.zip",
  "202301-divvy-tripdata.zip",
  "202302-divvy-tripdata.zip",
  "202303-divvy-tripdata.zip"
)

annual_data <- NULL  #### Create an empty dataframe to store the annual data

#### Iterate through the specified file names
for (file_name in file_names) {
  #### Download the .zip file locally
  local_zip_file <- tempfile(fileext = ".zip")
  aws.s3::save_object(
    object = file_name,
    bucket = bucket_name,
    file = local_zip_file
  )
  
  #### Read the .csv files from the .zip file
  zip_contents <- unzip(local_zip_file, list = TRUE)
  csv_files <- zip_contents$Name[!grepl("_MACOSX", zip_contents$Name)]
  
  for (csv_file in csv_files) {
    #### Extract the .csv file locally
    local_csv_file <- file.path(tempdir(), csv_file)
    unzip(local_zip_file, files = csv_file, exdir = tempdir())
    
    #### Read the data from the .csv file
    data <- read_csv(local_csv_file)  #### Adjust the function if you're using a different CSV library
    
    #### Append the data to the annual dataframe
    annual_data <- rbind(annual_data, data)
  }
}

### Inspect the table that has been created
colnames(annual_data)  #List of column names
nrow(annual_data)  #How many rows are in data frame?
dim(annual_data)  #Dimensions of the data frame?
head(annual_data)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(annual_data)  #See list of columns and data types (numeric, character, etc)
summary(annual_data)  #Statistical summary of data. Mainly for numerics
glimpse(annual_data)
library("skimr")
skim_without_charts(annual_data)


# Step 3: Clean up and add data to prepare for analysis
### Transform the time columns from character columns into proper time columns (POSIXct data types)
annual_data$ended_at <- as_datetime(annual_data$ended_at)
annual_data$started_at <- as_datetime(annual_data$started_at)

### Create a column where calculate the time of the ride
ride_length <- as.numeric(difftime(annual_data$ended_at,annual_data$started_at, units = "mins"))
annual_data$ride_length <- ride_length

### Check if there is outliers in the new column
ggplot(annual_data, aes(y = ride_length, color = rideable_type), title = "Looking for outliers by bike type") + 
  geom_boxplot() +
  facet_wrap(~rideable_type)
### Plenty of outliers, I have to clean a bit this column to work with it. I will remove docker_bike type in a new dataframe, and negative values (in ride_lenght) also
trips_cleaned <- annual_data %>% 
  filter(rideable_type != "docked_bike") %>% 
  filter(ride_length >= 0)
### Checking one more for outliers, and looking into the business problem *How different clients use different bikes*
ggplot(trips_cleaned, aes(y = ride_length, color = rideable_type), title = "Looking for outliers by bike type") + 
  geom_boxplot() +
  facet_grid(member_casual~rideable_type)


# Step 4: Conduct Descriptive analysis
### Descriptive analysis on ride_lenght (in minutes)
summary(trips_cleaned$ride_length)
### Compare members and casual users
aggregate(trips_cleaned$ride_length ~ trips_cleaned$member_casual, FUN = mean)
aggregate(trips_cleaned$ride_length ~ trips_cleaned$member_casual, FUN = median)
aggregate(trips_cleaned$ride_length ~ trips_cleaned$member_casual, FUN = max)
aggregate(trips_cleaned$ride_length ~ trips_cleaned$member_casual, FUN = min)

### Add a column with the day of the week when the ride started
trips_cleaned$weekday <- weekdays(trips_cleaned$started_at)

### See the average ride time by each day for members vs casual users
aggregate(trips_cleaned$ride_length ~ trips_cleaned$member_casual + trips_cleaned$weekday, FUN = mean)
trips_cleaned$weekday <- ordered(trips_cleaned$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) #### Notice that the days of the week are out of order. Let's fix that.

### Analyze ridership data by type and weekday
trips_cleaned %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	

### Create a visualization for average duration
trips_cleaned %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  ggtitle(label = 'Average ride lenght by client type', subtitle = 'Duration in minutes by weekday') +
  xlab("Weekday") + ylab("Average duration (minutes)") +
  scale_fill_discrete(name = "Client type")

### Create a visualization with number of rides by client type and weekday
#### Group the data by client type and weekday, and calculate the number of rides
rides_by_type_weekday <- trips_cleaned %>%
  group_by(member_casual, weekday) %>%
  summarize(num_rides = n()/1000)
ggplot(rides_by_type_weekday, aes(x = weekday, y = num_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  ggtitle(label = "Number of rides by weekday, and by client type") +
  labs(x = "Weekday", y = "Number of rides (in thousands)", fill = "Client type")

### Analyze last year trend  by client type, grouped by month. Of course, there is a seasonal trend (in winter, bike usage is lower in Chicago)
trips_cleaned %>%
  mutate(month = lubridate::floor_date(started_at, "month")) %>%
  group_by(month, member_casual) %>%
  summarise(n_rides = n()/1000) %>%
  ggplot(aes(x = month, y = n_rides, color = member_casual)) +
  geom_line() +
  labs(title = "Number of rides by client type and month",
       x = "Month",
       y = "Number of rides (in thousands)")

### Start the geographical analysis
library(ggmap)
google_maps_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
register_google(google_maps_key)
skim_without_charts(trips_cleaned)

### Group the data by start station name and calculate the number of rides for each station
start_station_counts <- trips_cleaned %>%
  group_by(start_station_name, member_casual) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

### Get a Chicago's map
map_chicago <- get_googlemap(center = c(lon = -87.6298, lat = 41.8781), zoom = 11)
ggmap(map_chicago)
### Group by longitude and latitude, and client type. And then count number of rides in each group. I used 20 top locations to focus where to promote physicalmarketing programs
rides_by_location <- trips_cleaned %>% 
  group_by(start_lng, start_lat, member_casual) %>% 
  summarize(num_rides = n()) %>% 
  ungroup()
top20_rides_by_location_member <- rides_by_location %>% 
  filter(member_casual == "member") %>% 
  top_n(20, num_rides)
top20_rides_by_location_casual <- rides_by_location %>% 
  filter(member_casual == "casual") %>% 
  top_n(20, num_rides)
### Plot the map and the data on top of it
ggmap(map_chicago) +
  geom_point(data = top20_rides_by_location_member, aes(x = start_lng, y = start_lat), color="red") +
  geom_point(data = top20_rides_by_location_casual, aes(x = start_lng, y = start_lat), color="blue") +
  scale_size(range = c(1, 10)) +
  labs(title = "Most frequent rides starting locations in Chicago",
       caption = "Red = Members. Blue = Casual riders") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank())
