library(readr)
# Load trip data for December 2023
dec_23_tripdata <- read_csv("divvy-tripdata/202312-divvy-tripdata.csv")


str(dec_23_tripdata)
head(dec_23_tripdata)
View(dec_23_tripdata)


# Get unique values ridetype
ride_type <- unique(dec_23_tripdata$rideable_type)
View(ride_type) # electric_bike/classic_bike/

# Count the total number of rows
total_rows <- nrow(dec_23_tripdata)

# Get unique values in member_casual
member_type <- unique(dec_23_tripdata$member_casual)
View(member_type) # member/casual

# Count the number of rows where member_casual == "member"
member_count <- sum(dec_23_tripdata$member_casual == "member")

# Calculate the percentage
member_percentage <- (member_count / total_rows) * 100

# Print the result
print(paste("Percentage of 'member':", round(member_percentage, 2), "%"))

#########o	Look for missing values and Omit them

# Check for missing values in the dataset
missing_summary <- colSums(is.na(dec_23_tripdata))
print("Summary of missing values in each column:")
print(missing_summary)


# Remove rows with missing values
cleaned_dec_23_tripdata <- na.omit(dec_23_tripdata)

# Confirm the dataset has no missing values
cleaned_missing_summary <- colSums(is.na(cleaned_dec_23_tripdata))
print("Summary of missing values after cleaning:")
print(cleaned_missing_summary)

# View the cleaned data
head(cleaned_dec_23_tripdata)

######## o	Ensure all date/time fields are in a proper datetime format.

# Check the structure of the dataset to identify date/time columns
str(cleaned_dec_23_tripdata)

# Convert the relevant columns to datetime format
cleaned_dec_23_tripdata$started_at <- as.POSIXct(cleaned_dec_23_tripdata$started_at, format = "%Y-%m-%d %H:%M:%S")
cleaned_dec_23_tripdata$ended_at <- as.POSIXct(cleaned_dec_23_tripdata$ended_at, format = "%Y-%m-%d %H:%M:%S")

# Verify the conversion
str(cleaned_dec_23_tripdata)

# Optionally, preview the cleaned data
head(cleaned_dec_23_tripdata)

######## o	Remove duplicate records if any.

# Check the number of rows before removing duplicates
print(paste("Number of rows before removing duplicates:", nrow(cleaned_dec_23_tripdata)))

# Load dplyr library if not already loaded
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Remove duplicate rows using dplyr
cleaned_dec_23_tripdata <- distinct(cleaned_dec_23_tripdata)

# Check the number of rows after removing duplicates
print(paste("Number of rows after removing duplicates:", nrow(cleaned_dec_23_tripdata)))
    

                 ######### ADD NEW COLUMNS


###### Calculate trip duration in minutes
cleaned_dec_23_tripdata$trip_duration <- as.numeric(difftime(
  cleaned_dec_23_tripdata$ended_at,
  cleaned_dec_23_tripdata$started_at,
  units = "mins"
))

# View the first few rows to confirm the new column
head(cleaned_dec_23_tripdata)

# Check summary of the trip_duration column
summary(cleaned_dec_23_tripdata$trip_duration)



###### Extract the day of the week from the started_at column
cleaned_dec_23_tripdata$day_of_week <- weekdays(cleaned_dec_23_tripdata$started_at)

# View the first few rows to confirm the new column
head(cleaned_dec_23_tripdata)

# Check unique days of the week to validate
unique(cleaned_dec_23_tripdata$day_of_week)



###### Extract the hour from the started_at column
cleaned_dec_23_tripdata$hour_of_day <- as.numeric(format(cleaned_dec_23_tripdata$started_at, "%H"))

# View the first few rows to confirm the new column
head(cleaned_dec_23_tripdata)

# Check summary of the hour_of_day column
summary(cleaned_dec_23_tripdata$hour_of_day)

write.csv(cleaned_dec_23_tripdata, "divvy-tripdata/cleaned_dec_23.csv")


                   ########Analyze the Data

# Load necessary library
library(dplyr)

# Load the dataset
data <- read.csv("divvy-tripdata/cleaned_dec_23.csv")



###### Total number of rides for "member" and "casual" riders.

#Aggregate total number of rides by user type
ride_summary <- data %>%
  group_by(member_casual) %>%
  summarise(Total_Rides = n())

# Print the result
print(ride_summary)


#######Average trip duration for each user type.

# Ensure trip_duration is numeric (if not already)
data$trip_duration <- as.numeric(data$trip_duration)

# Calculate the average trip duration for each user type
average_trip_duration <- data %>%
  group_by(member_casual) %>%
  summarise(Average_Trip_Duration = mean(trip_duration, na.rm = TRUE))

# Print the result
print(average_trip_duration)


######o	Most popular days of the week and times of day.

# Convert day_of_week and hour_of_day to appropriate types
data$day_of_week <- as.factor(data$day_of_week)
data$hour_of_day <- as.numeric(data$hour_of_day)

#####Method 1

# Find the most popular days of the week grouped by member_casual
popular_days_by_user <- data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(Total_Rides = n()) %>%
  arrange(member_casual, desc(Total_Rides))

# Find the most popular times of day grouped by member_casual
popular_times_by_user <- data %>%
  group_by(member_casual, hour_of_day) %>%
  summarise(Total_Rides = n()) %>%
  arrange(member_casual, desc(Total_Rides))

# Print the results
print("Most Popular Days of the Week by User Type:")
print(popular_days_by_user)

print("Most Popular Times of the Day by User Type:")
print(popular_times_by_user)


####Method2


# Convert day_of_week and hour_of_day to factors if needed
data$day_of_week <- as.factor(data$day_of_week)
data$hour_of_day <- as.numeric(data$hour_of_day)

# Find the most popular days of the week
popular_days <- data %>%
  group_by(member_casual) %>%
  count(day_of_week) %>%
  arrange(desc(n)) %>%
  rename(Day = day_of_week, Total_Rides = n)

# Find the most popular times of day
popular_times <- data %>%
  group_by(member_casual) %>%
  count(hour_of_day) %>%
  arrange(desc(n)) %>%
  rename(Hour = hour_of_day, Total_Rides = n)

# Find the most popular times of day for member rider
popular_times_member <- data %>%
  filter(member_casual == 'member') %>%
  count(hour_of_day) %>%
  arrange(desc(n)) %>%
  rename(Hour = hour_of_day, Total_Rides = n)

# Find the most popular times of day for casual rider
popular_times_casual <- data %>%
  filter(member_casual == 'casual') %>%
  count(hour_of_day) %>%
  arrange(desc(n)) %>%
  rename(Hour = hour_of_day, Total_Rides = n)

# Print the results
print("Most Popular Days of the Week:")
print(popular_days)

print("Most Popular Times of the Day:")
print(popular_times)

print("Most Popular Times of the Day for members:")
print(popular_times_member)

print("Most Popular Times of the Day for casual:")
print(popular_times_casual)


           ######2.	Analyze Usage Patterns

##### Identify trends in station usage (start and end locations).

# Load necessary library
library(dplyr)

# Load the dataset
data <- read.csv("divvy_tripdata/cleaned_dec_23.csv")

# Identify trends in start station usage
start_station_trends <- data %>%
  group_by(start_station_name) %>%
  summarise(Total_Starts = n()) %>%
  arrange(desc(Total_Starts))

# Identify trends in end station usage
end_station_trends <- data %>%
  group_by(end_station_name) %>%
  summarise(Total_Ends = n()) %>%
  arrange(desc(Total_Ends))

# Print the results
print("Trends in Start Station Usage:")
print(start_station_trends)

print("Trends in End Station Usage:")
print(end_station_trends)


######## Compare preferences for bike types (classic, electric, cargo).

# Compare preferences for bike types overall
bike_type_preferences <- data %>%
  group_by(rideable_type) %>%
  summarise(Total_Rides = n()) %>%
  arrange(desc(Total_Rides))

# Compare preferences for bike types by user type
bike_type_by_user <- data %>%
  group_by(member_casual, rideable_type) %>%
  summarise(Total_Rides = n()) %>%
  arrange(member_casual, desc(Total_Rides))

# Print the results
print("Overall Bike Type Preferences:")
print(bike_type_preferences)

print("Bike Type Preferences by User Type:")
print(bike_type_by_user)



                #######4. Visualize Insights

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
data <- read.csv("divvy-tripdata/cleaned_dec_23.csv")


##########•	Bar charts: Total rides and average trip durations by user type.

# Ensure trip_duration is numeric
data$trip_duration <- as.numeric(data$trip_duration)

# Prepare data: Total rides by user type
total_rides <- data %>%
  group_by(member_casual) %>%
  summarise(Total_Rides = n())

# Prepare data: Average trip duration by user type
avg_trip_duration <- data %>%
  group_by(member_casual) %>%
  summarise(Average_Trip_Duration = mean(trip_duration, na.rm = TRUE))

# Bar chart: Total rides by user type
ggplot(total_rides, aes(x = member_casual, y = Total_Rides, fill = member_casual)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Rides by User Type",
       x = "User Type",
       y = "Total Rides") +
  theme_minimal()

##total rides for each user type as a percentage to the total

# Calculate total rides as a percentage of the overall total
rides_percentage <- data %>%
  group_by(member_casual) %>%
  summarise(Total_Rides = n()) %>%
  mutate(Percentage = (Total_Rides / sum(Total_Rides)) * 100)

# Bar chart: Rides as percentages by user type
ggplot(rides_percentage, aes(x = member_casual, y = Percentage, fill = member_casual)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Total Rides by User Type",
       x = "User Type",
       y = "Percentage (%)") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Bar chart: Average trip durations by user type
ggplot(avg_trip_duration, aes(x = member_casual, y = Average_Trip_Duration, fill = member_casual)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Trip Durations by User Type",
       x = "User Type",
       y = "Average Trip Duration (minutes)") +
  theme_minimal()


#######•	Heatmaps: Peak hours and days for each user type.

# Load the dataset
data <- read.csv("divvy-tripdata/cleaned_dec_23.csv")

# Ensure `hour_of_day` is numeric and `day_of_week` is a factor
data$hour_of_day <- as.numeric(data$hour_of_day)
data$day_of_week <- factor(data$day_of_week, 
                           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Aggregate data: Count rides by user type, day of week, and hour of day
heatmap_data <- data %>%
  group_by(member_casual, day_of_week, hour_of_day) %>%
  summarise(Total_Rides = n()) %>%
  ungroup()

# Heatmap: Peak hours and days for each user type
ggplot(heatmap_data, aes(x = hour_of_day, y = day_of_week, fill = Total_Rides)) +
  geom_tile(color = "white") +
  facet_wrap(~ member_casual, ncol = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Total Rides") +
  labs(title = "Heatmap of Peak Hours and Days by User Type",
       x = "Hour of Day",
       y = "Day of Week") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))


#######•	Geographic maps: Popular routes and stations for each group.
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(sf)

# Load the dataset
data <- read.csv("divvy-tripdata/cleaned_dec_23.csv")

# Ensure latitude and longitude columns are numeric
data$start_lat <- as.numeric(data$start_lat)
data$start_lng <- as.numeric(data$start_lng)
data$end_lat <- as.numeric(data$end_lat)
data$end_lng <- as.numeric(data$end_lng)

# Aggregate data to find popular routes (start and end stations)
popular_routes <- data %>%
  group_by(member_casual, start_station_name, end_station_name, start_lat, start_lng, end_lat, end_lng) %>%
  summarise(Total_Rides = n()) %>%
  arrange(desc(Total_Rides))

# Filter top routes for visualization (e.g., top 50 routes for each group)
top_routes <- popular_routes %>%
  group_by(member_casual) %>%
  slice_max(Total_Rides, n = 50)

# Plot popular routes on a geographic map
ggplot() +
  geom_point(data = top_routes, aes(x = start_lng, y = start_lat, color = member_casual), size = 3, alpha = 0.7) +
  geom_point(data = top_routes, aes(x = end_lng, y = end_lat, color = member_casual), size = 3, shape = 21, alpha = 0.7) +
  geom_segment(data = top_routes, 
               aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, color = member_casual),
               size = 0.5, alpha = 0.6) +
  scale_color_manual(values = c("member" = "blue", "casual" = "red")) +
  labs(title = "Popular Routes and Stations by User Type",
       x = "Longitude",
       y = "Latitude",
       color = "User Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

########Using leaflet package for the geographical maps

# Load necessary libraries
library(dplyr)
library(leaflet)

# Load the dataset
data <- read.csv("divvy-tripdata/cleaned_dec_23.csv")

# Ensure latitude and longitude columns are numeric
data$start_lat <- as.numeric(data$start_lat)
data$start_lng <- as.numeric(data$start_lng)
data$end_lat <- as.numeric(data$end_lat)
data$end_lng <- as.numeric(data$end_lng)

# Aggregate data: Find popular routes (start and end stations)
popular_routes <- data %>%
  group_by(member_casual, start_station_name, end_station_name, start_lat, start_lng, end_lat, end_lng) %>%
  summarise(Total_Rides = n()) %>%
  arrange(desc(Total_Rides))

# Filter top routes for visualization (e.g., top 50 routes for each group)
top_routes <- popular_routes %>%
  group_by(member_casual) %>%
  slice_max(Total_Rides, n = 50)

# Create a leaflet map
leaflet() %>%
  addTiles() %>%
  # Add start stations
  addCircleMarkers(
    data = top_routes,
    lng = ~start_lng,
    lat = ~start_lat,
    color = "blue",
    radius = ~sqrt(Total_Rides) / 2,
    label = ~paste0("Start Station: ", start_station_name, 
                    "<br>Total Rides: ", Total_Rides),
    group = "Start Stations"
  ) %>%
  # Add end stations
  addCircleMarkers(
    data = top_routes,
    lng = ~end_lng,
    lat = ~end_lat,
    color = "red",
    radius = ~sqrt(Total_Rides) / 2,
    label = ~paste0("End Station: ", end_station_name, 
                    "<br>Total Rides: ", Total_Rides),
    group = "End Stations"
  ) %>%
  # Add routes (lines connecting start and end points)
  addPolylines(
    data = top_routes,
    lng = ~c(start_lng, end_lng),
    lat = ~c(start_lat, end_lat),
    color = ~ifelse(member_casual == "member", "blue", "red"),
    weight = 2,
    label = ~paste0("Route: ", start_station_name, " → ", end_station_name, 
                    "<br>Total Rides: ", Total_Rides),
    group = "Routes"
  ) %>%
  # Add layer control
  addLayersControl(
    overlayGroups = c("Start Stations", "End Stations", "Routes"),
    options = layersControlOptions(collapsed = FALSE)
  )

