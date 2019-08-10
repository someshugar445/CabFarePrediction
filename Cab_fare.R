# Load all the packages required for the analysis
library(ggplot2) # Visualisation
library(dplyr) # Data Wrangling
library(e1071) # Prediction: SVR
library(randomForest) # Prediction: Random Forest
library(corrplot)
library(rpart)
library(geosphere)
# Remove all objects from R
rm(list = ls())

setwd("/home/someshugar/Project2")

# Reading in the train data
train <- read.csv("/home/someshugar/Project2/train_cab.csv")
# Reading in the test data
test <- read.csv("/home/someshugar/Project2/test.csv")

head(train)
head(test)

str(train)
str(test)

summary(train)
summary(test)

sum(is.na(train))
sum(is.na(test))

as.data.frame(table(train$passenger_count))

train = train[which(train$passenger_count >= 1 & train$passenger_count <= 6),]

#train[(train$passenger_count >= 1.3 & train$passenger_count < 2),]

train <- train[!(train$passenger_count==1.3),]

#train <- train[complete.cases(train),]

sum(is.na(train$passenger_count))

hist(train$passenger_count)

str(train$fare_amount)

train$fare_amount <- as.numeric(as.factor(train$fare_amount))

sum(is.na(train$fare_amount))

train = subset(train,(train$fare_amount> quantile(train$fare_amount, c(.001)) & 
                 train$fare_amount< quantile(train$fare_amount, c(0.999))))

# box plot for 'Fare Amount'
boxplot(train$fare_amount,xlab="Fare Amount")


train$pickup_datetime <- as.POSIXct(strptime(train$pickup_datetime, "%Y-%m-%d %H:%M:%S")) 
test$pickup_datetime <- as.POSIXct(strptime(test$pickup_datetime, "%Y-%m-%d %H:%M:%S"))

str(train$pickup_datetime)
str(test$pickup_datetime)

library(lubridate)

# Generate the four new time variables
train <- train %>% 
  mutate(hour = hour(pickup_datetime),
         wday = wday(pickup_datetime, label = TRUE),
         month = month(pickup_datetime, label = TRUE),
         year = year(pickup_datetime))
head(train)

test <- test %>% 
  mutate(hour = hour(pickup_datetime),
         wday = wday(pickup_datetime, label = TRUE),
         month = month(pickup_datetime, label = TRUE),
         year = year(pickup_datetime))

head(test)

sum(is.na(train))

train <- train[complete.cases(train),]

# Convert latitudes and longitudes from decimal degrees to radians

library(pracma)
train$pickup_longitude <- deg2rad(train$pickup_longitude)
train$pickup_latitude <- deg2rad(train$pickup_latitude)
train$dropoff_longitude <- deg2rad(train$dropoff_longitude)
train$dropoff_latitude <- deg2rad(train$dropoff_latitude)

test$pickup_longitude <- deg2rad(test$pickup_longitude)
test$pickup_latitude <- deg2rad(test$pickup_latitude)
test$dropoff_longitude <- deg2rad(test$dropoff_longitude)
test$dropoff_latitude <- deg2rad(test$dropoff_latitude)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}



train$distance <- gcd.hf(train$pickup_longitude, train$pickup_latitude,train$dropoff_longitude, train$dropoff_latitude)

test$distance <- gcd.hf(test$pickup_longitude, test$pickup_latitude,test$dropoff_longitude, test$dropoff_latitude)

###################################Model Development#######################################

train_df <- train
test_df <- test

# build linear regression model on full data
lm_model <- lm(fare_amount ~ pickup_longitude+pickup_latitude
                +dropoff_longitude+dropoff_latitude+passenger_count+hour
                +wday+month+year+distance,data = train_df)  
print(lm_model)


# Create a Random Forest model
rf_model <- randomForest(fare_amount ~ pickup_longitude+pickup_latitude
                         +dropoff_longitude+dropoff_latitude+passenger_count+hour
                         +wday+month+year+distance,data = train_df)
print(rf_model)


# Predicting on test set
predTrain <- predict(rf_model, test_df)
View(predTrain)


cab_fare <- data.frame(fare_amount = as.integer(predTrain))

write.csv(cab_fare
          ,file = "cab_fare.csv"
          ,row.names = FALSE
          ,quote = FALSE
)





