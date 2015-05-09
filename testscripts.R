## ======================== Question ========================
# How can historical usage patterns of bike rentals combine with weather data 
# in order to forecast bike rental demand in the Capital Bikeshare program in 
# Washington, D.C.?

## ======================== project set up ========================
# using R version 3.1.3
library(ggplot2)
library(ggdendro)
library(lubridate)
library(plyr); library(dplyr)
source("myplclust.R")

setwd("~/../datascience/BikeSharingDemand")

## ======================== Download the data ========================
# dir.create("data")
# download train and test data sets from 
# https://www.kaggle.com/c/bike-sharing-demand/data
# place train.csv and test.csv into data subdirectory
# data obtained 2015-05-08

## ======================== load data ========================
train <- read.csv("./data/train.csv", stringsAsFactors=FALSE)
test <- read.csv("./data/test.csv", stringsAsFactors=FALSE)

## ======================== condition/clean data ======================== 
seasons <- c("spring", "summer", "fall", "winter")
# see https://www.kaggle.com/c/bike-sharing-demand/data for formal weather 
# description for competition:
weatherlabels <- c("nice weather", "cloudy/misty", "light weather",
                   "heavy weather")
daysoftheweek <- c("Monday","Tuesday","Wednesday","Thursday","Friday",
                   "Saturday","Sunday")
train <- mutate(train, 
                datetime = ymd_hms(datetime),
                season = factor(season, levels=1:4, labels=seasons),
                holiday = as.logical(holiday),
                workingday = factor(workingday, levels=c(1,0), labels=c("Workday","Holiday/Weekend")),
                weather = factor(weather,levels=1:4, labels=weatherlabels),
                dayofweek = factor(weekdays(datetime), levels=daysoftheweek),
                timeofday = hour(datetime))
#test <- mutate(test, ...)
#   TODO: fill out test data later once have solid train dataset

## only 1 heavy weather data point?
print("have very limited heavy weather data, interpret as light weather")
print(which(train$weather == "heavy weather"))
print(which(test$weather == "heavy weather"))
#print(train[which(train$weather == "heavy weather")+-11:12,])
# yes looks like it's just a very, very uncommon data label
# this should be explored (is this data set viable for weather patterns in the
# DC area?)
## interpolate heavy weather as light weather for graphing purposes
train$weather[train$weather == "heavy weather"] = "light weather"
test$weather[test$weather == "heavy weather"] = "light weather"


## ======================== Exploratory analysis ========================
graphics.off()
windows.options(width = 12, height = 8, xpinch=96, ypinch=96, xpos=0, ypos=0)

## time of day, temperature, weather label
gg1 <- ggplot(train, aes(timeofday, count, color=atemp)) +
    facet_grid(workingday ~ weather) +
    geom_point() +
    geom_smooth() +
    scale_colour_gradientn("Temp (°F)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142"))
windows()
print(gg1)
ggsave("time_tempurature_weather_workingday.png")

## day of week, temperature, weather label
gg2 <- ggplot(train, aes(dayofweek, log(count,10))) +
    facet_grid(. ~ weather) +
    geom_boxplot()
windows()
print(gg2)

## clustering using Hierarchical Clustering
# use a subset of the data for plotting clarity
set.seed(35)
subs1 <- train %>% sample_n(1000) %>%
    select(timeofday, atemp, weather, dayofweek, season, humidity, windspeed, count)
#subs1 <- data.frame(lapply(subs1, unclass), as.is=TRUE)
hClustering <- hclust(dist(subs1))

gg3 <- ggdendrogram(hClustering) + geom_line(aes(x=0:1, y=c(300,300)))
windows()
print(gg3)
# has 2 major clusters, with 4 subclusters, one of which is very small,
# but still very distinct
# TODO: anything more to interpret from this dendrogram? 

## atemp vs. temperature, rentals
gg4 <- ggplot(train, aes(temp, atemp, color=humidity, size=windspeed)) +
    geom_point() +
    scale_colour_gradient(low="blue",high="red")
windows()
print(gg4)
ggsave("atemp_temp_windspeed_humidity.png")
