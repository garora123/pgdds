library(ggplot2)
library(dplyr)

# import data
uber_request <- read.csv("Uber Request Data.csv", header = T, stringsAsFactors = F)

# data understanding
summary(uber_request)
str(uber_request)

# data preparation
# 1) convert request.id and driver.id to char
list <- c("Request.id", "Driver.id")
uber_request[list] <- sapply(uber_request[list], function(x) as.character(x))

# 2) convert timestamps to date objects
indices <- grep("\\d+\\-\\d+\\-\\d+", uber_request$Request.timestamp)
uber_request$Request.timestamp[indices] <- as.character(strptime(uber_request$Request.timestamp[indices], "%d-%m-%Y %H:%M:%S"))

indices <- grep("\\d+\\/\\d+\\/\\d+",uber_request$Request.timestamp)
uber_request$Request.timestamp[indices] <- paste(uber_request$Request.timestamp[indices], ":00" , sep = "" )
uber_request$Request.timestamp[indices] <- as.character(strptime(uber_request$Request.timestamp[indices], "%d/%m/%Y %H:%M:%S"))

indices <- grep("\\d+\\-\\d+\\-\\d+", uber_request$Drop.timestamp)
uber_request$Drop.timestamp[indices] <- as.character(strptime(uber_request$Drop.timestamp[indices], "%d-%m-%Y %H:%M:%S"))

indices <- grep("\\d+\\/\\d+\\/\\d+",uber_request$Drop.timestamp)
uber_request$Drop.timestamp[indices] <- paste(uber_request$Drop.timestamp[indices], ":00" , sep = "" )
uber_request$Drop.timestamp[indices] <- as.character(strptime(uber_request$Drop.timestamp[indices], "%d/%m/%Y %H:%M:%S"))

uber_request$Request.timestamp <- strptime(uber_request$Request.timestamp, "%Y-%m-%d %H:%M:%S")
uber_request$Drop.timestamp <- strptime(uber_request$Drop.timestamp, "%Y-%m-%d %H:%M:%S")


# add few columns
uber_request$Weekday <- weekdays(uber_request$Request.timestamp)
uber_request$Hour <- as.numeric(format(uber_request$Request.timestamp, "%H"))

uber_request$Supply <- ifelse(uber_request$Status == "Trip Completed", "Available", "Not Available")

# bin the hours into time of day
uber_request$timeofday <- ifelse(uber_request$Hour >= 4 & uber_request$Hour < 8, 'Early Morning',
                                 ifelse(uber_request$Hour >= 8 & uber_request$Hour < 12, 'Morning',
                                        ifelse(uber_request$Hour >= 12 & uber_request$Hour < 16, 'Afternoon',
                                               ifelse(uber_request$Hour >= 16 & uber_request$Hour < 20, 'Evening',
                                                      ifelse(uber_request$Hour >= 20 & uber_request$Hour <= 23, 'Night',
                                                             ifelse(uber_request$Hour >= 0 & uber_request$Hour < 4, 'Midnight', 'NA'))))))
table(factor(uber_request$timeofday))

# percentage of trips completed
uber_request$Status <- factor(uber_request$Status)
table(uber_request$Status)
length(which(uber_request$Status == "Trip Completed"))/ nrow(uber_request)
length(which(uber_request$Status != "Trip Completed"))/ nrow(uber_request)

# check for duplicate request ids
length(unique(uber_request$Request.id)) == nrow(uber_request)

# remove unwanted columns
names(uber_request)
uber_request[,c(5,6)] <- NULL

# frequency of status
p1 <- ggplot(uber_request, aes(x = factor(Status), fill = Pickup.point)) + geom_histogram(stat = "count") + 
  labs(x = "Status", y = "Frequency") + labs(title = "Trip Status Frequency")
#p1 <- p1 + geom_text(aes(label = length(factor(Status))), position = position_stack(vjust = 0.5), size = 4)
p1

# Trip completed analysis
uber_request$Status <- as.character(uber_request$Status)
tripcompleted <- filter(uber_request, Status == "Trip Completed")
df <- aggregate(tripcompleted, list(tripcompleted$Weekday, tripcompleted$Pickup.point), length)
df[,c(4:8)] <- NULL
colnames(df) <- c("Weekday", "Pickup.point", "Number_Of_Trips")

df$Weekday <- factor(df$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
ggplot(df , aes(x = factor(Weekday), y = Number_Of_Trips, fill = Pickup.point)) + geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Weekday", y = "Trip Count") + labs(title = "Trip Completed Stats by Each Day")

# Trip completed status each hour on each weekday by pickup point (line plot)
if(!require("cowplot"))  install.packages("cowplot")
library(cowplot)

plot_grid(ggplot(filter(tripcompleted, Weekday == "Monday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + 
  scale_y_continuous(name = "Trip Count") + 
    theme_bw() +
  labs(title = "Trips Completed on Monday"),
  ggplot(filter(tripcompleted, Weekday == "Tuesday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
    scale_x_continuous(breaks = seq(0,23,1)) + 
    scale_y_continuous(name = "Trip Count") + 
    theme_bw() +
    labs(title = "Trips Completed on Tuesday"),
  ggplot(filter(tripcompleted, Weekday == "Wednesday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
    scale_x_continuous(breaks = seq(0,23,1)) + 
    scale_y_continuous(name = "Trip Count") + 
    theme_bw() +
    labs(title = "Trips Completed on Wednesday"),
  ggplot(filter(tripcompleted, Weekday == "Thursday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
    scale_x_continuous(breaks = seq(0,23,1)) + 
    scale_y_continuous(name = "Trip Count") + 
    theme_bw() +
    labs(title = "Trips Completed on Thursday"),
  ggplot(filter(tripcompleted, Weekday == "Friday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
    scale_x_continuous(breaks = seq(0,23,1)) + 
    scale_y_continuous(name = "Trip Count") + 
    theme_bw() +
    labs(title = "Trips Completed on Friday")
)

# Cancelled trip analysis
cancelled <- subset(uber_request, Status == "Cancelled", select = c("Status","Driver.id", "Pickup.point", "Weekday", "Hour"))

df <- aggregate(cancelled, list(cancelled$Weekday, cancelled$Pickup.point), length)
df[,c(4:8)] <- NULL
colnames(df) <- c("Weekday", "Pickup.point", "Number_Of_Trips")

df$Weekday <- factor(df$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
ggplot(df , aes(x = factor(Weekday), y = Number_Of_Trips, fill = Pickup.point)) + geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Weekday", y = "Trip Count") + labs(title = "Trip Cancelled Stats by Each Day")


# Trip cancelled status each hour on each weekday by pickup point (line plot)
#if(!require("cowplot"))  install.packages("cowplot")
#library(cowplot)

plot_grid(ggplot(filter(cancelled, Weekday == "Monday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + 
            scale_y_continuous(name = "Trip Count") + 
            theme_bw() +
            labs(title = "Trips Cancelled on Monday"),
          ggplot(filter(cancelled, Weekday == "Tuesday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + 
            scale_y_continuous(name = "Trip Count") + 
            theme_bw() +
            labs(title = "Trips Cancelled on Tuesday"),
          ggplot(filter(cancelled, Weekday == "Wednesday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + 
            scale_y_continuous(name = "Trip Count") + 
            theme_bw() +
            labs(title = "Trips Cancelled on Wednesday"),
          ggplot(filter(cancelled, Weekday == "Thursday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + 
            scale_y_continuous(name = "Trip Count") + 
            theme_bw() +
            labs(title = "Trips Cancelled on Thursday"),
          ggplot(filter(cancelled, Weekday == "Friday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + 
            scale_y_continuous(name = "Trip Count") + 
            theme_bw() +
            labs(title = "Trips Cancelled on Friday")
)

# No cars available analysis
nocars <- subset(uber_request, Status == "No Cars Available", select = c("Status","Driver.id", "Pickup.point", "Weekday", "Hour"))

df <- aggregate(nocars, list(nocars$Weekday, nocars$Pickup.point), length)
df[,c(4:8)] <- NULL
colnames(df) <- c("Weekday", "Pickup.point", "Number_Of_Trips")

df$Weekday <- factor(df$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
ggplot(df , aes(x = factor(Weekday), y = Number_Of_Trips, fill = Pickup.point)) + geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Weekday", y = "Trip Count") + labs(title = "Trip Cancelled Stats by Each Day")


# No cars available status each hour on each weekday by pickup point (line plot)
#if(!require("cowplot"))  install.packages("cowplot")
#library(cowplot)

plot_grid(ggplot(filter(nocars, Weekday == "Monday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + 
            scale_y_continuous(name = "Trip Count") + 
            theme_bw() +
            labs(title = "Trips w/ no cars available on Monday"),
          ggplot(filter(nocars, Weekday == "Tuesday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + 
            scale_y_continuous(name = "Trip Count") + 
            theme_bw() +
            labs(title = "Trips w/ no cars available on Tuesday"),
          ggplot(filter(nocars, Weekday == "Wednesday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + 
            scale_y_continuous(name = "Trip Count") + 
            theme_bw() +
            labs(title = "Trips w/ no cars available on Wednesday"),
          ggplot(filter(nocars, Weekday == "Thursday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + 
            scale_y_continuous(name = "Trip Count") + 
            theme_bw() +
            labs(title = "Trips w/ no cars available on Thursday"),
          ggplot(filter(nocars, Weekday == "Friday") , aes(x = Hour, colour = Pickup.point)) + geom_line(stat = "count", size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + 
            scale_y_continuous(name = "Trip Count") + 
            theme_bw() +
            labs(title = "Trips w/ no cars available on Friday")
)

# Demand Supply Analysis
# Demand from Airport each day, each hour
df <- filter(uber_request, Pickup.point == "Airport")
colour <- c("springgreen", "yellow")

demand <- aggregate(df$Request.id, list(df$Weekday, df$Hour, df$Supply), length)
names(demand) <- c("Weekday", "Hour", "Supply", "Count")
plot_grid(ggplot(filter(demand, Weekday == "Monday"), aes(x = Hour, y = Count, colour = factor(Supply))) + geom_line(size = 1.5) + 
  scale_x_continuous(breaks = seq(0,23,1)) + theme_bw() + 
  labs(title = "Demand/ Supply towards City On Monday") + 
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
    scale_colour_manual(values = colour),
  ggplot(filter(demand, Weekday == "Tuesday"), aes(x = Hour, y = Count, colour = factor(Supply))) + geom_line(size = 1.5) + 
    scale_x_continuous(breaks = seq(0,23,1)) + theme_bw() + 
    labs(title = "Demand/ Supply towards City On Tuesday") + 
    theme(legend.position = "bottom", legend.direction = "horizontal",
          legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
    scale_colour_manual(values = colour),
  ggplot(filter(demand, Weekday == "Wednesday"), aes(x = Hour, y = Count, colour = factor(Supply))) + geom_line(size = 1.5) + 
    scale_x_continuous(breaks = seq(0,23,1)) + theme_bw() + 
    labs(title = "Demand/ Supply towards City On Wednesday") + 
    theme(legend.position = "bottom", legend.direction = "horizontal",
          legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
    scale_colour_manual(values = colour),
  ggplot(filter(demand, Weekday == "Thursday"), aes(x = Hour, y = Count, colour = factor(Supply))) + geom_line(size = 1.5) + 
    scale_x_continuous(breaks = seq(0,23,1)) + theme_bw() + 
    labs(title = "Demand/ Supply towards City On Thursday") + 
    theme(legend.position = "bottom", legend.direction = "horizontal",
          legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
    scale_colour_manual(values = colour),
  ggplot(filter(demand, Weekday == "Friday"), aes(x = Hour, y = Count, colour = factor(Supply))) + geom_line(size = 1.5) + 
    scale_x_continuous(breaks = seq(0,23,1)) + theme_bw() + 
    labs(title = "Demand/ Supply towards City On Friday") + 
    theme(legend.position = "bottom", legend.direction = "horizontal",
          legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
    scale_colour_manual(values = colour)
  )


# Demand from City each day, each hour
df <- filter(uber_request, Pickup.point == "City")
colour <- c("turquoise1", "violetred1")

demand <- aggregate(df$Request.id, list(df$Weekday, df$Hour, df$Supply), length)
names(demand) <- c("Weekday", "Hour", "Supply", "Count")
plot_grid(ggplot(filter(demand, Weekday == "Monday"), aes(x = Hour, y = Count, colour = factor(Supply))) + geom_line(size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + theme_bw() + 
            labs(title = "Demand/ Supply towards Airport On Monday") + 
            theme(legend.position = "bottom", legend.direction = "horizontal",
                  legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
            scale_colour_manual(values = colour),
          ggplot(filter(demand, Weekday == "Tuesday"), aes(x = Hour, y = Count, colour = factor(Supply))) + geom_line(size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + theme_bw() + 
            labs(title = "Demand/ Supply towards Airport On Tuesday") + 
            theme(legend.position = "bottom", legend.direction = "horizontal",
                  legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
            scale_colour_manual(values = colour),
          ggplot(filter(demand, Weekday == "Wednesday"), aes(x = Hour, y = Count, colour = factor(Supply))) + geom_line(size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + theme_bw() + 
            labs(title = "Demand/ Supply towards Airport On Wednesday") + 
            theme(legend.position = "bottom", legend.direction = "horizontal",
                  legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
            scale_colour_manual(values = colour),
          ggplot(filter(demand, Weekday == "Thursday"), aes(x = Hour, y = Count, colour = factor(Supply))) + geom_line(size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + theme_bw() + 
            labs(title = "Demand/ Supply towards Airport On Thursday") + 
            theme(legend.position = "bottom", legend.direction = "horizontal",
                  legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
            scale_colour_manual(values = colour),
          ggplot(filter(demand, Weekday == "Friday"), aes(x = Hour, y = Count, colour = factor(Supply))) + geom_line(size = 1.5) + 
            scale_x_continuous(breaks = seq(0,23,1)) + theme_bw() + 
            labs(title = "Demand/ Supply towards Airport On Friday") + 
            theme(legend.position = "bottom", legend.direction = "horizontal",
                  legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
            scale_colour_manual(values = colour)