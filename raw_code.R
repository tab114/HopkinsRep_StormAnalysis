setwd("~/Statistics/Courses/Reproducible Research/Peer Assesments/Peer Assessment 2")

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
              destfile = "StormData.csv.bz2", method = "curl")

StormData <- read.csv(bzfile("weatherdata.csv.bz2"))
head(StormData)
names(StormData)

# number of unique event types
length(unique(StormData$EVTYPE))

# translate all letters to lowercase
event_types <- tolower(StormData$EVTYPE)
# replace all punct. characters with a space
event_types <- gsub(pattern = "[[:blank:][:punct:]+]", replacement = " ", x = event_types)
#[:blank:]  Space and TAB characters only.
#[:punct:]  Punctuation symbols . , " ' ? ! ; : # $ % & ( ) * + - / < > = @ [ ] \ ^ _ { } | ~
#The plus sign + indicates there is one or more of the preceding element - blank and/or puct. 
length(unique(event_types))
head(event_types)
# update the data frame
StormData$EVTYPE <- event_types

#Dangerous Events with respect to Population Health
library(plyr)
casualties <- ddply(StormData, .(EVTYPE), summarize,
                    fatalities = sum(FATALITIES),
                    injuries = sum(INJURIES))
head(casualties)
#the empty ENVTYPE was '?' before.

# Find events that caused most death and injury
fatal_events <- head(casualties[order(casualties$fatalities, decreasing = T), ], 10)
injury_events <- head(casualties[order(casualties$injuries, decreasing = T), ], 10)

#Top 10 events that caused largest number of deaths are:
fatal_events[, c("EVTYPE", "fatalities")]

#Top 10 events that caused most number of injuries are
injury_events[, c("EVTYPE", "injuries")]


#Economic Effects of Weather Events
names(StormData)
head(StormData$PROPDMG)
head(StormData$PROPDMGEXP)

exp_transform <- function(e) {
  # h = hundred, k = thousand, m = million, b = billion, '' '-' '?' '+' = The raw number. 
  if (e %in% c('h', 'H'))
    return(2)
  else if (e %in% c('k', 'K'))
    return(3)
  else if (e %in% c('m', 'M'))
    return(6)
  else if (e %in% c('b', 'B'))
    return(9)
  else if (!is.na(as.numeric(e))) # if a digit
    return(as.numeric(e))
  else if (e %in% c('', '-', '?', '+'))
    return(0)
  else {
    stop("Invalid exponent value.")
  }
}

prop_dmg_exp <- sapply(StormData$PROPDMGEXP, FUN=exp_transform)
StormData$prop_dmg <- StormData$PROPDMG * (10^prop_dmg_exp)
crop_dmg_exp <- sapply(StormData$CROPDMGEXP, FUN=exp_transform)
StormData$crop_dmg <- StormData$CROPDMG * (10^crop_dmg_exp)

# Compute the economic loss by event type
library(plyr)
econ_loss <- ddply(StormData, .(EVTYPE), summarize,
                   prop_dmg = sum(prop_dmg),
                   crop_dmg = sum(crop_dmg))
# filter out events that caused no economic loss
econ_loss <- econ_loss[(econ_loss$prop_dmg > 0 | econ_loss$crop_dmg > 0), ]

# Find events that caused most crop and prop damage:
prop_dmg_events <- head(econ_loss[order(econ_loss$prop_dmg, decreasing = T), ], 10)
crop_dmg_events <- head(econ_loss[order(econ_loss$crop_dmg, decreasing = T), ], 10)

# Top 10 events that caused most property damage (in dollars) are:
prop_dmg_events[, c("EVTYPE", "prop_dmg")]

#Similarly, the events that caused biggest crop damage are
crop_dmg_events[, c("EVTYPE", "crop_dmg")]


##Health impact of weather events

#The following plot shows top dangerous weather event types.
library(ggplot2)
library(gridExtra)
# Set the levels in order
p1 <- ggplot(data=fatal_events,
             aes(x=reorder(x=EVTYPE, X=fatalities), y=fatalities, fill=fatalities)) +
  geom_bar(stat="identity") +  # bars represent the y values.
  coord_flip() +  # Flipped cartesian coordinates so that horizontal becomes vertical, 
                  # and vertical, horizontal.
  ylab("Total Number of Fatalities") +
  xlab("Event Type") +
theme(legend.position="none") # No legend on the right
# reorder treats its first argument as a categorical variable, and reorders its levels 
# based on the values of a second variable, usually numeric.

p2 <- ggplot(data=injury_events,
             aes(x=reorder(EVTYPE, injuries), y=injuries, fill=injuries)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  ylab("Total number of injuries") +
  xlab("Event type") +
  theme(legend.position="none")

grid.arrange(p1, p2, main="Top deadly weather events in the US (1950-2011)")


## Economic impact of weather events

p1 <- ggplot(data=prop_dmg_events,
             aes(x=reorder(EVTYPE, prop_dmg), y=log10(prop_dmg), fill=prop_dmg, alpha=1/2)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Event Type") +
  ylab("Property Damage in dollars (log-scale)") +
  theme(legend.position="none")
p2 <- ggplot(data=crop_dmg_events,
             aes(x=reorder(EVTYPE, crop_dmg), y=crop_dmg, fill=crop_dmg, alpha = 1/2)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  xlab("Event Type") +
  ylab("Crop Damage in dollars") + 
  theme(legend.position="none")

grid.arrange(p1, p2, main="Weather costs to the US economy (1950-2011)")
