## ------------------------------------------------------------------------ 
##  title: "Reproducible Research"
##  author: "Macedo, Glener Diniz"
##  date: "Saturday, Agost 15, 2015"
##  output: html_document
## ------------------------------------------------------------------------ 
## -- Reproducible Research: Peer Assessment 1 ----------------------------
## -----Load the data------------------------------------------------------
data <- read.csv("activity.csv")   
## ------------------------------------------------------------------------
library(ggplot2)
total.steps <- tapply(data$steps,
                      data$date, 
                      FUN=sum, 
                      na.rm=TRUE)

qplot(total.steps, 
      binwidth=1000, 
      xlab="Total number of steps taken each day",
      ylab= deparse(substitute(Count)), 
      asp = NA
)

mean(total.steps, 
     na.rm=TRUE
     )

median(total.steps, 
       na.rm=TRUE
       )


## ------------------------------------------------------------------------
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), 
                      by=list(interval=data$interval),
                      FUN=mean, 
                      na.rm=TRUE
                       )

# ---Series plot of the 5-minute interval (xlab)
# ---The average number of steps taken, averaged across all days (ylab)  

ggplot(data=averages, 
       aes(x=interval, 
           y=steps
          )
      ) +
    geom_line() +
    geom_point(size=3, 
               colour="#CC0000"
              )+   
    xlab("5-minute interval") +
    ylab("Average Number of Steps Taken")


## ------------------------------------------------------------------------
averages[which.max(averages$steps),]


## ----how_many_missing----------------------------------------------------
missing <- is.na(data$steps)
# How many missing
table(missing)


## ------------------------------------------------------------------------
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, 
                       interval
                      ) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, 
                   "steps"]
                  )
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, 
                            filled.data$steps, 
                            filled.data$interval
                           )


## ------------------------------------------------------------------------
total.steps <- tapply(filled.data$steps, 
                      filled.data$date, 
                      FUN=sum
                     )
qplot(total.steps, 
      binwidth=1000, 
      xlab="Total number of steps taken each day"
      )
mean(total.steps)
median(total.steps)


## ------------------------------------------------------------------------
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday",
                   "Tuesday",
                   "Wednesday",
                   "Thursday",
                   "Friday"
                   )
        )
        return("weekday")
    else if (day %in% c("Saturday",
                        "Sunday",
                        "sábado"
                        )
            )
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day  <- sapply(filled.data$date, 
                           FUN=weekday.or.weekend
                          )
 

## ------------------------------------------------------------------------
averages <- aggregate(steps ~ interval + day, 
                      data=filled.data, 
                      mean)
ggplot(averages, 
       aes(interval, 
           steps
          )
       ) + 
       geom_line() + 
       facet_grid(day ~ .) +
       xlab("Five-Minute Interval") + 
       ylab("Number of steps")


