## Unzipping and loading the data

unzip("activity.zip")
df <- read.csv("activity.csv")
str(df) #checking if the data was uploaded correctly
unique(df$date) #checking if the dates are proper
unique(df$interval)
nint <- length(unique(df$interval))
nint/12 == 24 #checking if the intervals are proper

time <- strptime(paste(df$date, df$interval), "%Y-%m-%d %H%M")

df$date <- as.Date(df$date, "%Y-%m-%d")
df$wkday <- weekdays(df$date)
  
file.remove("activity.csv")

colSums(is.na(df)) #checking for missing values

time <- strptime(paste(df$date, df$interval))

## Replacing NA's with the mean values for an interval
mint <- vector(mode = "numeric", length = nint)

templist <- mapply(function(x, y) {
  mint[y] <<- mean(x$steps, na.rm = TRUE)
  x$steps[is.na(x$steps)] <- mint[y]
  return(x)
  }, split(df, df$interval), 1:nint, SIMPLIFY = FALSE)

dfcomp <- do.call(rbind, templist)
rm(templist)

mint1 <- ts(mint, start = 0, frequency = 288)

## What is mean total number of steps taken per day?



## average daily activity pattern
plot(mint1, type='l')
