# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
df <- read.csv("activity.csv")
nint <- length(unique(df$interval))
df$date <- as.Date(df$date, "%Y-%m-%d")
df$wkday <- weekdays(df$date)
file.remove("activity.csv")
```

```
## [1] TRUE
```

## Imputing missing values

```r
mint <- vector(mode = "numeric", length = nint)

templist <- mapply(function(x, y) {
  mint[y] <<- mean(x$steps, na.rm = TRUE)
  x$steps[is.na(x$steps)] <- mint[y]
  return(x)
  }, split(df, df$interval), 1:nint, SIMPLIFY = FALSE)

dfcomp <- do.call(rbind, templist)
rm(templist)
```

## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
