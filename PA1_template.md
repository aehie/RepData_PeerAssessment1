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

## What is mean total number of steps taken per day?
First we compute the total number of steps taken per day:


```r
sperday <- sapply(split(df$steps, df$date), sum, na.rm = TRUE)
```

Here is the histogram of the total number of steps taken per day:

```r
hist(sperday, main = "Steps per day", xlab = "", ylab = "Number of days")
```

![](./PA1_template_files/figure-html/histmday-1.png) 

Average number of steps per day is:


```r
mean(sperday)
```

```
## [1] 9354.23
```

Median number of steps per day is:


```r
median(sperday)
```

```
## [1] 10395
```

## Imputing missing values

We substitute the missing values for steps my the mean values with the corresponding 5-minutes interval. We also save the mean values for the number of steps per interval into the vector *mint*


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

## What is the average daily activity pattern?

We convert the vector of mean steps per interval *mint* into a time series:

```r
mint <- ts(mint)
```

We plot the daily activity pattern across 5-minutes intervals


```r
plot(mint, main="Daily Activity Pattern", xlab = "5-minute interval",
     ylab = "Mean number of steps")
```

![](./PA1_template_files/figure-html/mintplot-1.png) 

The interval that has the highest average activity is:

```r
paste("the interval number" ,which.max(mint), " with the average number of steps ", max(mint))
```

```
## [1] "the interval number 104  with the average number of steps  206.169811320755"
```


## Are there differences in activity patterns between weekdays and weekends?
