# Dates-Times-in-R

#Group Names:
#Amani Almarzog
#Fatimah Al-Rashed

#--------------------------------ch16---------------------------------

########16.2.4 Exercises

install.packages("lubridate")
install.packages("nycflights13")
library(tidyverse)
library(lubridate)
library(nycflights13)

########16.2.4 Exercises
#1. What happens if you parse a string that contains invalid dates?

ymd(c("2010-10-10", "bananas")) #it will return NA
____________________
#2. What does the tzone argument to today() do? Why is it important?
#tzone specifies which time zone you would like to find the current date of. 
#It is important when you want ot find the date of a timezone that is different from the computer’s setting.
____________________
#3. Use the appropriate lubridate function to parse each of the following dates:
d1 <- "January 1, 2010"
mdy(d1) #"2010-01-01"
d2 <- "2015-Mar-07"
ymd(d2) #"2015-03-07"
d3 <- "06-Jun-2017"
dmy(d3)  #"2017-06-06"
d4 <- c("August 19 (2015)", "July 1 (2015)")
mdy(d4)  #"2015-08-19" "2015-07-01"
d5 <- "12/30/14" # Dec 30, 2014
mdy(d5)  #"2014-12-30"
____________________
########16.3.4 Exercises

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))

#1. How does the distribution of flight times within a day change over the course of the year?
flights_dt %>%
  mutate(date = make_date(year(dep_time),
                          month(dep_time),
                          mday(dep_time)),
         hour = hour(dep_time)) %>%
  group_by(date, hour) %>%
  filter(date == '2013-01-01') %>%
  ggplot(mapping = aes(x = hour)) +
  geom_density(alpha = .1)
______________________
#2. Compare dep_time, sched_dep_time and dep_delay. Are they consistent? Explain your findings.
flights_dt %>% select(contains('dep')) %>%
  mutate(cal_delay = as.numeric(dep_time - sched_dep_time) / 60) %>%
  filter(dep_delay != cal_delay) #check
#These inconsistent,The dep_delay show the flights were delayed, but cal_delay show the flights departed early.
_____________________
#3. Compare air_time with the duration between the departure and arrival. Explain your findings. (Hint: consider the location of the airport.)
flights_dt %>%
  left_join(airports, by = c('origin' = 'faa')) %>%
  left_join(airports, by = c('dest' = 'faa'), suffix = c('.origin','.dest')) %>%
  select(dep_time, arr_time, air_time, contains('tzone'))
# time difference between arr_time and dep_time with timezone 
flights_dt %>%
  left_join(airports, by = c('origin' = 'faa')) %>%
  left_join(airports, by = c('dest' = 'faa'), suffix = c('.origin','.dest')) %>%
  select(dep_time, arr_time, air_time, contains('tzone')) %>%
  mutate(dep_time = force_tz(dep_time, tzone = tzone.origin),
         arr_time = force_tz(arr_time, tzone = tzone.dest),
         cal_air_time = as.numeric(arr_time - dep_time) * 60) %>%
  select(contains('air'))
#not more clear but air_time is the time spent in the air. Perhaps dep_time and arr_time do not necessarily mean the exact time the planes leave/touch the ground.
____________________
#4. How does the average delay time change over the course of a day? Should you use dep_time or sched_dep_time? Why?
#We should use sched_dep_time because it will tell us how much delay we should expect at the scheduled departure time.
flights_dt %>%
  mutate(hour = hour(sched_dep_time)) %>%
  group_by(hour) %>%
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = hour, y = avg_dep_delay)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(y = "Average departure delay (in minutes)",
       x = "Hour of the day")
____________________
#5. On what day of the week should you leave if you want to minimise the chance of a delay?
flights_dt %>%
  mutate(dow = wday(sched_dep_time)) %>%
  group_by(dow) %>%
  summarise(
    dep_delay = mean(dep_delay),
    arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  print(n = Inf)
#Saturday has the lowest average departure delay time and the lowest average arrival delay time.
flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  group_by(wday) %>% 
  summarize(ave_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = wday, y = ave_dep_delay)) + 
  geom_bar(stat = "identity")
____________________
#6. What makes the distribution of diamonds$carat and flights$sched_dep_time similar?
ggplot(diamonds, aes(x = carat %% 1 * 100)) +
  geom_histogram(binwidth = 1)
#In scheduled departure times it is 00 and 30 minutes, and minutes ending in 0 and 5.
ggplot(flights_dt, aes(x = minute(sched_dep_time))) +
  geom_histogram(binwidth = 1)
____________________
#7. Confirm my hypothesis that the early departures of flights in minutes 20-30 and 50-60 are caused by scheduled flights that leave early. Hint: create a binary variable that tells you whether or not a flight was delayed.
flights_dt %>% 
  mutate(minute = minute(dep_time),
         early = dep_delay < 0) %>% 
  group_by(minute) %>% 
  summarise(
    early = mean(early, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, early)) +
  geom_line()
____________________
#######16.4.5 Exercises 

#1. Why is there months() but no dmonths()?
#Because months have differing numbers of days.
#31 days: January, March, May, July, August, October, December
#30 days: April, June, September, November
#28 or 29 days: February
____________________
#2. Explain days(overnight * 1) to someone who has just started learning R. How does it work?
#The variable overnight is equal to TRUE or FALSE. If it is an overnight flight, this becomes 1 day, and if not, then overnight = 0, and no days are added to the date.
____________________
#3. Create a vector of dates giving the first day of every month in 2015. Create a vector of dates giving the first day of every month in the current year.
#To get the vector of the first day of the month in 2015.
ymd("2015-01-01") + months(0:11)
#To get the vector of the first day of the month for this year.
floor_date(today(), unit = "year") + months(0:11)
____________________
#4. Write a function that given your birthday (as a date), returns how old you are in years.
age <- function(bday) {
  (bday %--% today()) %/% years(1)
}
age(ymd("1997-11-15"))
____________________
#5. Why can't (today() %--% (today() + years(1)) / months(1) work?
#The code in the question is missing a parentheses.
(today() %--% (today() + years(1))) / months(1)
#To find the number of months within an interval use %/% instead of /.
(today() %--% (today() + years(1))) %/% months(1)
