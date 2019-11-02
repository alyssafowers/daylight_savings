library(tidyverse)
library(janitor)
library(lubridate)
library(maptools)


#getting data, with a little help from https://www.r-bloggers.com/ggplot2-plotting-dates-hours-and-minutes/

x_seq <- seq(from = as.POSIXct("2019-02-01", tz = "EST"),length.out = 365, by = "days")
coord <- matrix(c(-80.33, 25.73), nrow = 1)

sunrise <- sunriset(coord, x_seq, direction = "sunrise",POSIXct.out = TRUE)
sunrise$hms <- format(sunrise$time, format = "%H:%M:%S")
sunrise$hms <- as.POSIXct(sunrise$hms, format = "%H:%M:%S")
sunrise$date <- as.character(sunrise$time) %>% strsplit(" ", fixed = TRUE) %>% 
  lapply(function(x) x[1]) %>% unlist() %>% ymd()

sunrise$dst <- sunrise$hms

for(i in 1:nrow(sunrise)){
  if(sunrise[i,"date"] >= as.Date("2019-03-10") & sunrise[i, "date"] <= as.Date("2019-11-02")){
    sunrise[i, "dst"] <- sunrise[i, "hms"] - hours(1)
  }
}
  
colnames(sunrise) <- c("sunrise_day_frac", "sunrise_time", "sunrise_hms", "date", "sunrise_dst")
#sunrise$type = "sunrise"

sunset <- sunriset(coord, x_seq, direction = "sunset",POSIXct.out = TRUE)
sunset$hms <- format(sunset$time, format = "%H:%M:%S")
sunset$hms <- as.POSIXct(sunset$hms, format = "%H:%M:%S")
sunset$date <- as.character(sunset$time) %>% strsplit(" ", fixed = TRUE) %>% 
  lapply(function(x) x[1]) %>% unlist() %>% ymd()
sunset$dst <- sunset$hms
for(i in 1:nrow(sunset)){
  if(sunset[i,"date"] >= as.Date("2019-03-10") & sunset[i, "date"] <= as.Date("2019-11-02")){
    sunset[i, "dst"] <- sunset[i, "hms"] - hours(1)
  }
}

colnames(sunset) <- c("sunset_day_frac", "sunset_time", "sunset_hms", "date", "sunset_dst")
#sunset$type = "sunset"

solarnoon <- solarnoon(coord, x_seq,POSIXct.out = TRUE)
solarnoon$hms <- format(solarnoon$time, format = "%H:%M:%S")
solarnoon$hms <- as.POSIXct(solarnoon$hms, format = "%H:%M:%S")
solarnoon$date <- as.character(solarnoon$time) %>% strsplit(" ", fixed = TRUE) %>% 
  lapply(function(x) x[1]) %>% unlist() %>% ymd()
solarnoon$dst <- solarnoon$hms
for(i in 1:nrow(solarnoon)){
  if(solarnoon[i,"date"] >= as.Date("2019-03-10") & solarnoon[i, "date"] <= as.Date("2019-11-02")){
    solarnoon[i, "dst"] <- solarnoon[i, "hms"] - hours(1)
  }
}
colnames(solarnoon) <- c("solarnoon_day_frac", "solarnoon_time", "solarnoon_hms", "date", "solarnoon_dst")
#solarnoon$type = "solarnoon"

sun <- inner_join(sunrise, sunset)
sun <- inner_join(sun, solarnoon)

#using astronomical dawn and dusk (18 degrees below horizon)

dawn <- crepuscule(coord, x_seq, solarDep = 18, direction = "dawn", POSIXct.out = TRUE)
dawn$hms <- format(dawn$time, format = "%H:%M:%S")
dawn$hms <- as.POSIXct(dawn$hms, format = "%H:%M:%S")
dawn$date <- as.character(dawn$time) %>% strsplit(" ", fixed = TRUE) %>% 
  lapply(function(x) x[1]) %>% unlist() %>% ymd()
dawn$dst <- dawn$hms
for(i in 1:nrow(dawn)){
  if(dawn[i,"date"] >= as.Date("2019-03-10") & dawn[i, "date"] <= as.Date("2019-11-02")){
    dawn[i, "dst"] <- dawn[i, "hms"] - hours(1)
  }
}
colnames(dawn) <- c("dawn_day_frac", "dawn_time", "dawn_hms", "date", "dawn_dst")
#dawn$type <- "dawn"

dusk <- crepuscule(coord, x_seq, solarDep = 18, direction = "dusk", POSIXct.out = TRUE)
dusk$hms <- format(dusk$time, format = "%H:%M:%S")
dusk$hms <- as.POSIXct(dusk$hms, format = "%H:%M:%S")
dusk$date <- as.character(dusk$time) %>% strsplit(" ", fixed = TRUE) %>% 
  lapply(function(x) x[1]) %>% unlist() %>% ymd()
dusk$dst <- dusk$hms
for(i in 1:nrow(dusk)){
  if(dusk[i,"date"] >= as.Date("2019-03-10") & dusk[i, "date"] <= as.Date("2019-11-02")){
    dusk[i, "dst"] <- dusk[i, "hms"] - hours(1)
  }
}
colnames(dusk) <- c("dusk_day_frac", "dusk_time", "dusk_hms", "date", "dusk_dst")
#dusk$type <- "dusk"

sun <- inner_join(sun, dawn)
sun <- inner_join(sun, dusk)

sun_tidy_time <- sun %>% 
                  select(date, sunrise_dst, sunset_dst, dawn_dst,dusk_dst, solarnoon_dst) %>%
                  pivot_longer(cols = c(sunrise_dst, sunset_dst, dawn_dst,dusk_dst, solarnoon_dst),
                               names_to = "time", values_to = "hms")

ggplot()+geom_path(data = sun_tidy_time, aes(x = date, y = hms, group = time))+
  scale_y_datetime(breaks = "1 hour")+scale_x_date(date_breaks = "1 month", labels = NULL) + theme_classic()








###old scratch, do not use

#when I was tinkering with intervals:

#calculating fractions of day in each interval:

sun$before_dawn <- sun$dawn_day_frac
sun$dawn_to_sunrise <- sun$sunrise_day_frac - sun$dawn_day_frac
sun$sunrise_to_noon <- sun$solarnoon_day_frac - sun$sunrise_day_frac
sun$noon_to_sunset <- sun$sunset_day_frac - sun$solarnoon_day_frac
sun$sunset_to_dusk <- sun$dusk_day_frac - sun$sunset_day_frac
sun$after_dusk <- 1 - sun$dusk_day_frac

#sanity check (should be 1):
sum(sun$before_dawn, sun$dawn_to_sunrise, sun$sunrise_to_sunset, 
    sun$sunset_to_dusk, sun$after_dusk)/nrow(sun)

sun_tidy_interval <- sun %>% select(date, before_dawn, dawn_to_sunrise, sunrise_to_noon, noon_to_sunset,
                                    sunset_to_dusk, after_dusk) %>% 
  pivot_longer(cols = c(before_dawn, dawn_to_sunrise, 
                        sunrise_to_noon, noon_to_sunset, sunset_to_dusk, after_dusk), names_to = "interval",
               values_to = "day_frac")

sun_tidy_interval$interval_order <- factor(sun_tidy_interval$interval, levels = c("before_dawn",
                                                                                  "dawn_to_sunrise", "sunrise_to_noon", "noon_to_sunset", "sunset_to_dusk",
                                                                                  "after_dusk"))

ggplot()+geom_area(data = sun_tidy_interval, aes(x = date, y = day_frac, fill = interval_order))



ggplot(data = sun) + geom_line(aes(x = date, y = dawn_hms)) + 
  geom_line(aes(x = date, y = sunrise_hms))+
  geom_line(aes(x = date, y = solarnoon_hms)) +
  geom_line(aes(x = date, y = sunset_hms)) +
  geom_line(aes(x = date, y = dusk_hms)) +
  scale_y_datetime()

sun_tidy <- rbind(sunrise, sunset, dusk, dawn)


sun <- clean_names(read_csv("miami_raw_data.csv"))

sun$date <- mdy(paste(sun$month, sun$day, "2019", sep = "/"))

#extracting times from weird text strings
sun$sunrise <- sun$sunrise_raw %>% strsplit(" am", fixed = TRUE) %>% 
  lapply(function(x) x[1]) %>% unlist() %>% hm()
  
sun$sunset <- sun$sunset_raw %>% strsplit(" pm", fixed = TRUE) %>% 
  lapply(function(x) x[1]) %>% unlist() %>% hm() + hours(12)

#formatting time so ggplot can read it

sunset_char <- format(sun$sunset, format = "%H:%M:%S")


as.POSIXct(sunset_char, format = "%H:%M:%S")

ggplot(data = sun, aes(x = date, y = sunset))+geom_line()
