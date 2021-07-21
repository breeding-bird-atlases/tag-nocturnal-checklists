library(here)
library(dplyr)
library(lubridate)
library(suncalc)

# update month and year to most recent ebd download
month <- "may"
year <- 2021

# results from bba3
bba3 <- read.delim(here("data", "ebird", "1_raw",
                        paste0("mddcbba3_", month, year, ".txt")), 
                   quote = "")
                   
## label nocturnal checklists
# nocturnal = 20 min after sunset, 40 min before sunrise
bba3 <- getSunlightTimes(data = data.frame(date = 
                                             as_date(bba3$observation_date),
                                           lat = bba3$latitude,
                                           lon = bba3$longitude),
                         keep = c("sunrise", 
                                  "nauticalDawn",
                                  "sunset", 
                                  "nauticalDusk"),
                         tz = "EST") %>%
  rename(nautical_dawn = nauticalDawn,
         nautical_dusk = nauticalDusk) %>%
  select(sunrise:nautical_dusk) %>%
  bind_cols(bba3, .) %>%
  # if there is NA time, only the date will be kept in the column
  mutate(datetime = as_datetime(paste(observation_date, 
                                      time_observations_started),
                                tz = "EST"))

# create new columns with suncalc info
bba3[, (length(bba3) + 1):(length(bba3) + 7)] <-   
  getSunlightTimes(data = data.frame(date = as.Date(bba3$observation_date),
                                     lat = bba3$latitude,
                                     lon = bba3$longitude), 
                   keep = c("sunrise", "nauticalDawn", 
                            "sunset", "nauticalDusk"), 
                   tz = "EST")
# remove duplicate columns
bba3[, c("date", "lat", "lon")] <- NULL

# getSunlightTimes() doesn't account for daylight savings time
# it uses the date and location to find the (non-DST) sunrise, etc time, so it 
# isn't informed by the time on the checklist.
dst <- list(
  as_datetime("2020-03-08 02:00:00", tz = "EST") %--%
    as_datetime("2020-11-01 02:00:00", tz = "EST"),
  
  as_datetime("2021-03-14 02:00:00", tz = "EST") %--%
    as_datetime("2021-11-07 02:00:00", tz = "EST"),
  
  as_datetime("2022-03-13 02:00:00", tz = "EST") %--%
    as_datetime("2022-11-06 02:00:00", tz = "EST"),
  
  as_datetime("2023-03-12 02:00:00", tz = "EST") %--%
    as_datetime("2023-11-05 02:00:00", tz = "EST"),
  
  as_datetime("2024-03-10 02:00:00", tz = "EST") %--%
    as_datetime("2024-11-03 02:00:00", tz = "EST")
)

# an NA time will be coerced to 1 AM if it isn't excluded
indx <- which(bba3$datetime %within% dst &
                !is.na(bba3$time_observations_started))

cols <- c("sunrise", "nautical_dawn", "sunset", "nautical_dusk")

bba3[indx, cols]<- lapply(bba3[indx, cols], function(x) x + hours(1))

rm(indx, cols)

# add columns for what ebird considers nocturnal
bba3 <- bba3 %>%
  mutate(ebird_dawn = sunrise - hms::as_hms(40*60),
         ebird_dusk = sunset + hms::as_hms(20*60),
         nocturnal = ifelse(datetime <= ebird_dawn |
                              datetime >= ebird_dusk,
                            "nocturnal", "diurnal"))

# datetime without time is assumed to be 0:00, which would be interpreted as
# nocturnal. Change these to NAs.
bba3$nocturnal[is.na(bba3$time_observations_started)] <- NA_character_
# check there are the same number of NA values in each column
sum(is.na(bba3$time_observations_started)) == sum(is.na(bba3$nocturnal))

# change the checklist's designation to diurnal if it extends past dawn
notna <- which(!is.na(bba3$duration_minutes) &
                 !is.na(bba3$time_observations_started))

indx <- which(bba3$nocturnal[notna] == "nocturnal" &
                ms(paste(bba3$duration_minutes[notna], 0)) > 
                abs(as_datetime(bba3$ebird_dawn[notna]) -
                      as_datetime(bba3$datetime[notna])))

bba3[indx, "nocturnal"] <- "diurnal"
                          
rm(indx, notna)
