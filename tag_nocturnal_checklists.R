library(here)
library(dplyr)
library(lubridate)
library(suncalc)

# results from bba3
bba3 <- read.delim(here("data", "ebird", "1_raw",
                        paste0("mddcbba3_", month, year, ".txt")), 
                   quote = "")
                   
## label nocturnal checklists
# nocturnal = 20 min after sunset, 40 min before sunrise
bba3$observation_datetime <- as_datetime(paste(bba3$observation_date, 
                                               bba3$time_observations_started), 
                                         tz = "EST")
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
## compensate for this
### DST starts March 8 2020 02:00:00
### DST ends November 1 2020 02:00:00
dst_2020 <- as_datetime("2020-03-08 02:00:00", tz = "EST") %--% 
  as_datetime("2020-11-01 02:00:00", tz = "EST")
dst_2021 <- as_datetime("2021-03-14 02:00:00", tz = "EST") %--%
  as_datetime("2021-11-07 02:00:00", tz = "EST")

indx <- which(bba3$observation_datetime %within% dst_2020 |
                bba3$observation_datetime %within% dst_2021)
bba3[indx, c("sunrise")] <- bba3[indx, c("sunrise")] + hours(1)
bba3[indx, c("nauticalDawn")] <- bba3[indx, c("nauticalDawn")] + hours(1)
bba3[indx, c("sunset")] <- bba3[indx, c("sunset")] + hours(1)
bba3[indx, c("nauticalDusk")] <- bba3[indx, c("nauticalDusk")] + hours(1)

rm(indx)

# add columns for what ebird considers nocturnal
bba3$ebird_dawn <- bba3$sunrise - hms::as_hms(40*60)
bba3$ebird_dusk <- bba3$sunset + hms::as_hms(20*60)
bba3 <- bba3 %>% 
  rename(nautical_dawn = nauticalDawn, nautical_dusk = nauticalDusk) %>%
  mutate(nocturnal = if_else(observation_datetime <= ebird_dawn | 
                                 observation_datetime >= ebird_dusk, 
                             "nocturnal", "diurnal"))

# change the checklist's designation to diurnal if it extends past dawn
notna <-which(!is.na(bba3$duration_minutes))
indx <- which(bba3$nocturnal[notna] == "nocturnal" &
                ms(paste(bba3$duration_minutes[notna], 0)) > 
                abs(as_datetime(bba3$ebird_dawn[notna]) -
                      as_datetime(bba3$observation_datetime[notna])))
bba3[indx, "nocturnal"] <- "diurnal"
rm(indx, notna)
