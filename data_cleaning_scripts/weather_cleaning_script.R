library(tidyverse)
library(here)
library(readr)

january <- read_delim(here("raw_data/weather_data/january_weather1.txt"), delim = "=", col_names = FALSE)
february <- read_delim(here("raw_data/weather_data/february_weather.txt"), delim = "=", col_names = FALSE)
march <- read_delim(here("raw_data/weather_data/march_weather.txt"), delim = "=", col_names = FALSE)
april <- read_delim(here("raw_data/weather_data/april_weather.txt"), delim = "=", col_names = FALSE)
may <- read_delim(here("raw_data/weather_data/may_weather.txt"), delim = "=", col_names = FALSE) 
june <- read_delim(here("raw_data/weather_data/june_weather.txt"), delim = "=", col_names = FALSE)
july <- read_delim(here("raw_data/weather_data/july_weather.txt"), delim = "=", col_names = FALSE)
august <- read_delim(here("raw_data/weather_data/august_weather.txt"), delim = "=", col_names = FALSE)
september <- read_delim(here("raw_data/weather_data/september_weather.txt"), delim = "=", col_names = FALSE)
october <- read_delim(here("raw_data/weather_data/october_weather.txt"), delim = "=", col_names = FALSE)
november <- read_delim(here("raw_data/weather_data/november_weather.txt"), delim = "=", col_names = FALSE)
december <- read_delim(here("raw_data/weather_data/december_weather.txt"), delim = "=", col_names = FALSE)


#The file with weather information have more nulls than data.
#The only historical information that I could find, come in a text file and need a lot text process berfore to use them,
#the process_weather function does that process. 
process_weather <- function(weather){ 
  weather <- weather %>%
    pivot_longer(cols = 1:ncol(weather),
                 names_to = "names",
                 values_to = "data") %>%
    select(-names) %>%
    drop_na()
  
month_weather <- weather %>%
  mutate(origin = "EWR") %>%
  mutate(year = str_sub(data, start = 2, end = 5)) %>%
  mutate(day = str_sub(data, start = 8, end = 9)) %>%
  mutate(month = str_sub(data, start = 6, end = 7)) %>%
  mutate(hour = str_sub(data, start = 10, end = 11)) %>%
  mutate(minute = str_sub(data, start = 12, end = 13)) %>%
  mutate(hour = as.integer(hour) + 1) %>%
  mutate(date = make_datetime(as.integer(year), as.integer(month), as.integer(day), as.integer(hour))) %>%
  select(- c(year, day, month, hour, minute)) %>%
  mutate(temp_dew = str_extract(data, pattern = "M?[0-9]{2}/M?[0-9]{2}")) %>%
  separate(col = temp_dew,
           into = c("temp", "dewp"),
           sep = "/") %>%
  mutate(temp = str_replace(temp, pattern = "M", "-")) %>%
  mutate(dewp = str_replace(dewp, pattern = "M", "-")) %>%
  mutate(wind = str_extract(data, pattern = "[0-9]{5}G?([0-9][0-9])?KT")) %>%
  mutate(wind = ifelse(is.na(wind),
                       str_extract(data, pattern = "VRB[0-9]{2}G?([0-9][0-9])?KT"),
                       wind)) %>%
  mutate(wind_dir = str_sub(wind, start = 1, end = 3)) %>%
  mutate(wind_speed = str_sub(wind, start = 4, end = 5)) %>%
  mutate(wind_gust = ifelse(str_detect(wind, pattern = "G"), 
                             str_sub(wind, start = 7, end = 8),
                            0)) %>%
  mutate(wind_dir = ifelse(str_detect(wind_dir, pattern = "VRB"), -1.5, wind_dir)) %>%
  mutate(precipitation = str_extract(data, pattern = "RA")) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = "SN"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = "IC"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = "GR"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = "UP"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = "DZ"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = "SG"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = "PL"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = "GS"), precipitation)) %>%
  mutate(precipitation = str_replace(precipitation, pattern = "RA", "Rain")) %>%
  mutate(precipitation = str_replace(precipitation, pattern = "SN", "Snow")) %>%
  mutate(precipitation = str_replace(precipitation, pattern = "IC", "Ice Crystals")) %>%
  mutate(precipitation = str_replace(precipitation, pattern = "GR", "Hail")) %>%
  mutate(precipitation = str_replace(precipitation, pattern = "UP", "Unknown Precipitation")) %>%
  mutate(precipitation = str_replace(precipitation, pattern = "DZ", "Drizzle")) %>%
  mutate(precipitation = str_replace(precipitation, pattern = "SG", "Snow Grains")) %>%
  mutate(precipitation = str_replace(precipitation, pattern = "PL", "Ice Pellets")) %>%
  mutate(precipitation = str_replace(precipitation, pattern = "GS", "Snow Pellets")) %>%
  mutate(precipitation = ifelse(is.na(precipitation), "None", precipitation)) %>%
  mutate(amount_precipitation = str_extract(data, pattern = " P[0-9]{4}")) %>%
  mutate(amount_precipitation = str_replace(amount_precipitation, pattern = " P", "")) %>%
  mutate(amount_precipitation = ifelse(is.na(amount_precipitation), "0", amount_precipitation)) %>%
  mutate(amount_precipitation = as.numeric(amount_precipitation) / 100) %>%
  mutate(visibility = str_extract(data, pattern = "[0-9]/?[0-9]?SM")) %>%
  mutate(visibility = str_replace(visibility, pattern = "SM", "")) %>%
  mutate(count = nchar(visibility)) %>%
  mutate(visibility = ifelse(count >2, 
                             as.numeric(str_sub(visibility, start = 1, end = 1))/ 
                               as.numeric(str_sub(visibility, start = 3, end = 3)),
                             visibility)) %>%
  mutate(pressure = str_extract(data, pattern = " A[0-9]{4}")) %>%
  mutate(pressure = str_replace(pressure, pattern = " A", "")) %>%
  mutate(pressure = as.numeric(pressure) / 100) %>%
  mutate_at(c("temp", "dewp", "wind_dir", "wind_speed", "wind_gust", "amount_precipitation", "visibility"), 
            as.numeric) %>%
  select(-c(data, wind, count)) %>%
  drop_na()
#I delete 18 rows because they have missing data.
return(month_weather)
}
#We process month by moth because it was the easier way to obtain the information and control the number of columns
january <- process_weather(january)

february <- process_weather(february)

march <- process_weather(march)

april <- process_weather(april)

may <- process_weather(may)

june <- process_weather(june)

july <- process_weather(july)

august <- process_weather(august)

september <- process_weather(september)

october <- process_weather(october)

november <- process_weather(november)

december <- process_weather(december)
#Here we unit the different data sets
weather <- rbind(january, february, march, april, may, june, july, august, september, october, november, december)
#Here we make the file for cleaning data
write_csv(weather, "clean_data/weather.csv")
