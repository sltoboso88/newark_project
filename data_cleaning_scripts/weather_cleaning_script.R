library(tidyverse)
library(here)
library(readr)
library(lubridate)
#Download the files by airport and moths 
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

january_jfk <- read_delim(here("raw_data/weather_data/KJFK_January.txt"), delim = "=", col_names = FALSE)
february_jfk <- read_delim(here("raw_data/weather_data/KJFK_February.txt"), delim = "=", col_names = FALSE)
march_jfk <- read_delim(here("raw_data/weather_data/KJFK_March.txt"), delim = "=", col_names = FALSE)
april_jfk <- read_delim(here("raw_data/weather_data/KJFK_April.txt"), delim = "=", col_names = FALSE)
may_jfk <- read_delim(here("raw_data/weather_data/KJFK_May.txt"), delim = "=", col_names = FALSE) 
june_jfk <- read_delim(here("raw_data/weather_data/KJFK_June.txt"), delim = "=", col_names = FALSE)
july_jfk <- read_delim(here("raw_data/weather_data/KJFK_July.txt"), delim = "=", col_names = FALSE)
august_jfk <- read_delim(here("raw_data/weather_data/KJFK_August.txt"), delim = "=", col_names = FALSE)
september_jfk <- read_delim(here("raw_data/weather_data/KJFK_September.txt"), delim = "=", col_names = FALSE)
october_jfk <- read_delim(here("raw_data/weather_data/KJFK_October.txt"), delim = "=", col_names = FALSE)
november_jfk <- read_delim(here("raw_data/weather_data/KJFK_November.txt"), delim = "=", col_names = FALSE)
december_jfk <- read_delim(here("raw_data/weather_data/KJFK_December.txt"), delim = "=", col_names = FALSE)

january_lga <- read_delim(here("raw_data/weather_data/KLGA_January.txt"), delim = "=", col_names = FALSE)
february_lga <- read_delim(here("raw_data/weather_data/KLGA_February.txt"), delim = "=", col_names = FALSE)
march_lga <- read_delim(here("raw_data/weather_data/KLGA_March.txt"), delim = "=", col_names = FALSE)
april_lga <- read_delim(here("raw_data/weather_data/KLGA_April.txt"), delim = "=", col_names = FALSE)
may_lga <- read_delim(here("raw_data/weather_data/KLGA_May.txt"), delim = "=", col_names = FALSE) 
june_lga <- read_delim(here("raw_data/weather_data/KLGA_June.txt"), delim = "=", col_names = FALSE)
july_lga <- read_delim(here("raw_data/weather_data/KLGA_July.txt"), delim = "=", col_names = FALSE)
august_lga <- read_delim(here("raw_data/weather_data/KLGA_August.txt"), delim = "=", col_names = FALSE)
september_lga <- read_delim(here("raw_data/weather_data/KLGA_September.txt"), delim = "=", col_names = FALSE)
october_lga <- read_delim(here("raw_data/weather_data/KLGA_October.txt"), delim = "=", col_names = FALSE)
november_lga <- read_delim(here("raw_data/weather_data/KLGA_November.txt"), delim = "=", col_names = FALSE)
december_lga <- read_delim(here("raw_data/weather_data/KLGA_December.txt"), delim = "=", col_names = FALSE)

#The file with weather information have more nulls than data.
#The only historical information that I could find, come in a text file and need a lot text process berfore to use them,
#the process_weather function does that process. 
process_weather <- function(weather, string){ 
  weather <- weather %>%
    pivot_longer(cols = 1:ncol(weather),
                 names_to = "names",
                 values_to = "data") %>%
    select(-names) %>%
    drop_na()
  
month_weather <- weather %>%
  mutate(origin = string) %>%
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
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " SN"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " IC"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " GR"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " UP"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " DZ"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " SG"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " PL"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " GS"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " -SN"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " -IC"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " -GR"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " -UP"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " -DZ"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " -SG"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " -PL"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " -GS"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " +SN"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " +IC"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " +GR"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " +UP"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " +DZ"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " +SG"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " +PL"), precipitation)) %>%
  mutate(precipitation = ifelse(is.na(precipitation), str_extract(data, pattern = " +GS"), precipitation)) %>%
  mutate(precipitation = str_replace(precipitation, pattern = " -", "")) %>%
  mutate(precipitation = str_replace(precipitation, pattern = " +", "")) %>%
  mutate(precipitation = str_replace(precipitation, pattern = " ", "")) %>%
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
january <- process_weather(january, "EWR")
february <- process_weather(february, "EWR")
march <- process_weather(march, "EWR")
april <- process_weather(april, "EWR")
may <- process_weather(may, "EWR")
june <- process_weather(june, "EWR")
july <- process_weather(july, "EWR")
august <- process_weather(august, "EWR")
september <- process_weather(september, "EWR")
october <- process_weather(october, "EWR")
november <- process_weather(november, "EWR")
december <- process_weather(december, "EWR")

january_jfk <- process_weather(january_jfk, "JFK")
february_jfk <- process_weather(february_jfk, "JFK")
march_jfk <- process_weather(march_jfk, "JFK")
april_jfk <- process_weather(april_jfk, "JFK")
may_jfk <- process_weather(may_jfk, "JFK")
june_jfk <- process_weather(june_jfk, "JFK")
july_jfk <- process_weather(july_jfk, "JFK")
august_jfk <- process_weather(august_jfk, "JFK")
september_jfk <- process_weather(september_jfk, "JFK")
october_jfk <- process_weather(october_jfk, "JFK")
november_jfk <- process_weather(november_jfk, "JFK")
december_jfk <- process_weather(december_jfk, "JFK")

january_lga <- process_weather(january_lga, "LGA")
february_lga <- process_weather(february_lga, "LGA")
march_lga <- process_weather(march_lga, "LGA")
april_lga <- process_weather(april_lga, "LGA")
may_lga <- process_weather(may_lga, "LGA")
june_lga <- process_weather(june_lga, "LGA")
july_lga <- process_weather(july_lga, "LGA")
august_lga <- process_weather(august_lga, "LGA")
september_lga <- process_weather(september_lga, "LGA")
october_lga <- process_weather(october_lga, "LGA")
november_lga <- process_weather(november_lga, "LGA")
december_lga <- process_weather(december_lga, "LGA")

#Here we unit the different moths and airports
weather <- rbind(january, february, march, april, may, june, july, august, september, october, november, december,
                 january_jfk, february_jfk, march_jfk, april_jfk, may_jfk, june_jfk, july_jfk, august_jfk, 
                 september_jfk,october_jfk, november_jfk, december_jfk, january_lga, february_lga, march_lga, 
                 april_lga, may_lga, june_lga, july_lga, august_lga, september_lga, october_lga, 
                 november_lga, december_lga)

#Here we make the file for cleaning data


write_csv(weather, "clean_data/weather.csv")
