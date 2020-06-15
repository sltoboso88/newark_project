library(tidyverse)
library(here)
library(janitor)
library(lubridate)

#In airport are 3 columns with nulls but they are relative with time zone and I don't need this 
#information for this project, so I just will drop them

airports <- read_csv(here("raw_data/airports.csv"))

airports <- airports %>%
  select(-c(tzone, tz, dst))

airports %>%
  filter(lat < -90 | lat > 90) 

airports %>%
  filter(lon < -180 | lon > 180) 

#the values for latitude and longitude are valid so I can work with this table

write_csv(airports, "clean_data/airport.csv")

flights <- read_csv(here("raw_data/flights.csv"))


#The NA datas in the columns dep_time and dep_delay are cancelation of flights and you can see that in this web 
#https://www.transtats.bts.gov/HomeDrillChart.asp So I will change the NA for a 2500 value because it is a values that isn't
#in the column dep_time and dep_delay. 
flights <- flights %>%
  mutate(dep_delay = case_when(is.na(dep_delay) & is.na(dep_time) ~ 2500,
                               is.na(dep_delay) & !is.na(dep_time)  ~ 0,
                               !is.na(dep_delay) ~ dep_delay)) %>%
  mutate(dep_time = ifelse(is.na(dep_time), 2500, dep_time)) %>%
  #The nulls in tailnum are relative with flights that has been canceled 
  mutate(tailnum = ifelse(is.na(tailnum), "CANCELLED", tailnum)) %>%
  #the time that we have in the weather table was taking 51 minutes after the hour, we will do a 
  #new hour column it will have the hour schedule flightif it is lower than 21 or the next hour if it is higher or eaual than 21
  mutate(hour = ifelse(minute >= 21, hour + 1, hour)) %>%
  #we will create a column date with the year, day, month and hour in this way will be easir join the tables weather
  mutate(date = make_datetime(year, month, day, hour)) %>%
  #For this project I don't need the information about arr_time, sched_arr_time, arr_delay and air_time
  #because they aren't a reason for a delay, they are causing because the delay. 
  select(-c(arr_time, arr_delay, sched_arr_time, air_time, year, month, day, hour, minute, time_hour)) 




#All the planes that are in the planes table are in the flight table but a lot flights that are in flights table aren't in plane.
#I don't need the speed and also it is a column with a lot missing data so I will drop it. The engine electric is relative with drones,
#and here we are dealing with flights so I will delete them.

planes <- read_csv(here("raw_data/planes.csv"))
#Here is some data that let me complete part of the some missing values in the year of built column, some flight don't have any 
#information about the year of built so I will asume that year as 2020, it is a year that isn't in this column.


tailnum_d <- planes %>%
  filter(engine == "Electric") %>%
  inner_join(flights, by = "tailnum") %>%
  select(tailnum)
#631 flights are drones and in this particular case we will only considering the flights that aren't drones. The 16.7% of this 
#flights were delay
flights <- flights %>%
  anti_join(tailnum_d, by = "tailnum")


#aux_planes is a file that have some information special the year about some planes that have some missing 
#information in the original plane file. 
aux_planes <- read_csv(here("raw_data/aux_planes.csv"))

aux_finder <- function(aux_planes, value){
  x <- aux_planes %>%
    filter(tailnum == value) %>%
    select(year_built)
  return(as.numeric(x[1]))
}


aux_planes <- aux_planes %>%
  clean_names() %>%
  mutate(year_built = ifelse(is.na(year_built), 2100, year_built))

planes <- planes %>%
  rowwise() %>%
  mutate(year = ifelse(tailnum %in% aux_planes$tailnum, aux_finder(aux_planes, tailnum), year)) %>%
  drop_na(year) %>%
  select(-speed)


total_delays <- flights %>%
  filter(dep_delay != 2500) %>%
  filter(dep_delay >= 15) %>%
  count()

flight_a <- flights %>%
  anti_join(planes, by = "tailnum")
#615 unique values 
#delay is when the delay time is 15 of greater
delays_w_tn <- flight_a %>%
  filter(dep_delay != 2500) %>%
  filter(dep_delay >= 15) %>%
  count()
#8529 total delays withouth tailnum total flights without tailnum 38997 22% 
#proportion of flights that are delays and don't have tailnum
proportion <- delays_w_tn[1] / total_delays[1] #0.095

#we will not delete the flights that don't have information in the planes table because they could be security flight so we will
#include them in the model, we will put different simbols for recognice them
flight_a <- unique(flight_a$tailnum)

flight_a <- flight_a %>%
  as.tibble() %>%
  rename(tailnum = value) %>%
  rowwise() %>%
  mutate(year = 2100) %>%
  mutate(type = "Unknown") %>%
  mutate(manufacturer = "Unknown") %>%
  mutate(model = "Unknown") %>%
  mutate(engines = 0) %>%
  mutate(seats = 0) %>%
  mutate(engine = "Unknown")


planes <- rbind(planes, flight_a)

write_csv(planes, "clean_data/planes.csv")
write_csv(flights, "clean_data/flights.csv")
