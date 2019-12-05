library(tidyverse)
library(rvest)
library(readr)

##input accident data
data_2018 = 
  read_csv("./data/2018data.csv") %>% 
  janitor::clean_names() 
newnames = colnames(data_2018) %>% 
  str_replace("number_of_","") 
names(data_2018) = newnames

tidy_data = 
  data_2018 %>% 
  mutate(
    date_complete = date 
  ) %>% 
  separate(date, into = c("month", "day", "year"), sep = "/") %>% 
  separate(time, into = c("hour", "minute"), sep = ":") %>%
  select(-zip_code, -location, -on_street_name, -cross_street_name, -off_street_name,-collision_id,-year) %>% 
  rename("vehicle_type" = "vehicle_type_code_1") %>% 
  mutate( day = as.numeric(day),
          month = as.numeric(month),
          hour = as.numeric(hour),
          minute = as.numeric(minute),
          latitude = replace_na(latitude,0),
          vehicle_type = str_to_lower(vehicle_type)
  ) %>%
  filter( latitude != 0)

## number of broken light and signal per day
traffic_street_light = read_csv(file = "./large_data/311_Service_Requests_from_2010_to_Present.csv") %>%
  janitor::clean_names() %>%
  select(created_date, closed_date, complaint_type, descriptor, incident_zip, borough, latitude, longitude) %>%
  separate(created_date, into = c("created_month", "created_day", "created_year"), sep = "([\\/\\ \\:])") %>%
  separate(closed_date, into = c("closed_month", "closed_day", "closed_year"), sep = "([\\/\\ \\:])") %>%
  filter(created_year == 2018 | closed_year == 2018) %>%
  mutate(created_day = as.numeric(created_day),
         created_month = as.numeric(created_month),
         created_year = as.numeric(created_year),
         closed_day = as.numeric(closed_day),
         closed_month = as.numeric(closed_month),
         closed_year = as.numeric(closed_year))
traffic_street_light = traffic_street_light %>%
  filter(created_year == closed_year & ((created_month < closed_month)|(created_month == closed_month & created_day <= closed_day))) %>%
  bind_rows(traffic_street_light %>% filter(created_year < closed_year))

## combine data of accident and traffic signal

whole_accident = tidy_data %>%
  drop_na(borough, month, day) %>%
  group_by(day, month, borough) %>%
  summarize(num_accident = n())
signal = traffic_street_light %>%
  filter(complaint_type == "Traffic Signal Condition") %>%
  drop_na(created_year, closed_year, created_month, closed_month, created_day, closed_day, borough) 
whole_accident = whole_accident %>% 
  ungroup() %>%
  mutate(num_signal = rep(0,nrow(whole_accident)))
for (i in 1:nrow(whole_accident)){
  day = whole_accident[i,] %>% pull(.,day)
  month = whole_accident[i,] %>% pull(.,month)
  year = 2018
  borough1 = whole_accident[i,] %>% pull(.,borough)
  filter_n = signal %>% 
    filter(created_year == year & ((created_month < month)|(created_month == month & created_day <= day))) %>%
    bind_rows(signal %>% filter(created_year < year)) 
  filter_n = filter_n %>%
    filter(year == closed_year & ((month < closed_month) |(month == closed_month & day <= closed_day))) %>%
    bind_rows(filter_n %>% filter(year < closed_year)) %>%
    filter(borough == borough1) %>%
    nrow()
  whole_accident[i,]$num_signal = filter_n
}
write.csv(whole_accident,file="./data/whole_accident.csv",append=F)

## combine data of accident and street light

night_accident = tidy_data %>%
  drop_na(borough, month, day) %>%
  filter(hour >= 18 | hour <= 5) %>%
  group_by(day, month, borough) %>%
  summarize(num_accident = n()) 
night_accident_0 = left_join(whole_accident, night_accident, by = c("month","day","borough")) %>% 
  filter(is.na(num_accident.y)) %>%
  select(day, month, borough) %>%
  mutate(num_accident = rep(0,4))
night_accident = bind_rows(night_accident, night_accident_0)
light = traffic_street_light %>%
  filter(complaint_type == "Street Light Condition") %>%
  drop_na(created_year, closed_year, created_month, closed_month, created_day, closed_day, borough) 
night_accident = night_accident %>% 
  ungroup() %>%
  mutate(num_light = rep(0,nrow(night_accident)))
for (i in 1:nrow(night_accident)){
  day = night_accident[i,] %>% pull(.,day)
  month = night_accident[i,] %>% pull(.,month)
  year = 2018
  borough1 = night_accident[i,] %>% pull(.,borough)
  filter_n = light %>% 
    filter(created_year == year & ((created_month < month)|(created_month == month & created_day <= day))) %>%
    bind_rows(light %>% filter(created_year < year)) 
  filter_n = filter_n %>%
    filter(year == closed_year & ((month < closed_month) |(month == closed_month & day <= closed_day))) %>%
    bind_rows(filter_n %>% filter(year < closed_year)) %>%
    filter(borough == borough1) %>%
    nrow()
  night_accident[i,]$num_light = filter_n
}
write.csv(night_accident,file="./data/night_accident.csv",append=F)