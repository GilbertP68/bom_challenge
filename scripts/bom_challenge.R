##### Challenge Putting it all together #####
library(tidyverse)
library(data.table)

##### Question 1 #####
# For each station, how many days have a minimum temp, a max
# temp, and a rainfall measurement recorded

#--- Observation ---#
# 1. Check the contents of the table - (especially the data type, column headings)
# 2. Columns "Temp_min_max", "Rainfall", and "Solar_exposure" must be cleaned
#    because they contain columns integer, double and other characters ( such as - and /)
# 3. The data in the columns mentioned above is of type character.

#--- Solution ---#
#--- Data cleaning ---# 
# 1. Split the column "Temp_min_max" into 2 columns named as "Temp_min" and "Temp_max"
# 2. Replace all characters in "Temp_min_max", "Rainfall", and "Solar_exposure" that 
#    are non-numeric as "na"
# 3. Write this table as a new dataframe and save it in a new folder "Clean_Data" in 
#    the folder "Data"
#
#--- Answer the question ---#


bom_data_clean <- bom_raw %>% 
  separate(col = Temp_min_max, into = c("Temp_min","Temp_max"), sep="/") %>% # Split columns Temp_min_max into Temp_min and Temp_max
  mutate(Temp_min = ifelse(str_detect(Temp_min,"-"),yes = NA,no = Temp_min), # Replacing "-" with NA in Temp_min
         Temp_max = ifelse(str_detect(Temp_max,"-"),yes = NA,no = Temp_max))%>% # # Replacing "-" with NA in Temp_max
  mutate_at(vars(Temp_min, Temp_max, Solar_exposure), as.numeric) %>%
  write_csv("Data/bom_data_clean.csv")

#----- Filtering on numeric data -----#
bom_data_clean%>%  
  group_by(Station_number)%>%
  filter(Temp_min >= 0, Temp_max >= 0, Rainfall >= 0) %>%  # Filter on numeric data in Temp_min, Temp_max, Rainfall
  summarise(Day=n()) %>%  # Number of days for each station 
  write_csv("Results/bom_data_days_with_measurements.csv")


##### Question 2 #####
# Which month saw the lowest average daily temperature difference?

bom_data_clean %>%  
  filter(Temp_min >= 0, Temp_max >= 0) %>%
  mutate(Temp_diff = Temp_max - Temp_min) %>% 
  group_by(Month) %>% 
  summarise(mean_Temp_diff = mean(Temp_diff)) %>% 
  arrange(mean_Temp_diff) %>%
  slice(1) %>% 
  write_csv("Results/month_with_lowest_mean_diff.csv")
  #select(Month, min(mean_Temp_diff))
  # month_lowest_mean_daily_Temp_diff <- mean_Temp_diff)     
    
##### Question 3 ##### 
# Which state saw the lowest average daily temperature difference?
bom_stations_raw <- read_csv("Data/BOM_stations.csv")
bom_stations_raw 

bom_stations_raw %>% 
  gather(Station_number, data, -info) %>%
  spread(info, data) %>% 
  select(Station_number, state, start, end, elev, lat, lon, name) %>% 
  group_by(state) %>% 
  write_csv("Data/bom_stations_raw.csv")

meteo_data <- read_csv("Data/bom_data_clean.csv",
                       col_types= cols(
                         Station_number = col_double(),
                         Year = col_double(),
                         Month = col_double(),
                         Day = col_double(),
                         Temp_min = col_double(),
                         Temp_max = col_double(),
                         Rainfall = col_double(),
                         Solar_exposure = col_double()
                       ))
meteo_data

stations_data <- read_csv("Data/bom_stations_raw.csv")
stations_data

stations_meteo_merged <- full_join(meteo_data, stations_data, by = c("Station_number" = "Station_number"))
stations_meteo_merged

stations_meteo_merged %>% 
write_csv("Data/stations_meteo_merged.csv")

stations_meteo_merged %>%  
  filter(Temp_min >= 0, Temp_max >= 0) %>%
  mutate(Temp_diff = Temp_max - Temp_min) %>% 
  group_by(state) %>% 
  summarise(mean_Temp_diff = mean(Temp_diff)) %>% 
  arrange(mean_Temp_diff) %>%
  slice(1)  %>% 
  write_csv("Results/state_with_lowest_mean_diff.csv")


##### Question 4 ##### 
# Does the westmost (lowest longitude) or eastmost (highest longitude)
# weather station in our dataset have a higher average solar exposure?

stations_ew <- stations_meteo_merged %>%
    select(lon, Solar_exposure, Station_number) %>%
    filter(!is.na(Solar_exposure), # Filtering on Solar exposure that excludes "NA" and on "longitude"
          lon %in% c(max(lon), min(lon))) %>% # that will display only maximum and minimum longitudes
    group_by(lon, Station_number) %>%
    summarise(mean_Solar_exposure = mean(Solar_exposure))%>%
    ungroup %>% 
    arrange(lon)
    
    ifelse(stations_ew$mean_Solar_exposure[1] > stations_ew$mean_Solar_exposure[2],
       "WESTMOST has a higher average solar exposure than EASTMOST",
       "EASTMOST has a higher average solar exposure than WESTMOST")

##### Same as above but the dataframe was not assigned to a variable so as we can see the results #####
stations_meteo_merged  %>% 
  select(lon, Solar_exposure, Station_number) %>%
  filter(!is.na(Solar_exposure),  # Filtering on solar exposure that excludes "NA" 
        lon %in% c(max(lon), min(lon))) %>% # as well as on longitude that will display maximum and minimum values
  group_by(lon, Station_number) %>% 
  summarise(mean_Solar_exposure = mean(Solar_exposure))
# ungroup %>% ##### It looks like ungroup is unnecessary!
# arrange(lon) ##### Same applies to "arrange(lon)", which is unnecessay!

    
    
    
    
############# Challenge 09.10.19 ##############
##### Question 1 #####
 
       
perth <- filter(stations_meteo_merged, Station_number == 9225)
                
perth    
    
##### Temp_max vs Temp_min
ggplot(data = perth,
       mapping = aes(x = Temp_min,
                   y = Temp_max)) +
  geom_point()
  
##### Temp_max vs Rainfall
ggplot(data = perth,
       mapping = aes(x = Temp_max,
                     y = Rainfall)) +
  geom_point()

##### Temp_max vs solar exposure
ggplot(data = perth,
       mapping = aes(x = Solar_exposure,
                     y = Temp_max)) +
  geom_point()


##### Question 2 #####
ggplot(data = perth,
       mapping = aes(x = Temp_min,
                     y = Temp_max,
                     size = Rainfall,
                     colour = Solar_exposure)) +
  geom_point()


########## Question 3 ##########
install.packages("cowplot")
library(cowplot)

##### Temp_max vs Temp_min
TpMin_Max <- ggplot(data = perth,
       mapping = aes(x = Temp_min,
                     y = Temp_max)) +
  geom_point()

##### Temp_max vs Rainfall
TpMax_Rain <- ggplot(data = perth,
       mapping = aes(x = Temp_max,
                     y = Rainfall)) +
  geom_point()

##### Temp_max vs solar exposure
TpMax_Solar <- ggplot(data = perth,
       mapping = aes(x = Solar_exposure,
                     y = Temp_max)) +
  geom_point()


##### All in One #####
all_in_One <- ggplot(data = perth,
       mapping = aes(x = Temp_min,
                     y = Temp_max,
                     size = Rainfall,
                     colour = Solar_exposure)) +
  geom_point()

plot_grid(TpMin_Max, TpMax_Rain, TpMax_Solar, all_in_One)


##### Question 4 #####
# Calculate the average monthly rainfall for each station
# Produce a line plot to visualise this data and the state 

state_station_month_rain <- stations_meteo_merged  %>%
  select(state, Station_number, name, Month, Rainfall) %>%
  filter(!is.na(Rainfall)) %>% 
  group_by(state, Station_number, Month) %>% 
  summarise(mean_rainfall = mean(Rainfall)) %>% 
  ungroup() 
 
mthlist <- month.abb[c(seq(1,12,1))]
      
rainBymonth_state<- ggplot(data = state_station_month_rain,
       mapping = aes(x = Month,
                     y = mean_rainfall,
                     group = Station_number,
                     colour = Station_number,
                     )) +
  geom_line()+
    scale_x_continuous(limits= c(1,12), breaks = c(seq(1,12,1)), label = mthlist)+
  facet_grid(state~.)+
  labs(title = "Average monthly rainfall by station by state",
       x = "Month",
       y = "Mean rainfall (mm)")+
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black", size = 0.2))

rainBymonth_state + guides(
  fill = guide_legend(
  title.theme = element_text(
  size = 2,
  colour = "red"
)))
