##### Challenge Putting it all together #####
library(tidyverse)
library(data.table)

##### Question 1 #####
# For each station, how many days have a minimum temp, a max
# temp, and a rainfall measurement recorded
bom_data <- read_csv("Data/BOM_data.csv")
bom_data

bom_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min","Temp_max"), sep="/") %>% 
  filter(Temp_min !="-") %>% # Filter on minimum temperature values
  filter(Temp_max !="-") %>% # Filter on maximum temperature values
  filter(Rainfall !="-") %>% # Filter on rainfall values
  group_by(Station_number) %>% # For each station
  summarise(Day=n()) # Number of days for each station 

##### Question 2 #####
# Which month saw the lowest average daily temperature difference?

bom_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min","Temp_max"), sep="/") %>% 
  filter(Temp_min !="-") %>% # Filter on minimum temperature values
  filter(Temp_max !="-") %>% # Filter on maximum temperature values
  select(Station_number, Year, Month, Day, Temp_min, Temp_max, Rainfall, Solar_exposure)
