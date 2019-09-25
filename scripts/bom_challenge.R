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
# 1. 

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
