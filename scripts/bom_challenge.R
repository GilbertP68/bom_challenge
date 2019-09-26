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

bom_raw <- read_csv("Data/BOM_data.csv", na = "-",
                     col_types = cols(
                       Station_number = col_double(),
                       Year = col_double(),
                       Month = col_double(),
                       Day = col_double(),
                       Temp_min_max = col_character(),
                       Rainfall = col_double(),
                       Solar_exposure = col_double() # The col_character for Solar_exposure has been changed to col_double as type
                     ))


bom_data_clean <- bom_raw %>% 
 separate(col = Temp_min_max, into = c("Temp_min","Temp_max"), sep="/") %>% # Split columns Temp_min_max into Temp_min and Temp_max
 mutate(Temp_min = ifelse(str_detect(Temp_min,"-"),yes = NA,no = Temp_min), # Replacing "-" with NA in Temp_min
        Temp_max = ifelse(str_detect(Temp_max,"-"),yes = NA,no = Temp_max))%>% # # Replacing "-" with NA in Temp_max
 mutate_at(vars(Temp_min, Temp_max), as.numeric) %>% 
 write_csv("Data/bom_data_clean.csv") 

#----- Filtering on numeric data -----#
bom_data_clean%>%  
  group_by(Station_number)%>%
  filter(Temp_min >= 0, Temp_max >= 0, Rainfall >= 0) %>%  # Filter on numeric data in Temp_min, Temp_max, Rainfall
  summarise(Day=n()) %>%  # Number of days for each station 
  write_csv("Results/bom_data_dats_with_measurements")


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


  