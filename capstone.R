# capstone project
# conclusion could be - I have saved time
# no need to include code
# how about police data?
# strava could be cool
# townsville police data
# regions of high crime in qld?

# road deaths - this looks good
# https://www.bitre.gov.au/statistics/safety/fatal_road_crash_database.aspx

library(tidyverse)
library(readxl)

path = 'data/BITRE_ARDD_Fatalities_Apr_2019.xlsx'
road_data = read_excel(path = path, sheet = 2, na = c("", -9), skip = 4)
head(road_data)  
names(road_data)
str(road_data)

# Just use state:age columns

# trends over time
# plots showing interactions 
# model lines