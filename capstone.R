# capstone project
# conclusion could be - I have saved time
# how about police data?
# strava could be cool
# townsville police data
# regions of high crime in qld?

# road deaths - this looks good


library(tidyverse)
library(readxl)

# from here: https://www.bitre.gov.au/statistics/safety/fatal_road_crash_database.aspx
road_data = read_excel(path = 'data/BITRE_ARDD_Fatalities_Apr_2019.xlsx', sheet = 2, na = c("", -9), skip = 4)
# from here: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3105.0.65.0012016?OpenDocument
state_pop = read_excel('data/historical_state_pop.xls', sheet=4, skip = 4)
# the order is nsw, vic, qld, sa, wa, tas, nt, act  
names(state_pop)[1] = 'States'
state_pop = state_pop %>% 
  filter(str_detect(States, pattern="Estimated"))
state_pop = state_pop[-9,] # remove total Aust pop
State = c('NSW', 'Vic', 'Qld', 'SA', 'WA', 'Tas', 'NT', 'ACT')
state_pop$States = States
# only need from 1989
state_pop = select(state_pop, c(1, 20:47))
# need to get pop for 2017, 2018
# from pop projections https://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/3222.0Main+Features12017%20(base)%20-%202066?OpenDocument
nsw = c(7867052,	8001204)
vic = c(6320292,	6465890)
qld = c(4928412,	5013437)
sa = c(1723714,	1735172)
wa = c(2574762,	2598092)
tas = c(522279,	526930)
nt = c(247659,	249796)
act = c(411986,	420667)
state_pop_17_18 = rbind.data.frame(nsw, vic, qld, sa, wa, tas, nt, act)
names(state_pop_17_18) = c('2017', '2018')
state_pop = cbind(state_pop, state_pop_17_18)

# filter out 2019 data - not a complete year
road_data = filter(road_data, Year != 2019)
# Don't need Crash ID
road_data = road_data %>% select(-`Crash ID`)
road_data$State = factor(road_data$State, levels=c('NSW', 'Vic', 'Qld', 'WA', 'SA', 'Tas', 'ACT', 'NT'))

# need to make long form

# need to plot pop change over time by state 

# Just use state:age columns

# trends over time
# plots showing interactions 
# model lines

# make the year day etc into datetime
# need to look at working with time series in r
# and dates
# possibly one of those animated plots showing change over time

# get state populations so can normalize by pop - which is most dangerous


#need to count rows grouped by year
# then line plot over time faceted by state
state_p = ggplot(road_data, aes(x=Year, colour=State)) + 
  geom_line(stat="count") +
  ylab('Number of Deaths')
state_p

# double check
deaths_by_year_state = road_data %>% 
  group_by(State, Year) %>% 
  tally()
  

# Normalize for state population 
# plot population
state_pop_long = state_pop %>% 
  gather(key='Year', value='Population', 2:31)
names(state_pop_long)[1] = 'State'
state_pop_long$Year = as.numeric(state_pop_long$Year)
state_pop_long$State = factor(state_pop_long$State, levels = c('NSW', 'Vic', 'Qld', 'WA', 'SA', 'Tas', 'ACT', 'NT'))

pop_p = ggplot(state_pop_long, aes(x=Year, y=Population, colour=State)) +
  geom_line() +
  # log axis??
pop_p

d2018 = road_data %>% 
  filter(Year==2018) %>% 
  group_by(State) %>% 
  tally()

norm_2018 = d2018$n / state_pop 

# need to get state population by year - create another df

norm_2018_10k = norm_2018 * 10000 # number of deaths per 10,000 people 
norm_2018_10k = as.data.frame(t(norm_2018_10k)) # NT is by far the most dangerous






