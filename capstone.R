# capstone project
# conclusion could be - I have saved time
# how about police data?
# strava could be cool
# townsville police data
# regions of high crime in qld?

# road deaths - this looks good


library(tidyverse)
library(readxl)
theme_set(theme_minimal())

# Import road death data
# from here: https://www.bitre.gov.au/statistics/safety/fatal_road_crash_database.aspx
road_data = read_excel(path = 'data/BITRE_ARDD_Fatalities_Apr_2019.xlsx', sheet = 2, na = c("", -9), skip = 4)
# filter out 2019 data - not a complete year
road_data = filter(road_data, Year != 2019)
# Don't need Crash ID
road_data = road_data %>% select(-`Crash ID`)
road_data$State = factor(road_data$State, levels=c('NSW', 'Vic', 'Qld', 'WA', 'SA', 'Tas', 'ACT', 'NT'))
road_data$Dayweek = factor(road_data$Dayweek, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

# Import population data
# from here: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3105.0.65.0012016?OpenDocument
state_pop = read_excel('data/historical_state_pop.xls', sheet=4, skip = 4)
# the order is nsw, vic, qld, sa, wa, tas, nt, act  
names(state_pop)[1] = 'States'
state_pop = state_pop %>% 
  filter(str_detect(States, pattern="Estimated"))
States = c('NSW', 'Vic', 'Qld', 'SA', 'WA', 'Tas', 'NT', 'ACT', 'Total')
state_pop$States = States
# only need from 1989
state_pop = select(state_pop, c(1, 20:47))
# need to get pop for 2017, 2018
# from pop projections https://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/3222.0Main+Features12017%20(base)%20-%202066?OpenDocument
# used Series B projections, which is based on current birth rate
nsw = c(7867052,	8001204)
vic = c(6320292,	6465890)
qld = c(4928412,	5013437)
sa = c(1723714,	1735172)
wa = c(2574762,	2598092)
tas = c(522279,	526930)
nt = c(247659,	249796)
act = c(411986,	420667)
total = c(24600777,	25015825)
state_pop_17_18 = rbind.data.frame(nsw, vic, qld, sa, wa, tas, nt, act, total)
names(state_pop_17_18) = c('2017', '2018')
state_pop = cbind(state_pop, state_pop_17_18)





# Just use state:age columns

# trends over time
# plots showing interactions 
# model lines

# get state populations so can normalize by pop - which is most dangerous


# Total deaths by year, month, day
year_total_p = ggplot(road_data, aes(x=Year)) +
  geom_line(stat = 'count') +
  ylab('Number of Deaths')
year_total_p

mth_total_p = ggplot(road_data, aes(x=Month)) +
  geom_line(stat = 'count') +
  ylab('Number of Deaths') +
  scale_x_continuous(breaks = c(1:12), labels=month.abb) 
mth_total_p

day_total_p = ggplot(road_data, aes(x=Dayweek, group=1)) +
  geom_line(stat = 'count') +
  ylab('Number of Deaths') +
  scale_x_discrete(labels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))
day_total_p

#need to count rows grouped by year
# then line plot over time faceted by state
year_p = ggplot(road_data, aes(x=Year, colour=State)) + 
  geom_line(stat="count") +
  ylab('Number of Deaths')
year_p

# Deaths grouped by State
deaths_by_year_state = road_data %>% 
  group_by(State, Year) %>% 
  tally(name = 'Deaths') 

total_deaths = road_data %>% 
  group_by(Year) %>% 
  tally(name = 'Deaths') 

total_deaths$State = 'Total'
deaths_by_year_state = rbind.data.frame(deaths_by_year_state, total_deaths)
deaths_by_year_state$State = factor(deaths_by_year_state$State, levels = c('NSW', 'Vic', 'Qld', 'WA', 'SA', 'Tas', 'ACT', 'NT', 'Total'))

state_pop_long = state_pop %>% 
  gather(key='Year', value='Population', 2:31)
names(state_pop_long)[1] = 'State'
state_pop_long$Year = as.numeric(state_pop_long$Year)
state_pop_long$State = factor(state_pop_long$State, levels = c('NSW', 'Vic', 'Qld', 'WA', 'SA', 'Tas', 'ACT', 'NT', 'Total'))

# join deaths with state population info
death_pop = full_join(deaths_by_year_state, state_pop_long, by=c('State', 'Year'))

# remove total for these graphs
death_pop_states = death_pop %>% 
  filter(State !='Total') 
death_pop_states$State =  fct_drop(death_pop_states$State)

state_pop_p = ggplot(death_pop_states, aes(x=Year, y=Population/1000000, colour=State)) +
  geom_line() +
  ylab('Population (millions)')
state_pop_p

# Normalize for state population 
death_pop_states = death_pop_states %>% 
  mutate(death_proportion = Deaths / Population) %>% 
  mutate(death_prop_per_10k = death_proportion * 10000)

# plot proportions
prop_p_10k = ggplot(death_pop_states, aes(x=Year, y=death_prop_per_10k, colour=State)) + 
  geom_line() +
  geom_smooth(method = 'loess', se=F) +
  ylab('Number of Deaths per 10,000 People')
prop_p_10k

#


# testing size by proportion
year_p2 = ggplot(death_pop_states, aes(x=Year, y=Deaths, colour=State)) + 
  geom_line(aes(size=death_prop_per_10k)) +
  #geom_smooth(method = 'loess') +
  ylab('Number of Deaths per 10,000 People')
year_p2
# doesn't look good but an example of what I might be able to do

# think about how to investigate whether xmas and easter periods are more dangerous

# could try number of deaths by month animated by year
library(gganimate)
library(gifski)

library(devtools)
install_github('thomasp85/transformr')
library(transformr)

month_p = ggplot(road_data, aes(x=Month, colour=State, frame=Year)) + 
  geom_line(stat="count") +
  ylab('Number of Deaths') +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                     labels=c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D')) +
  # gganimate code
  ggtitle("Year: {frame_time}") +
  transition_time(Year) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

animate(month_p, width = 450, height = 450)


# and day of week animated by month

# number of deaths can be a size of a point also??

## Report
# conclusion
# could be used to inform public policy 
# find out how australia compares with other countries - calculte total stats for Aus and compare

# intro - aim is to gain inssights from road death data that may help to shape public policy

# include r script as appendix





