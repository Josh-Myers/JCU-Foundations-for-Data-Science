# FDS capstone project
library(tidyverse)
library(readxl)
library(ggthemes)
library(lubridate)
theme_set(theme_minimal())

# Import road death data
# from here: https://www.bitre.gov.au/statistics/safety/fatal_road_crash_database.aspx
road_data = read_excel(path = 'data/BITRE_ARDD_Fatalities_Apr_2019.xlsx', sheet = 2, na = c("", -9, 'Other/-9'), skip = 4)
# filter out 2019 data - not a complete year
road_data = filter(road_data, Year != 2019)
# Don't need Crash ID
road_data = road_data %>% select(-`Crash ID`)
road_data$State = factor(road_data$State, levels=c('NSW', 'Vic', 'Qld', 'WA', 'SA', 'Tas', 'ACT', 'NT'))
road_data$Dayweek = factor(road_data$Dayweek, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

# Describe NA in dataset here
road_data$Gender[road_data$Gender == 'Unspecified'] = NA

# extract hour of day from Time column
time_of_day <- ymd_hms(road_data$Time)
road_data$time_of_day <- hour(time_of_day) + minute(time_of_day)/60
# Deaths by date
# date_data = read_excel(path='data/BITRE_ARDD_Fatalities_Apr_2019.xlsx', sheet = 3, na = c("", -9), skip = 2, col_types = c('date', 'numeric', 'numeric',
#                                                                                                                        'text', 'text'))
# date_data$Date = as.Date(date_data$Date)
# date_p = ggplot(date_data, aes(x=Date, y=`Number Fatalities`)) +
#   geom_point(alpha=0.01) +
#   scale_x_date() 
# date_p

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

# from 2018 mv census https://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/9309.0Main+Features131%20Jan%202018?OpenDocument
mv_names = c('Passenger', 'CamperVan', 'LightCommercial', 'LightRigid', 'HeavyRigid', 'Articulated', 'Non-Freight', 'Bus', 'Motorcycle', 'Total')
mv_census = c(14330432, 66594, 3187131, 158032, 346966, 100694, 24165, 98565, 860700,  19173279)    
# levels of road user are: Car/Truck, Cyclist, Motorcycle, Pedestrian - so compine into car/truck/bus vs MC
# are MC more at risk - can then normalize - remove cyclist, pedestrian etc from User
names(mv_census) = mv_names

mc = mv_census[9]
total = mv_census[10]
mc_prop = mc/total # so MC account for 5% of vehicles on the road
bus = mv_census[8]
bus_prop = bus/total
heavy_rigid = mv_census[5]
heavy_rigid_prop = heavy_rigid/total
articulated = mv_census[6]
articulated_prop = articulated/total
prop_pop = rbind(mc_prop, bus_prop, heavy_rigid_prop, articulated_prop)

# Just use state:age columns

# trends over time
# plots showing interactions 
# model lines

# get state populations so can normalize by pop - which is most dangerous

# year, month day by state plots
# fig 1 (a, b, c)
year_p = ggplot(road_data, aes(x=Year, colour=State)) + 
  geom_line(stat="count") +
  ylab('Number of Deaths') +
  scale_colour_colorblind() 
year_p

mth_p = ggplot(road_data, aes(x=Month, colour=State)) +
  geom_line(stat = 'count') +
  ylab('Number of Deaths') +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_colour_colorblind() 
mth_p

day_p = ggplot(road_data, aes(x=Dayweek, group=State, colour=State)) +
  geom_line(stat = 'count') +
  ylab('Number of Deaths') +
  scale_x_discrete(labels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')) +
  scale_colour_colorblind() 
day_p

road_data$hour_int = as.integer(road_data$time_of_day)  
# 12 pm to 11am for plotting
road_data$hour_int = factor(road_data$hour_int, levels = c(0:23)) 
summary(road_data$hour_int)
# 40 na - remove
road_data_hour = road_data %>% 
  drop_na(hour_int)

hour_p = ggplot(road_data_hour, aes(x=hour_int, group=State, colour=State)) +
  geom_line(stat = 'count') +
  ylab('Number of Deaths') +
  scale_color_colorblind()
hour_p
  
# Make df deaths grouped by State
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

# join deaths with state population info (so can normalize)
death_pop = full_join(deaths_by_year_state, state_pop_long, by=c('State', 'Year'))

# remove total for these graphs
death_pop_states = death_pop %>% 
  filter(State !='Total') 
death_pop_states$State = fct_drop(death_pop_states$State)

# plot population by year for the States - probabily don't need to include this
state_pop_p = ggplot(death_pop_states, aes(x=Year, y=Population/1000000, colour=State)) + # millions
  geom_line() +
  ylab('Population (millions)') +
  scale_colour_colorblind() 
state_pop_p

# Normalize for state population 
death_pop_states = death_pop_states %>% 
  mutate(death_proportion = Deaths / Population) %>% 
  mutate(death_prop_per_10k = death_proportion * 10000)

# plot proportions
prop_p_10k = ggplot(death_pop_states, aes(x=Year, y=death_prop_per_10k, colour=State)) + 
  geom_line(alpha=0.5) +
  geom_smooth(method = 'loess', se=F) +
  ylab('Number of Deaths per 10,000 People') +
  scale_colour_colorblind() 
prop_p_10k

# age by gender
age_sex_df = road_data %>% 
  drop_na(Gender)
  
age_p = ggplot(age_sex_df, aes(x=Gender, y=Age, colour=Gender)) +
  #geom_boxplot() +
  geom_violin() +
  geom_point(position=position_jitterdodge(), alpha=0.01) +
  scale_colour_colorblind() +
  theme(legend.position = 'none')
age_p
  
# road user
summary(as.factor(road_data$`Road User`))
# 77 na

road_data = road_data %>% 
  mutate(User = ifelse(`Road User`=='Motorcycle pillion passenger'|`Road User`=='Motorcycle rider', 'Motorcycle',
                       ifelse(`Road User`=='Driver'|`Road User`=='Passenger', 'Car/Truck', 
                              ifelse(`Road User`=='Pedestrian', 'Pedestrian', 
                                     ifelse(`Road User`=='Pedal cyclist', 'Cyclist', NA)))))

user_p = road_data %>% 
  drop_na(User) %>%  # drop 77 NA
  factor(User, levels = c('Car/Truck', 'Motorcycle', ))
  ggplot(aes(x=Year, colour=User)) +
  geom_line(stat = 'count')
user_p

# what proportion of deaths in 2018 were MC?
# remove pedestrian and cyclist
data_2018 = road_data %>% 
  filter(Year==2018, User != 'Pedestrian', User != 'Cyclist') 

mc_2018 = summary(as.factor(data_2018$User))
car_truck_2018 = mc_2018[1]
mc_2018 = mc_2018[2]
total_2018 = car_truck_2018 + mc_2018

prop_car_truck_deaths_2018 = car_truck_2018/total_2018
mc_prop_2018 = mc_2018/total_2018 # Only 5% of vehicles on the road are mc, but they account for 20% of mv deaths 

# proportion of trucks involved in crash compared with census 2018
# bus, heavy rigid, articulated trucks
# are any of these overrepresented?
df_2018 = road_data %>% 
  filter(Year==2018) 
bus_deaths_2018 = summary(as.factor(df_2018$`Bus Involvement`))[2]
total = nrow(bus_deaths_2018_data)
bus_prop_2018 = bus_deaths_2018/total # 0.02 of deaths, but 0.005 of vehicles on road # not heavily overrepresented (4 times as many) - and driving a lot of the time

heavy_rigid_deaths_2018 = summary(as.factor(df_2018$`Heavy Rigid Truck Involvement`))[2]
heavy_rigid_prop_2018 = heavy_rigid_deaths_2018 / total # 0.06 of deaths (6%) and account for 0.018 (~2%) of vehicles (overrepresented 3 times as many)

articulated_deaths_2018 = summary(as.factor(df_2018$`Articulated Truck Involvement`))[2]
articulated_prop_2018 = articulated_deaths_2018 / total # 0.079 of deaths (~8%) and account for 0.005 of vehicles (heavily overrepresented 16 times as many)

deaths_2018 = c(mc_prop_2018, bus_prop_2018, heavy_rigid_prop_2018, articulated_prop_2018)
props_2018 = cbind.data.frame(prop_pop, deaths_2018)
names(props_2018) = c('Population', 'Deaths')
props_2018$user = c('Motorcycle', 'Bus', 'Heavy Rigid Truck', 'Articulated Truck')

# need to make long
props_2018 = props_2018 %>% 
  gather('Group', 'Proportion', 1:2) %>% 
  ggplot(aes(x=factor(user, levels=c('Bus', 'Heavy Rigid Truck', 'Articulated Truck', 'Motorcycle')), y=Proportion, colour=Group, fill=Group)) +
  geom_bar(stat='identity', position = 'dodge') +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  xlab('Type of Vehicle')
props_2018

# speed limit
speed_box_p = road_data %>% 
  drop_na(c(`Speed Limit`,Gender)) %>% # drop 1340 NA speed limit and 23 na gender
  ggplot(aes(x=Gender, y=`Speed Limit`, colour=Gender)) +
  geom_violin() +
  geom_point(position = position_jitterdodge(), alpha=0.01)
speed_box_p

# males vs females - far more males die on the road
sex_p = road_data %>% 
  drop_na(Gender) %>%  # rm 23 obs na for gender
  ggplot(aes(x=Year, colour=Gender)) +
  geom_line(stat = 'count') +
  scale_colour_colorblind()
sex_p

# barplot x=speed, colour=sex
sex_bar_p = road_data %>% 
  drop_na(c(Gender, `Speed Limit`)) %>% 
  ggplot(aes(x=`Speed Limit`, fill=Gender)) +
  geom_bar(position = 'dodge') +
  scale_fill_colorblind()
sex_bar_p
  
# make age discrete
# 0-17, 18-40, 40-60, 60+
age_p = road_data %>% 
  drop_na(Age, Gender) %>% 
  mutate(age_cat = cut(Age, breaks = c(-Inf, 16, 40, 60, Inf), labels = c('0-16', '17-40', '41-60', '>60'))) # %>% 
  ggplot(aes(x=Year, colour=age_cat, linetype=Gender)) +
  geom_line(stat = 'count') +
  scale_fill_colorblind()
age_p # young men dominate the numbers - improving, but still highest 
 
# christmas and easter
# Xmas period is 12 days beginning Dec 23
# Easter period is 5 days beginning thursday before good friday
# total = 17 days / 365
# summary(as.factor(road_data$`Christmas Period`))
# summary(as.factor(road_data$`Easter Period`))
# 
# road_data = road_data %>% 
#   mutate(Is_holiday = ifelse(`Christmas Period`=='Yes' | `Easter Period`=='Yes', 'Yes', 'No'))
# 
# # tally deaths
# holiday_df = road_data %>% 
#   group_by(Is_holiday, Year) %>% 
#   tally(name = 'Deaths') 
# 
# # create number of deaths per day
# holiday_df = holiday_df %>% 
#   mutate(Deaths_per_day = ifelse(Is_holiday=='Yes', Deaths/17, Deaths/348)) # to get deaths per day if holiday /17 (17 days in holiday period), elso /348 (365-17)
# 
# holiday_p = ggplot(holiday_df, aes(x=Year, y=Deaths_per_day, colour=Is_holiday)) +
#   geom_line()
# holiday_p



## Report
# intro
# investigate the motorvehicle death data - look for trends over time and over-represeted, at-risk groups, compare with other country data

# conclusion
# males more at risk
# mc more at risk of death that other vehicle types
# busses, heavy rigid and articulated trucks all overrepreseted, articulated trucks heavily so

# could be used to inform public policy - target males and mc riders - improve safety for mc riders
# NT has improved, but still higher risk of death than other states
# steady deterioration over time due to improved safety features (cite), effective government campaigns???
# 

# find out how australia compares with other countries - calculte total stats for Aus and compare



# intro - aim is to gain inssights from road death data that may help to shape public policy

# include r script as appendix





