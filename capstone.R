# FDS capstone project
library(tidyverse)
library(ggthemes)
library(lubridate)
#library(Hmisc)
library(mice)
theme_set(theme_minimal())

# Import road death data
# from here: https://www.bitre.gov.au/statistics/safety/fatal_road_crash_database.aspx
path = 'https://data.gov.au/data/dataset/5b530fb8-526e-4fbf-b0f6-aa24e84e4277/resource/fd646fdc-7788-4bea-a736-e4aeb0dd09a8/download/bitre_ardd_fatalities_apr_2019.csv'
road_data = read.csv(path, header = T, na.strings = c("", -9, 'Other/-9'))
# filter out 2019 data - not a complete year
road_data = filter(road_data, Year != 2019)
# Don't need Crash ID
road_data = road_data %>% select(-Crash.ID)
road_data$State = factor(road_data$State, levels=c('NSW', 'Vic', 'Qld', 'WA', 'SA', 'Tas', 'ACT', 'NT'))
road_data$Month = as.integer(road_data$Month)
road_data$Dayweek = factor(road_data$Dayweek, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
# extract hour of day from Time column
road_data$Bus.Involvement = as.factor(road_data$Bus.Involvement)
road_data$Heavy.Rigid.Truck.Involvement = as.factor(road_data$Heavy.Rigid.Truck.Involvement)
road_data$Articulated.Truck.Involvement = as.factor(road_data$Articulated.Truck.Involvement)
road_data$Gender[road_data$Gender == 'Unspecified'] = NA
road_data$Gender = droplevels(road_data$Gender)
road_data$Gender = as.factor(road_data$Gender)
road_data$Road.User = as.factor(road_data$Road.User)
#describe(road_data)
# state 0 missing
# month 0 missing
# year 0 missing
# day 0 missing
# time 40 missing - drop
# crash type 0 missing - drop
# bus involvement 0 missing
# heavy rigid truck involvement many missing (20493) - but I only use 2018 data
truck_2018 = road_data %>% 
  filter(Year==2018) %>% 
  summarise(count = sum(is.na(Heavy.Rigid.Truck.Involvement)))
truck_2018 # none missing for 2018
# articultated truck involevelment 0 missing
# speed limit 1340 missing
# road user 77 missing
# gender 22 missing
# age 84 missing
# national remoteness area 45567 missing - drop this column
# national LGA Name 2017 45553 missing - drop - not using
# national road type - 45564 missing - drop
# Xmas period 0 missing
# Easter period 0 missing

# now deal with time
time_of_day <- hm(road_data$Time)
road_data$time_of_day <- hour(time_of_day) + minute(time_of_day)/60

road_data = road_data %>% 
  select(c(State:Dayweek, Bus.Involvement:Articulated.Truck.Involvement, Road.User:Age, time_of_day))
road_data = rename(road_data, bus_involve=Bus.Involvement, heavy_rigid_involve=Heavy.Rigid.Truck.Involvement, 
                          articulated_involve=Articulated.Truck.Involvement, road_user=Road.User) 
init = mice(road_data, maxit=0) # need to impute road user (77), gender (22), age (84), time (40)
meth = init$method
predM = init$predictorMatrix

# don't use bus involve, heavy rigid involve, or articulated involve pas predictor variables
predM[, c('bus_involve')]=0
predM[, c('heavy_rigid_involve')]=0
predM[, c('articulated_involve')]=0

# don't need to impute heavy rigid
meth[c('heavy_rigid_involve')]=""

# set methods for imputation
meth[c('road_user')]="polyreg" 
meth[c('Gender')]="logreg" 
meth[c('Age')]="pmm"
meth[c('time_of_day')]="pmm"

imputed = mice(road_data, method=meth, predictorMatrix=predM, m=5)
densityplot(imputed) # can I somehow plot the categorical variables? see https://web.maths.unsw.edu.au/~dwarton/missingDataLab.html
plot(imputed)
road_data <- complete(imputed)
sapply(road_data, function(x) sum(is.na(x)))

# make age discrete
# 0-17, 18-40, 40-60, 60+
road_data$Gender = factor(road_data$Gender, levels = c('Male', 'Female'))
road_data = road_data %>% 
  mutate(age_cat = cut(Age, breaks = c(-Inf, 16, 40, 60, Inf), labels = c('0-16 years', '17-40 years', '41-60 years', '>60 years'))) 

# create 5-year intervals as factor
five_years = c('1989 to 1993', '1994 to 1998', '1999 to 2003', '2004 to 2008', '2009 to 2013', '2014 to 2018')
road_data = road_data %>% 
  mutate(five_yr_interval = cut(Year, breaks = c(-Inf, 1993, 1998, 2003, 2008, 2013, Inf),
                                labels=five_years)) 

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

# year, month, day by state plots
state_year_p = road_data %>% 
  #mutate_at(vars(Year, State), factor) %>%
  group_by(Year, State, age_cat, Gender) %>% 
  summarise(n=n()) %>%
  ggplot(aes(x=Year, y=n, group=State, colour=State, fill=State)) + 
  geom_line(alpha=0.5) +
  geom_smooth(se=F, size=0.5, method = 'loess') +
  ylab('Number of Deaths') +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015), labels = c('1990', '1995', '2000', '2005', '2010', '2015')) +
  scale_colour_colorblind() +
  scale_fill_colorblind() +
  ggtitle('Number of Deaths for Each State (1989 to 2018)') +
  facet_grid(age_cat~Gender) +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) 
state_year_p

# Make df deaths grouped by State
deaths_by_year_state = road_data %>% 
  group_by(State, Year, age_cat, Gender) %>% 
  tally(name = 'Deaths') 

total_deaths = road_data %>% 
  group_by(Year, age_cat, Gender) %>% 
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
# state_pop_p = ggplot(death_pop_states, aes(x=Year, y=Population/1000000, colour=State)) + # millions
#   geom_line() +
#   ylab('Population (millions)') +
#   scale_y_log10() +
#   scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015), labels = c('1990', '1995', '2000', '2005', '2010', '2015')) +
#   scale_colour_colorblind() +
#   ggtitle('Population Growth of Each State (1989 to 2018)') +
#   theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) 
# state_pop_p

# Normalize for state population 
death_pop_states = death_pop_states %>% 
  mutate(death_proportion = Deaths / Population) %>% 
  mutate(death_prop_per_10k = death_proportion * 100000)

# plot proportions
# prop_p_10k = ggplot(death_pop_states, aes(x=Year, y=death_prop_per_10k, colour=State)) + 
#   geom_line(alpha=0.5) +
#   geom_smooth(method = 'loess', se=F) +
#   ylab('Number of Deaths per 10,000 People') +
#   scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015), labels = c('1990', '1995', '2000', '2005', '2010', '2015')) +
#   scale_colour_colorblind() +
#   ggtitle('Number of Deaths in Each State per 10,000 people (1989 to 2018)') +
#   theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) 
# prop_p_10k # NT is overrepreseted

#state_plots = plot_grid(state_year_p, state_pop_p, prop_p_10k, labels = c('A', 'B', 'C'), nrow = 1)

prop_p_100k_sex_age = ggplot(death_pop_states, aes(x=Year, y=death_prop_per_10k, colour=State)) + 
  geom_line(alpha=0.5) +
  geom_smooth(method = 'loess', se=F, size=0.5) +
  ylab('Number of Deaths per 100,000') +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015), labels = c('1990', '1995', '2000', '2005', '2010', '2015')) +
  scale_colour_colorblind() +
  ggtitle('Number of Deaths per 100,000 people (1989 to 2018)') +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_y_log10() +
  facet_grid(age_cat~Gender)
prop_p_100k_sex_age # NT young people especially males are overrepreseted

age_p = ggplot(road_data, aes(x=Year, colour=age_cat, linetype=Gender)) +
  geom_line(stat = 'count') +
  scale_colour_colorblind() +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015), labels = c('1990', '1995', '2000', '2005', '2010', '2015')) +
  labs(colour='Age Group') +
  ylab('Number of Deaths') +
  ggtitle('Number of Deaths Each Year by Age and Gender') +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) 
age_p # young men dominate the numbers - seem overrepresented - check against population stats for each age group - improving, but still highest 

# proportion of population each age in 2016: https://profile.id.com.au/australia/five-year-age-groups
zero_to_4 = 0.06
five_to_9 = 0.06
ten_to_14 = 0.06
fifteen_to_19 = 0.06
twenty_to_24 = 0.07
twenty5_to_29 = 0.07
thirty_to_34 = 0.07
thirty5_to_39 = 0.07
forty_to_44 = 0.07
forty5_to_49 = 0.07
fifty_to_54 = 0.07
fifty5_to_59 = 0.06
sixty_to_64 = 0.06
sixty5_to_69 = 0.05
seventy_to_74 = 0.04
seventy5_to79 = 0.03
eighty_to_84 = 0.02
eighty5_and_over = 0.02

# so 15 to 39 
prop_twenty_to_39_male = (twenty_to_24 + twenty5_to_29 + thirty_to_34 + thirty5_to_39)/2
# 14% of population 20 to 39 male in 2016
# (males are approximately 50% of population in this age group)
# https://www.abs.gov.au/ausstats/abs@.nsf/0/1CD2B1952AFC5E7ACA257298000F2E76?OpenDocument
# but accounted for what proportion of deaths?
twenty_to_39_deaths = road_data %>% 
  filter(Year==2016) %>% 
  group_by(Age, Gender) %>% 
  tally() 

total_deaths_2016 = twenty_to_39_deaths %>% 
  summarise(total=sum(n)) %>% 
  summarise(total=sum(total)) # 1293 deaths in 2016

age_names = c('0 to 19', '20 to 39', '>=40' )
age_deaths_2016 = twenty_to_39_deaths %>% 
  mutate(Age_20_39 = cut(Age, breaks = c(-Inf, 19, 39, Inf), labels = age_names)) %>% 
  group_by(Age_20_39, Gender) %>% 
  tally()
twenty_to_39_male_deaths_2016 = filter(age_deaths_2016, Age_20_39=='20 to 39', Gender=='Male') 
# 364 deaths male (110 female)
Prop_male_deaths_2016 = twenty_to_39_male_deaths_2016$n/total_deaths_2016$total # 28% of deaths, but only 14% of population

# month~age~gender faceted by 1/2 decades
mth_p = road_data %>%
  group_by(Month, five_yr_interval, age_cat, Gender) %>%
  summarise(count=n()) %>%
  mutate(five_yr_ave = count/5)  %>%
  ggplot(aes(x=Month, y=five_yr_ave, group=five_yr_interval, colour=five_yr_interval)) +
  geom_line() +
  ylab('Number of Deaths') +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_log10() +
  scale_colour_colorblind() +
  scale_fill_colorblind() +
  labs(fill="Five-Year Interval") +
  labs(colour='Five-Year Interval') +
  facet_grid(age_cat~Gender) +
  ggtitle('Number of Deaths by Month') +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) 
mth_p # trend upwards in December

# day~age~gender of week
day_p = road_data %>% 
  group_by(Dayweek, five_yr_interval, age_cat, Gender) %>% 
  summarise(count=n()) %>% 
  mutate(five_yr_ave = count/5) %>% 
  ggplot(aes(x=Dayweek, y=five_yr_ave,  group=five_yr_interval, colour=five_yr_interval)) +
  geom_line() +
  ylab('Number of Deaths') +
  xlab('Day of Week') +
  scale_x_discrete(labels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')) +
  labs(colour="Five-Year Interval") +
  scale_colour_colorblind() +
  facet_grid(age_cat~Gender) +
  ggtitle('Number of Deaths by Day of the Week') +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) 
day_p # the peak of weekend deaths is dominated by the young men

road_data$hour_int = as.integer(road_data$time_of_day)  
# 12 pm to 11am for plotting
road_data$hour_int = factor(road_data$hour_int, levels = c(0:23)) 
summary(road_data$hour_int)

# hour_p = road_data %>% 
#   group_by(hour_int, five_yr_interval, age_cat, Gender) %>% 
#   summarise(count=n()) %>% 
#   mutate(five_yr_ave = count/5) %>% 
#   ggplot(aes(x=hour_int, y=five_yr_ave, group=five_yr_interval, colour=five_yr_interval)) +
#   geom_line() +
#   ylab('Number of Deaths') +
#   scale_y_log10() +
#   scale_color_colorblind() +
#   scale_fill_colorblind() +
#   facet_grid(age_cat~Gender)
# hour_p # 2-6pm is most deaths, secondary peak ~ midnight
hour_p = road_data %>% 
  group_by(hour_int, five_yr_interval, age_cat, Gender) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=hour_int, y=count, group=five_yr_interval, colour=five_yr_interval)) +
  geom_line(alpha=0.5) +
  scale_x_discrete(breaks = c(0,3,6,9,12,15,18,21), labels = c('0', '3', '6', '9', '12', '15', '18', '21')) +
  xlab('Time (24 hour)') +
  geom_smooth(se=F, size=0.5, method = 'loess') +
  ylab('Number of Deaths') +
  scale_color_colorblind() +
  facet_grid(rows=vars(age_cat), cols = vars(Gender)) +
  ggtitle('Number of Deaths by Hour of the Day') +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) 
hour_p  # big peaks ~midnight for males 17-40, lesser so for females

hour_day_p = road_data %>% 
  group_by(hour_int, Dayweek, age_cat, Gender) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=hour_int, y=count, group=age_cat, colour=age_cat)) +
  geom_line(alpha=0.5) +
  scale_x_discrete(breaks = c(0,3,6,9,12,15,18,21), labels = c('0', '3', '6', '9', '12', '15', '18', '21')) +
  xlab('Time (24 hour)') +
  geom_smooth(se=F, size=0.5, method = 'loess') +
  labs(colour='Age') +
  ylab('Number of Deaths') +
  scale_color_colorblind() +
  facet_grid(rows=vars(Gender), cols = vars(Dayweek)) +
  ggtitle('Number of Deaths by Day and Time') +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) 
hour_day_p  # big peaks ~midnight for males 17-40, lesser so for females

# males vs females - far more males die on the road
# speed limit
# sex_bar_p = road_data %>% 
#   drop_na(c(Gender, `Speed Limit`)) %>% 
#   ggplot(aes(x=`Speed Limit`, fill=Gender)) +
#   geom_bar(position = 'dodge') +
#   scale_fill_colorblind()
# sex_bar_p

# # age by gender
# age_sex_df = road_data %>% 
#   drop_na(Gender)
#   
# age_p = ggplot(age_sex_df, aes(x=Gender, y=Age, colour=Gender)) +
#   #geom_boxplot() +
#   geom_violin() +
#   geom_point(position=position_jitterdodge(), alpha=0.01) +
#   scale_colour_colorblind() +
#   theme(legend.position = 'none')
# age_p
  
# road user
summary(road_data$road_user)

road_data = road_data %>% 
  mutate(User = ifelse(road_user=='Motorcycle pillion passenger'|road_user=='Motorcycle rider', 'Motorcycle',
                       ifelse(road_user=='Driver'|road_user=='Passenger', 'Car/Truck', 
                              ifelse(road_user=='Pedestrian', 'Pedestrian', 
                                     ifelse(road_user=='Pedal cyclist', 'Cyclist', NA))))) 
road_data$User = factor(road_data$User, levels = c('Car/Truck', 'Motorcycle', 'Pedestrian', 'Cyclist'))
user_p = road_data %>% 
  group_by(Year, User, age_cat, Gender) %>% 
  summarise(n=n()) %>%
  ggplot(aes(x=Year, y=n, colour=User)) +
  geom_line(alpha=0.5) +
  geom_smooth(se=F, size=0.5, method = 'loess') +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015), labels = c('1990', '1995', '2000', '2005', '2010', '2015')) +
  scale_y_log10() +
  ylab('Count') +
  scale_color_colorblind() +
  facet_grid(age_cat~Gender) +
  ggtitle('Number of Deaths by Type of Road User') +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) 
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
bus_deaths_2018 = summary(as.factor(df_2018$bus_involve))[2]
total = nrow(df_2018)
bus_prop_2018 = bus_deaths_2018/total # 0.02 of deaths, but 0.005 of vehicles on road # not heavily overrepresented (4 times as many) - and driving a lot of the time

heavy_rigid_deaths_2018 = summary(as.factor(df_2018$heavy_rigid_involve))[2]
heavy_rigid_prop_2018 = heavy_rigid_deaths_2018 / total # 0.06 of deaths (6%) and account for 0.018 (~2%) of vehicles (overrepresented 3 times as many)

articulated_deaths_2018 = summary(as.factor(df_2018$articulated_involve))[2]
articulated_prop_2018 = articulated_deaths_2018 / total # 0.079 of deaths (~8%) and account for 0.005 of vehicles (heavily overrepresented 16 times as many)

deaths_2018 = c(mc_prop_2018, bus_prop_2018, heavy_rigid_prop_2018, articulated_prop_2018)
props_2018 = cbind.data.frame(prop_pop, deaths_2018)
names(props_2018) = c('Population', 'Deaths')
props_2018$user = c('Motorcycle', 'Bus', 'Heavy Rigid Truck', 'Articulated Truck')

# need to make long
props_2018_p = props_2018 %>% 
  gather('Group', 'Proportion', 1:2) %>% 
  ggplot(aes(x=factor(user, levels=c('Bus', 'Heavy Rigid Truck', 'Articulated Truck', 'Motorcycle')), y=Proportion, colour=Group, fill=Group)) +
  geom_bar(stat='identity', position = 'dodge') +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  ggtitle('Proportion of Vehicle Types Involved in Fatatities \n Compared with Overall Population') +
  xlab('Type of Vehicle') +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) 
props_2018_p

#christmas and easter
#Xmas period is 12 days beginning Dec 23
#Easter period is 5 days beginning thursday before good friday
#total = 17 days / 365
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
#   mutate(Deaths_per_day = ifelse(Is_holiday=='Yes', Deaths/17, Deaths/348)) # to get deaths per day if holiday /17 (17 days in holiday period), else /348 (365-17)
# 
# holiday_p = ggplot(holiday_df, aes(x=Year, y=Deaths_per_day, colour=Is_holiday)) +
#   geom_line() +
#   scale_color_colorblind()
# holiday_p



## Report
# intro
# investigate the motorvehicle death data - look for trends over time and over-represeted, at-risk groups, compare with other country data

# conclusion
# males more at risk
# mc more at risk of death that other vehicle types
# busses, heavy rigid and articulated trucks all overrepreseted, articulated trucks heavily so
# consider targeting interventions to these groups

# could be used to inform public policy - target males and mc riders - improve safety for mc riders
# NT has improved, but still higher risk of death than other states
# steady deterioration over time due to improved safety features (cite), effective government campaigns???
# 

# find out how australia compares with other countries - calculte total stats for Aus and compare



# intro - aim is to gain inssights from road death data that may help to shape public policy

# include r script as appendix
# and mice plots in appendix also





