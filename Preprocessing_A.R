# Preprocessing Assignment Part A

# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Functions
rescale_01 <-function(x) (x-min(x))/(max(x)-min(x)) -1/2
z_stand<-function(x) (x-mean(x))/sd(x)
ExpectedBrix <- function(x) (x*0.21084778699754 + 4.28455310831511)

# Thresholds
Thresh.Brix.min <- 15
Thresh.Brix.max <- 30

Thresh.Pol.min <- 50
Thresh.Pol.max <- 105
ExpectedBrix.delta <- 1

Thresh.Fibre.min <- 4
Thresh.Fibre.max <- 25
Thresh.Fibre.delta <- .25

Thresh.Ash.min <- 0
Thresh.Ash.max <- 8

# Fibre Data
Lab_Fibre_Data = read.table('data/assessments/Sugar_Cane_NIRS/Sugar_Cane_Input_Files/Lab_Fibre_Weights.csv', header=T, sep = ',')

# Analysis
Lab_Fibre_Data$Fibre1 = 100 * (Lab_Fibre_Data$InitialSampleCanWeight_1 - Lab_Fibre_Data$FinalSampleCanWeight_1) / Lab_Fibre_Data$SampleWeight_1

Lab_Fibre_Data = Lab_Fibre_Data %>% 
  mutate(Fibre2 = 100 * (InitialSampleCanWeight_2 - FinalSampleCanWeight_2) / SampleWeight_2)

# filter out observations <=0
# using filter()
Lab_Fibre_Filtered = Lab_Fibre_Data %>% 
  filter(LabID > 0, SampleWeight_1 > 0, InitialSampleCanWeight_1 > 0, FinalSampleCanWeight_1 > 0, SampleWeight_2 > 0, InitialSampleCanWeight_2 > 0, 
         FinalSampleCanWeight_2 > 0, Fibre1 > 0, Fibre2 > 0)
# could also use filter_all()
#Lab_Fibre_Filtered2 = Lab_Fibre_Data %>% 
#  filter_all(all_vars(. >0))

# filter out abs_diff of Fibre1 and Fibre2 >=0.25
#Lab_Fibre_Filtered = Lab_Fibre_Data %>% 
#  mutate(abs_diff = abs(Fibre1 - Fibre2)) %>%  
#  filter(LabID > 0, SampleWeight_1 > 0, InitialSampleCanWeight_1 > 0, FinalSampleCanWeight_1 > 0, SampleWeight_2 > 0, InitialSampleCanWeight_2 > 0, 
#         FinalSampleCanWeight_2 > 0, Fibre1 > 0, Fibre2 > 0, abs_diff < 0.25)

Lab_Fibre_Filtered = Lab_Fibre_Data %>% 
  filter(LabID > 0, SampleWeight_1 > 0, InitialSampleCanWeight_1 > 0, FinalSampleCanWeight_1 > 0, SampleWeight_2 > 0, InitialSampleCanWeight_2 > 0, 
         FinalSampleCanWeight_2 > 0, Fibre1 > 0, Fibre2 > 0, abs(Fibre1 - Fibre2) < 0.25)

Lab_Fibre_Filtered = Lab_Fibre_Filtered %>% 
  mutate(Fibre = (Fibre1+Fibre2)/2) 
  
Lab_Fibre_Filtered = Lab_Fibre_Filtered %>% 
  filter(Fibre > Thresh.Fibre.min) %>%  
  filter(Fibre < Thresh.Fibre.max)

Lab_Fibre = Lab_Fibre_Filtered %>% 
  select(LabID, Fibre)

# Ash data
Lab_Ash_Data = read.table('data/assessments/Sugar_Cane_NIRS/Sugar_Cane_Input_Files/Lab_Ash_Weights.csv', header=T, sep = ',')

#Ash = 100 * FinalWeight / InitialWeight
#where :
# InitialWeight  = InitialSampleInTinWeight - TinWeight 
# FinalWeight  = FinalSampleInTinWeight - TinWeight 

Lab_Ash_Calculated = Lab_Ash_Data %>% 
  filter(TinWeight > 0, InitialSampleInTinWeight > 0, FinalSampleInTinWeight > 0) %>% 
  mutate(InitialWeight = InitialSampleInTinWeight - TinWeight, FinalWeight = FinalSampleInTinWeight - TinWeight, Ash = 100 * FinalWeight/InitialWeight)

# update to filter out threshold values
Lab_Ash_Calculated = Lab_Ash_Data %>% 
  filter(TinWeight > 0, InitialSampleInTinWeight > 0, FinalSampleInTinWeight > 0) %>% 
  mutate(InitialWeight = InitialSampleInTinWeight - TinWeight, FinalWeight = FinalSampleInTinWeight - TinWeight, Ash = 100 * FinalWeight/InitialWeight) %>% 
  filter(Ash > Thresh.Ash.min, Ash < Thresh.Ash.max)

Lab_Ash = Lab_Ash_Calculated %>% 
  group_by(LabID) %>% 
  summarise(Ash=mean(Ash))
  
# Pol and Brix data
Lab_PB_Data = read.table('data/assessments/Sugar_Cane_NIRS/Sugar_Cane_Input_Files/Lab_Pol_Brix.csv', header=T, sep = ',')

Lab_PB_Data = Lab_PB_Data %>% 
  mutate(PredBrix = ExpectedBrix(Pol))

library(ggthemes)
PB_p = Lab_PB_Data %>% 
  mutate(Abs_Brix = abs(Brix - PredBrix), Abs_Brix_1 = factor(ifelse(Abs_Brix > 1, '> 1', '\u2264 1'))) %>% 
  ggplot(aes(x=Brix, y=Pol, colour=Abs_Brix_1)) +
  geom_point(alpha=0.4) +
  scale_color_colorblind() +
  labs(colour="| Brix - PredBrix |") +
  theme_minimal()
PB_p

Lab_PB  = Lab_PB_Data %>% 
  mutate(Abs_Brix = abs(Brix - PredBrix)) %>% 
  filter(Abs_Brix <= 1, Brix > Thresh.Brix.min, Brix < Thresh.Brix.max, Pol > Thresh.Pol.min, Pol < Thresh.Pol.max) %>% 
  select(LabID, Pol, Brix)
  
# join cleaned dfs together
Lab <- full_join(Lab_Ash, Lab_Fibre, by=c("LabID" = "LabID"))

Lab = full_join(Lab, Lab_PB, by=c("LabID" = "LabID"))  
Lab[1:11,]

write.table(Lab, file = "Lab_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")

# transform variables
Lab_Fibre = transform(Lab_Fibre, Fibre = z_stand(Fibre))
write.table(Lab_Fibre, file = "Lab_Fibre_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")

Lab_Ash = Lab_Ash %>% 
  transform(Ash = log10(Ash)) %>% 
  transform(Ash = z_stand(Ash))
write.table(Lab_Ash, file = "Lab_Ash_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")

Lab_PB$Bbin <- cut(Lab_PB$Brix, 40, labels = FALSE)
Lab_PB$Bbin <- as.factor(Lab_PB$Bbin)

Lab_B_Stratified_Balanced = Lab_PB %>% 
  group_by(Bbin) %>% 
  sample_n(size = 50, replace = TRUE) 

Lab_B_Stratified_Balanced = Lab_B_Stratified_Balanced %>% 
  transform(Brix = rescale_01(Brix)) %>% 
  select(LabID, Brix)
write.table(Lab_B_Stratified_Balanced, file = "Lab_Brix_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")


Lab_P_Stratified_Balanced = Lab_PB %>% 
  mutate(Pbin = as.factor(cut(Pol, 40, labels = FALSE))) %>% 
  group_by(Pbin) %>% 
  sample_n(size = 50, replace = TRUE) %>% 
  transform(Pol = rescale_01(Pol)) %>% 
  select(LabID, Pol)
write.table(Lab_P_Stratified_Balanced, file = "Lab_Pol_Out.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")






