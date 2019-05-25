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

# Import
Lab_Fibre_Data = read.table('data/assessments/Sugar_Cane_NIRS/Sugar_Cane_Input_Files/Lab_Fibre_Weights.csv', header=T, sep = ',')

# Analysis
Fibre_1 = 100 * (Lab_Fibre_Data$InitialSampleCanWeight_1 - Lab_Fibre_Data$FinalSampleCanWeight_1) / Lab_Fibre_Data$SampleWeight_1
Fibre_2 = 100 * (Lab_Fibre_Data$InitialSampleCanWeight_2 - Lab_Fibre_Data$FinalSampleCanWeight_2) / Lab_Fibre_Data$SampleWeight_2
