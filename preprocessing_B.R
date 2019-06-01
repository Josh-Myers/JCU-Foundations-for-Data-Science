# Preprocessing assignment Part B

# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Thresholds
Thresh.Brix.min <- 15
Thresh.Brix.max <- 30

Thresh.Pol.min <- 50
Thresh.Pol.max <- 105

Thresh.Fibre.min <- 4
Thresh.Fibre.max <- 25

Thresh.Ash.min <- 0
Thresh.Ash.max <- 8

NIRData = read.table('data/assessments/Sugar_Cane_NIRS/Sugar_Cane_Input_Files/NIRPred.csv', header=T, sep = ',')

summary(NIRData)
NIRData[1:15,]

NIRData$DateTime <- as.POSIXct(NIRData$DateTime, format = "%Y-%m-%d %H:%M:%S")

NIRData = transform(NIRData, LabID = floor(ScanID))
NIRData[1:15,]

NIRData_Filtered = NIRData %>% 
  filter(GH <= 3.5, NH <= 2, NIR_Pol > Thresh.Pol.min, NIR_Pol < Thresh.Pol.max, NIR_Brix > Thresh.Brix.min, NIR_Brix < Thresh.Brix.max,
         NIR_Fibre > Thresh.Fibre.min, NIR_Fibre < Thresh.Fibre.max, NIR_Ash > Thresh.Ash.min, NIR_Ash < Thresh.Ash.max, ScanID != -1)

NIR_Final = NIRData_Filtered %>% 
  group_by(LabID) %>% 
  summarise(DateTime = min(DateTime), NIR_Pol = mean(NIR_Pol), NIR_Brix = mean(NIR_Brix), NIR_Fibre = mean(NIR_Fibre), NIR_Ash = mean(NIR_Ash))
  
write.table(NIR_Final, file = "NIR_Final.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")


