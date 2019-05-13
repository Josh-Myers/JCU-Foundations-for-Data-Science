# Data Vis Assessment
Data = read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data', header = F, sep = ',', na.strings = '?', )
names(Data) <- c("Gender", "Age", "MonthlyExpenses", "MaritalStatus", "HomeStatus", "Occupation", "BankingInstitution", "YearsEmployed", 
                 "NoPriorDefault", "Employed", "CreditScore", "DriversLicense", "AccountType", "MonthlyIncome", "AccountBalance", "Approved")
Data$Gender <- as.factor(Data$Gender) # symmetric binary
Data$Age <- as.numeric(Data$Age) # numeric float
Data$MonthlyExpenses <- as.integer(Data$MonthlyExpenses) # numeric int
Data$MaritalStatus <- as.factor(Data$MaritalStatus)  # nominal
Data$HomeStatus <- as.factor(Data$HomeStatus) # nominal
Data$Occupation <- as.factor(Data$Occupation) # nominal
Data$BankingInstitution <- as.factor(Data$BankingInstitution) # nominal
Data$YearsEmployed <- as.numeric(Data$YearsEmployed) # numeric float
Data$NoPriorDefault <- as.factor(Data$NoPriorDefault) # symmetric binary
Data$Employed <- as.factor(Data$Employed) # symmetric binary
Data$CreditScore <- as.numeric(Data$CreditScore) # numeric float
Data$DriversLicense <- as.factor(Data$DriversLicense) # symmetric binary
Data$AccountType <- as.factor(Data$AccountType) # nominal
Data$MonthlyIncome <- as.integer(Data$MonthlyIncome) # numeric int
Data$AccountBalance <- as.numeric(Data$AccountBalance) # numeric float
Data$Approved <- as.factor(Data$Approved) # symmetric binary

initial = nrow(Data) 
summary(Data) # number missing
12 + 12 + 6 + 6 + 9 + 9 + 13
sum(is.na(Data)) # 67

# remove na
Data = na.omit(Data)
num_rm = initial - nrow(Data) # 37 rows removed 

# Proximity measures
library(cluster)
Dist <- daisy(Data, metric = 'gower', type = list(symm=c('Gender', 'NoPriorDefault', 'Employed', 'DriversLicense', 'Approved'))) 
# anyNA(Dist)
# why does this give a warning? 
summary(Dist)
Dist <- as.matrix(Dist)
dim(Dist)

dim <- ncol(Dist)  # used to define axis in image
image(1:dim, 1:dim, Dist, axes = FALSE, xlab="", ylab="", col = rainbow(100))
heatmap(Dist, Rowv=TRUE, Colv="Rowv", symm = TRUE)

numeric_cols = c('Age', 'MonthlyExpenses', 'YearsEmployed', 'CreditScore', 'MonthlyIncome', 'AccountBalance')
Data_numeric = Data[, numeric_cols]
pearson_cor = cor(Data_numeric, method = 'pearson')
spearman_cor = cor(Data_numeric, method = 'spearman')

# boxplots

ggplot(data=Data, aes(AccountBalance + 1, colour=Approved)) + geom_density() + scale_x_log10()

# density plot
summary(Data$AccountBalance)
# This variable is left skewed so I will transform (+ 1 so can log)
acc_balance_p = ggplot(data = Data, aes(x=Approved,  y=AccountBalance + 1, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.5) +
  ggtitle('The relationship between account balance \n and credit card application outcome') +
  ylab('Bank Account Balance + 1 ($)') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5))
acc_balance_p

library(tidyverse)
median_acc_bal = Data %>% 
  group_by(Approved) %>% 
  summarise(median = median(AccountBalance))
median_acc_bal

outlier.color = "red", outlier.shape = 3) + geom_jitter(width = 0.1, alpha = 0.05, color = "blue")





