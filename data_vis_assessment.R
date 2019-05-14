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
Data$DriversLicense <- as.factor(Data$DriversLicense) # symmetric binary - I think this could be asymmetric - maybe more informative if no license - but is fairly evenly distributed between y and n so keep as symmetric
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
Dist <- daisy(Data, metric = 'gower')
Dist_sym <- daisy(Data, metric = 'gower', type = list(symm=c('Gender', 'NoPriorDefault', 'Employed', 'DriversLicense', 'Approved'))) 
# anyNA(Dist_sym)
# why does this give a warning? 
equal = as.data.frame(Dist == Dist_sym)
summary(equal$`Dist == Dist_sym`) # they are the same anyway - can ignore that warning
summary(Dist)
Dist <- as.matrix(Dist)
dim(Dist)

dim <- ncol(Dist)  # used to define axis in image
image(1:dim, 1:dim, Dist, axes = FALSE, xlab="", ylab="", col = rainbow(100))
heatmap(Dist, Rowv=TRUE, Colv="Rowv", symm = TRUE)

# it's a heatmap and some are closer than others
# this colour are closer together, others are further apart

numeric_cols = c('Age', 'MonthlyExpenses', 'YearsEmployed', 'CreditScore', 'MonthlyIncome', 'AccountBalance')
Data_numeric = Data[, numeric_cols]
pearson_cor = cor(Data_numeric, method = 'pearson')
spearman_cor = cor(Data_numeric, method = 'spearman')

# boxplots
summary(Data$AccountBalance)
# This variable is left skewed so I will log transform (+ 1)
library(ggthemes)
acc_bal_p = ggplot(data = Data, aes(x=Approved,  y=AccountBalance + 1, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Account balance and credit card application outcome') +
  ylab('Bank Account Balance + 1 ($)') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  scale_y_log10(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() 
acc_bal_p
# good separation between groups - but there are a lot of observations clustered at 0 for both groups
# makes sense - many people applying for credit card need money

# monthly expenses
# multiply by 100 to get $ value
mth_exp_p = ggplot(data = Data, aes(x=Approved,  y=MonthlyExpenses * 100, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Monthly expenses and credit card application outcome') +
  ylab('Montly Expenses ($)') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() 
mth_exp_p
# some outliers this time
# a lot of people with $0 monthly expenses - seems unrealistic

# credit score (log2)
credit_score_p = ggplot(data = Data, aes(x=Approved,  y=CreditScore + 1, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Credit score and credit card application outcome') +
  ylab('Credit Score + 1') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  scale_y_continuous(trans = 'log2', labels = scales::comma) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() 
credit_score_p

# credit score (sqrt) - I like log better
# credit_score_p_sqrt = ggplot(data = Data, aes(x=Approved,  y=CreditScore, group=Approved, colour=Approved)) + 
#   geom_boxplot() + 
#   geom_point(position=position_jitterdodge(), alpha=0.1) +
#   ggtitle('Credit score and credit card application outcome') +
#   ylab('Credit Score') + 
#   xlab('Credit Card Application Approved') + 
#   scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
#   scale_y_sqrt(labels = scales::comma) +
#   theme_minimal() +
#   theme(legend.position="none") +
#   theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
#   scale_colour_colorblind() 
# credit_score_p_sqrt

# Age
age_p = ggplot(data = Data, aes(x=Approved,  y=Age, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Age and credit card application outcome') +
  ylab('Age') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  scale_y_sqrt(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() 
age_p

# free plots
# do a plot monthly expenses by age - I'm curious about all the $0 monthly expenses
# can use my marginal density plots here

