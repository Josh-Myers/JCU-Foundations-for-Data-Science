# Data Vis Assessment
# import and wrangle data----
Data = read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data', header = F, sep = ',', na.strings = '?')

info = read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.names', header = F, sep = ',')

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

# Proximity measures----
library(cluster)
# don't need to specify type because they are factor - which are treated as symmetric by default
# with binary variables (0,1) would need to specify type because they are treated as asymmetric by default
Dist <- daisy(Data, metric = 'gower')
summary(Dist)
Dist <- as.matrix(Dist)
dim(Dist)

# do a check - compare a pair that should be similar and a pair that should be dissimilar
# This is the most similar pair - makes sense, they are very similar
Data[which(Dist == min(Dist[Dist != min(Dist)]), arr.ind = TRUE)[1, ], ]

# Most dissimilar pair - seems reasonable
Data[which(Dist == max(Dist[Dist != max(Dist)]), arr.ind = TRUE)[1, ], ]

#Note – red is 0 then yellow, green, blue is higher (closer to 1)
#The pattern is because successes and failures are grouped together in the dataset
#Randomly reorder rows before daisy – pattern should disappear (except for red line)

dim <- ncol(Dist)  # used to define axis in image
image(1:dim, 1:dim, Dist, axes = FALSE, xlab="", ylab="", col = rainbow(100))
heatmap(Dist, Rowv=TRUE, Colv="Rowv", symm = TRUE)

# The pattern is because accepted and rejected are grouped together - randomly reshuffle - should go away
rand <- sample(nrow(Data))
Data_rand = Data[rand, ]
Dist_rand <- daisy(Data_rand, metric = 'gower')  
Dist_rand <- as.matrix(Dist_rand)
image(1:dim, 1:dim, Dist_rand, axes = FALSE, xlab="", ylab="", col = rainbow(100)) # evenly distributed now

# it's a heatmap and some are closer than others
# this colour are closer together, others are further apart

numeric_cols = c('Age', 'MonthlyExpenses', 'YearsEmployed', 'CreditScore', 'MonthlyIncome', 'AccountBalance')
Data_numeric = Data[, numeric_cols]
pearson_cor = cor(Data_numeric, method = 'pearson')
spearman_cor = cor(Data_numeric, method = 'spearman')

# SMC
ct = table(Data$NoPriorDefault, Data$Approved)
ssc = (ct[1,1]+ct[2,2])/sum(ct)
jc = ct[2,2]/sum(ct)


# boxplots----
summary(Data$AccountBalance)
# This variable is left skewed so I will log transform (+ 1)
library(ggplot2)
library(ggthemes)
acc_bal_p = ggplot(data = Data, aes(x=Approved,  y=AccountBalance + 1, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Account Balance and Credit Card Application Outcome') +
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
  ggtitle('Monthly Expenses and Credit Card Application Outcome') +
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
  ggtitle('Credit Score and Credit Card Application Outcome') +
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
#   ggtitle('Credit Score and Credit Card Application Outcome') +
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
  ggtitle('Age and Credit Card Application Outcome') +
  ylab('Age') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  scale_y_sqrt(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() 
age_p

# bar plots----
# change approved to no and yes

Data$Approved =  fct_recode(Data$Approved, 'No' = '-', 'Yes' = '+')

#Data$Approved = as.character(Data$Approved)
#Data$Approved[Data$Approved == '-'] = 'No'
#Data$Approved[Data$Approved == '+'] = 'Yes'
#Data$Approved = factor(Data$Approved, levels = c('No', 'Yes'))

# Employed
employed_p = ggplot(data=Data, aes(x=Employed, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Employment Status and Credit Card Application Outcome') +
  xlab('Employed') + 
  ylab('Count') +
  scale_x_discrete(labels=c("f" = "No", "t" = "Yes")) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
employed_p

# MaritalStatus  
# u = unmarried; y = yes; l must be 'living together'
# only 2 living together - both approved
# consider removing - just put a note
filter(Data, MaritalStatus=='l')
marital_p = ggplot(data=filter(Data, MaritalStatus %in% c('u', 'y')), aes(x=MaritalStatus, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Marital Status and Credit Card Application Outcome') +
  xlab('Marital Status') + 
  ylab('Count') +
  scale_x_discrete(labels=c("l" = "Other", "u" = "Unmarried", 'y'='Married')) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
marital_p

# interestingly married people are less likely to be approved

# BankingInstitution 
Data$BankingInstitution = fct_infreq(Data$BankingInstitution)
bank_p = ggplot(data=Data, aes(x=BankingInstitution, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Banking Institution and Credit Card Application Outcome') +
  xlab('Banking Institution') + 
  ylab('Count') +
  scale_x_discrete(labels=c(LETTERS[1:9])) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
bank_p

# NoPriorDefault
default_p = ggplot(data=Data, aes(x=NoPriorDefault, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Prior Default and Credit Card Application Outcome') +
  xlab('Prior Default') + 
  ylab('Count') +
  scale_x_discrete(labels=c("f" = "Yes", "t" = "No")) + # this seems weird but the variable is "no prior default" I am presenting as "prior default" y/n
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
default_p
# very powerful explanatory variable

# free plots (EDA) ----
# do a plot monthly expenses by age - I'm curious about all the $0 monthly expenses
# can use my marginal density plots here
library(ggExtra)
age_exp_p = ggplot(data = Data, mapping = aes(x=Age, y=MonthlyExpenses, colour=Approved)) +
  geom_point(alpha=0.4) +
  scale_color_colorblind() +
  theme_minimal() 

age_exp_p = ggMarginal(age_exp_p, type = 'density',
    margins = 'both',
    size = 5, groupColour = TRUE,
    groupFill = TRUE)
age_exp_p

# plot age by mthly expenses - i'm interested in ppl with $0 mthly expenses - are they younger?
num_cols = c("Age", "MonthlyExpenses", "YearsEmployed", "CreditScore", "MonthlyIncome", "AccountBalance")
cat_cols = c("Gender", "MaritalStatus", "HomeStatus", "Occupation", "BankingInstitution", "NoPriorDefault", 
             "Employed", "DriversLicense", "AccountType", "Approved")

# Age
age_p = ggplot(data = Data, aes(x=Approved,  y=Age, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Age and Credit Card Application Outcome') +
  ylab('Age') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  scale_y_sqrt(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() 
age_p

# monthly expenses
# multiply by 100 to get $ value
mth_exp_p = ggplot(data = Data, aes(x=Approved,  y=MonthlyExpenses * 100, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Monthly Expenses and Credit Card Application Outcome') +
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

# years employed
years_employed_p = ggplot(data = Data, aes(x=Approved,  y=YearsEmployed, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Years Employed and Credit Card Application Outcome') +
  ylab('Credit Score + 1') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() 
credit_score_p

# credit score (log2)
credit_score_p = ggplot(data = Data, aes(x=Approved,  y=CreditScore + 1, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Credit Score and Credit Card Application Outcome') +
  ylab('Credit Score + 1') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  scale_y_continuous(trans = 'log2', labels = scales::comma) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() 
credit_score_p

# monthly income
mth_inc_p = ggplot(data = Data, aes(x=Approved,  y=MonthlyIncome + 1, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Monthly Income and Credit Card Application Outcome') +
  ylab('Monthly Income + 1 ($)') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  scale_y_log10(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() 
mth_inc_p
# a bunch of people with 0 mthly income approved.. why?? seems wrong
zero_income_approved = filter(Data, Approved=='+', MonthlyIncome==0)

# acct balance
acc_bal_p = ggplot(data = Data, aes(x=Approved,  y=AccountBalance + 1, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Account Balance and Credit Card Application Outcome') +
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

# Categorical variables - bar plots
# gender
gender_p = ggplot(data=Data, aes(x=Gender, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Gender and Credit Card Application Outcome') +
  xlab('Gender') + 
  ylab('Count') +
  scale_x_discrete(labels=c("f" = "No", "t" = "Yes")) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
gender_p
# 'b' bigger proportion and less likely to be approved.. women?? 

# MaritalStatus  
# u = unmarried; y = yes; l must be 'living together'
# only 2 living together - both approved
# consider removing - just put a note
filter(Data, MaritalStatus=='l')
marital_p = ggplot(data=filter(Data, MaritalStatus %in% c('u', 'y')), aes(x=MaritalStatus, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Marital Status and Credit Card Application Outcome') +
  xlab('Marital Status') + 
  ylab('Count') +
  scale_x_discrete(labels=c("l" = "Other", "u" = "Unmarried", 'y'='Married')) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
marital_p
# interestingly married people are less likely to be approved

# home status
home_p = ggplot(data=Data, aes(x=HomeStatus, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Home Status and Credit Card Application Outcome') +
  xlab('Home Status') + 
  ylab('Count') +
  scale_x_discrete(labels=c("l" = "Other", "u" = "Unmarried", 'y'='Married')) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
home_p
# 'p' less likely to be approved - renting???

# occupation
occ_p = ggplot(data=Data, aes(x=Occupation, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Occupation and Credit Card Application Outcome') +
  xlab('Occupation') + 
  ylab('Count') +
  scale_x_discrete(labels=c("l" = "Other", "u" = "Unmarried", 'y'='Married')) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
occ_p
# interesting some occs more and less likely - interesting to plot against mthly income grouped by gender
# may find that higher paid more likely to be approved are men???

# BankingInstitution 
Data$BankingInstitution = fct_infreq(Data$BankingInstitution)
bank_p = ggplot(data=Data, aes(x=BankingInstitution, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Banking Institution and Credit Card Application Outcome') +
  xlab('Banking Institution') + 
  ylab('Count') +
  scale_x_discrete(labels=c(LETTERS[1:9])) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
bank_p

# NoPriorDefault
default_p = ggplot(data=Data, aes(x=NoPriorDefault, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Prior Default and Credit Card Application Outcome') +
  xlab('Prior Default') + 
  ylab('Count') +
  scale_x_discrete(labels=c("f" = "Yes", "t" = "No")) + # this seems weird but the variable is "no prior default" I am presenting as "prior default" y/n
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
default_p
# very powerful explanatory variable

# Employed
employed_p = ggplot(data=Data, aes(x=Employed, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Employment Status and Credit Card Application Outcome') +
  xlab('Employed') + 
  ylab('Count') +
  scale_x_discrete(labels=c("f" = "No", "t" = "Yes")) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
employed_p

# DriversLicense
licence_p = ggplot(data=Data, aes(x=DriversLicense, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Drivers License and Credit Card Application Outcome') +
  xlab('Drivers License') + 
  ylab('Count') +
  scale_x_discrete(labels=c("f" = "No", "t" = "Yes")) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
licence_p

# AccountType
acc_type_p = ggplot(data=Data, aes(x=AccountType, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Account Type and Credit Card Application Outcome') +
  xlab('Account Type') + 
  ylab('Count') +
  scale_x_discrete(labels=c("f" = "No", "t" = "Yes")) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
acc_type_p

# Approved
app_p = ggplot(data=Data, aes(x=Approved)) + 
  geom_bar() +
  ggtitle('Credit Card Application Outcome') +
  xlab('Approved') + 
  ylab('Count') +
  scale_x_discrete(labels=c("f" = "No", "t" = "Yes")) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
app_p
# more no than yes




# ideas
# interesting some occs more and less likely - interesting to plot against mthly income grouped by gender

# boxplots of income vs occupation stratified by gender
inc_occ_sex_p = ggplot(data = Data, mapping = aes(y=MonthlyIncome, colour=Approved)) +
  geom_boxplot() +
  facet_wrap(vars(Occupation))
inc_occ_sex_p
# boxplotes of savings vs occupation stratified by gender

# may find that higher paid more likely to be approved are men???


# income by acct balance (approved)

# occupation, income, acct balance by approved



# a bunch of people with 0 mthly income approved.. why?? seems wrong
zero_income_approved = filter(Data, Approved=='+', MonthlyIncome==0)

library(Hmisc)
describe(Data)
des = describe(Data)
plot(des)

# credit score by gender # not particularly enlightening
credit_score_gender_p = ggplot(data = Data, aes(x=Gender,  y=CreditScore + 1, group=Gender, colour=Gender)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Credit Score and Credit Card Application Outcome') +
  ylab('Credit Score + 1') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  scale_y_continuous(trans = 'log2', labels = scales::comma) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() 
credit_score_gender_p
# 'b' has lower credit score

# marital status by gender
marital_sex_p = ggplot(data=filter(Data, MaritalStatus %in% c('u', 'y')), aes(x=MaritalStatus, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Marital Status and Credit Card Application Outcome') +
  xlab('Marital Status') + 
  ylab('Count') +
  scale_x_discrete(labels=c("l" = "Other", "u" = "Unmarried", 'y'='Married')) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() +
  facet_wrap(vars(Gender))
marital_sex_p
# interesting - for sex 'b' married are much less likely to be approved

# employed by gender
employed_sex_p = ggplot(data=Data, aes(x=Employed, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Employment Status and Credit Card Application Outcome') +
  xlab('Employed') + 
  ylab('Count') +
  scale_x_discrete(labels=c("f" = "No", "t" = "Yes")) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() +
  facet_wrap(vars(Gender))
employed_sex_p

# DriversLicense gender
licence_sex_p = ggplot(data=Data, aes(x=DriversLicense, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Drivers License and Credit Card Application Outcome') +
  xlab('Drivers License') + 
  ylab('Count') +
  scale_x_discrete(labels=c("f" = "No", "t" = "Yes")) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() +
  facet_wrap(vars(Gender))
licence_sex_p
# for 'b' a higher proportion without drivers licence were not approved

occ_sex_p = ggplot(data=Data, aes(x=Occupation, fill=Gender)) + 
  geom_bar(position="dodge") +
  ggtitle('Occupation and Credit Card Application Outcome') +
  xlab('Occupation') + 
  ylab('Count') +
  scale_x_discrete(labels=c("l" = "Other", "u" = "Unmarried", 'y'='Married')) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() 
occ_sex_p
# b are much more likely to be 'c'

occ_sex_app_p = ggplot(data=Data, aes(x=Occupation, fill=Approved)) + 
  geom_bar(position="dodge") +
  ggtitle('Occupation and Credit Card Application Outcome') +
  xlab('Occupation') + 
  ylab('Count') +
  scale_x_discrete(labels=c("l" = "Other", "u" = "Unmarried", 'y'='Married')) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() +
  facet_wrap(vars(Gender))
occ_sex_app_p









