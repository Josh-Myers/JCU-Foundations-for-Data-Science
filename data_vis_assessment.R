# Data Vis Assessment
# 1. import and wrangle data----
library(tidyverse)
library(ggExtra)
library(cowplot)
library(ggthemes)
library(cluster)
library(Hmisc)

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

# 2. Proximity measures----
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
#image(1:dim, 1:dim, Dist, axes = FALSE, xlab="", ylab="", col = rainbow(100))
#heatmap(Dist, Rowv=TRUE, Colv="Rowv", symm = TRUE)

# The pattern is because accepted and rejected are grouped together - randomly reshuffle - should go away
rand <- sample(nrow(Data))
Data_rand = Data[rand, ]
Dist_rand <- daisy(Data_rand, metric = 'gower')  
Dist_rand <- as.matrix(Dist_rand)
#image(1:dim, 1:dim, Dist_rand, axes = FALSE, xlab="", ylab="", col = rainbow(100)) # evenly distributed now

# sort by credit outcome
Data_sort = Data[order(Data$Approved),]
Dist_sort = daisy(Data_sort, metric = 'gower')
Dist_sort = as.matrix(Dist_sort)
#image(1:dim, 1:dim, Dist_sort, axes = FALSE, xlab="", ylab="", col = rainbow(100)) # evenly distributed now

# it's a heatmap and some are closer than others
# this colour are closer together, others are further apart

numeric_cols = c('Age', 'MonthlyExpenses', 'YearsEmployed', 'CreditScore', 'MonthlyIncome', 'AccountBalance')
# numeric_cols = select_if(Data, is.numeric) another way
Data_numeric = Data[, numeric_cols]
pearson_cor = cor(Data_numeric, method = 'pearson')
spearman_cor = cor(Data_numeric, method = 'spearman')

# SMC
ct = table(Data$NoPriorDefault, Data$Approved)
ssc = (ct[1,1]+ct[2,2])/sum(ct)
jc = ct[2,2]/sum(ct)


# 3. boxplots----
summary(Data$AccountBalance)
# This variable is left skewed so I will log transform (+ 1)
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

# 4. bar plots----
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

# 5. Exploring Monthly Income (EDA) ----
describe(Data) # Hmisc
des = describe(Data)
plot(des)

# plot each variable
num_cols = c("Age", "MonthlyExpenses", "YearsEmployed", "CreditScore", "MonthlyIncome", "AccountBalance")
cat_cols = c("Gender", "MaritalStatus", "HomeStatus", "Occupation", "BankingInstitution", "NoPriorDefault", 
             "Employed", "DriversLicense", "AccountType", "Approved")

mth_inc_p = ggplot(data = Data, aes(x=Approved,  y=MonthlyIncome + 1, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Monthly Income All') +
  ylab('Monthly Income + 1 ($)') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  scale_y_log10(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() 

mth_inc_p = ggMarginal(mth_inc_p, type = 'density', margins = 'y', size = 5, groupColour = T, groupFill = T)
mth_inc_p
# this is interesting people with higher income are slighly less likely to be approved..
# its the cluster of people with 0 mthly income who were approved who are dragging the distribution down - look into those

Data %>% group_by(Approved) %>% 
  filter(MonthlyIncome==0) %>% 
  summarise(count=n())

summary(Data$Approved)
# there are 80/296 (0.27 proportion) people with 0 mthly income who were approved, compared with 48/357 (0.13 prop) not approved
# in the dataset there are more not approved than approved - so this definitely bucks the trend
# are they older?? retired?? have more savings?? are they a certain occupation??

# summarise income with 0 removed, are approved and not approved similar now??
inc_not_0_p = Data %>% 
  filter(MonthlyIncome != 0) %>% 
  ggplot(aes(x=Approved,  y=MonthlyIncome, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Monthly Income > 0') +
  ylab('Monthly Income ($)') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  scale_y_log10(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() 

inc_not_0_p = ggMarginal(inc_not_0_p, type = 'density', margins = 'y', size = 5, groupColour = T, groupFill = T)
inc_not_0_p

mth_inc_mult_p = plot_grid(mth_inc_p, inc_not_0_p, labels = c('Fig. A', 'Fig. B'))
ggsave("mth_inc_mult.png", mth_inc_mult_p, width = 10, height = 5)

# make a cat variable 0 monthly income or >0 monthly income
Data = Data %>%
  mutate(zeroMthlyInc = ifelse(MonthlyIncome==0, 0, 1))
  
Data$zeroMthlyInc = factor(Data$zeroMthlyInc, levels = c(0, 1), labels = c('Monthly Income = $0', 'Monthly Income > $0'))
summary(Data$zeroMthlyInc)

cred_mthInc_p = ggplot(data = Data, aes(x=Approved,  y=CreditScore+1, colour=Approved)) + 
  geom_boxplot() +
  geom_point(position=position_jitterdodge(), alpha=0.3) +
  ggtitle('Credit Score') +
  ylab('Credit Score + 1') + 
  xlab('Credit Card Application Approved') + 
  #scale_x_discrete(labels=c("0", ">0")) +
  scale_y_continuous(trans='log2', labels = scales::comma) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() + 
  facet_wrap(vars(zeroMthlyInc))
cred_mthInc_p

# zero mthly income that were approved had higher credit score 

# are they older - perhaps retired??
# Age
age_inc_p = ggplot(data = Data, aes(x=Approved,  y=Age, group=Approved, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Age') +
  ylab('Age') + 
  xlab('Credit Card Application Approved') + 
  scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  scale_y_sqrt(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() +
  facet_wrap(vars(zeroMthlyInc))
age_inc_p
# older on average

# NoPriorDefault
default_inc_p = ggplot(data=Data, aes(x=NoPriorDefault, fill=Approved, colour=Approved)) + 
  geom_bar(position='fill') +
  xlab('Prior Default') + 
  ylab('Proportion') +
  ggtitle('Prior Default') +
  scale_x_discrete(labels=c("f" = "Yes", "t" = "No")) + # this seems weird but the variable is "no prior default" I am presenting as "prior default" y/n
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  facet_wrap(vars(zeroMthlyInc))
default_inc_p
# greater proportion of no prior default with 0 mthly income
# people with $0 mthly income with no prior default were very likely to be approved (~90%), compared with mthly income >$0 (75%)

# Occupation
Data$Approved = factor(Data$Approved, labels = c('No', 'Yes'))

occ_inc_p = ggplot(data=Data, aes(x=Occupation, fill=zeroMthlyInc)) + 
  geom_bar() +
  #ggtitle('Prior Default and Credit Card Application Outcome') +
  xlab('Occupation') + 
  #ylab('Count') +
  scale_x_discrete(labels=c("f" = "Yes", "t" = "No")) + # this seems weird but the variable is "no prior default" I am presenting as "prior default" y/n
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind()
occ_inc_p
# more of c occupation

# look at only people with $0 income by occupation and approved
occ_0_df = Data %>% 
  filter(zeroMthlyInc=='Monthly Income = $0') %>% 
  group_by(Approved, Occupation) %>% 
  summarise(occ = n())

occ_inc0_p = Data %>% 
  filter(zeroMthlyInc=='Monthly Income = $0') %>% 
  ggplot(aes(x=Occupation, colour=Approved, fill=Approved)) +
  geom_bar() +
  xlab('Occupation') +
  ggtitle('Monthly Income = $0 Occupation') +
  facet_grid(vars(Approved)) +
  scale_color_colorblind() +
  scale_x_discrete(labels=c(LETTERS[1:14])) +
  scale_fill_colorblind() +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) 
occ_inc0_p  
# More occupation c and q for people with $0 income who were approved

# home status
# filter out 'gg' there is only 2 of those
home_inc_p = Data %>% 
  filter(HomeStatus != 'gg') %>% 
  ggplot(aes(x=HomeStatus, fill=Approved, colour=Approved)) + 
  geom_bar(position="fill") +
  ggtitle('Home Status') +
  xlab('Home Status') + 
  ylab('Proportion') +
  scale_x_discrete(labels=c("A", "B")) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  facet_wrap(vars(zeroMthlyInc))
home_inc_p
# 0 who were approved more likely to be 'g' home status - g is own your home??

acc_bal_inc_p = ggplot(data = Data, aes(x=Approved, y=AccountBalance+1, colour=Approved)) + 
  geom_boxplot() + 
  geom_point(position=position_jitterdodge(), alpha=0.1) +
  ggtitle('Account Balance') +
  ylab('Bank Account Balance + 1 ($)') + 
  xlab('Credit Card Application Approved') + 
  #scale_x_discrete(labels=c("-" = "No", "+" = "Yes")) +
  scale_y_log10(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_colour_colorblind() +
  facet_wrap(vars(zeroMthlyInc))
acc_bal_inc_p

# MaritalStatus  
# u = unmarried; y = yes; l must be 'living together'
# only 2 living together - both approved
# consider removing - just put a note
filter(Data, MaritalStatus=='l')
marital_inc_p = ggplot(data=filter(Data, MaritalStatus %in% c('u', 'y')), aes(x=MaritalStatus, fill=Approved)) + 
  geom_bar(position="fill") +
  ggtitle('Marital Status') +
  xlab('Marital Status') + 
  ylab('Proportion') +
  scale_x_discrete(labels=c("l" = "Other", "u" = "Unmarried", 'y'='Married')) +
  theme_minimal() +
  theme(plot.title = element_text(face='bold', hjust = 0.5, vjust = 0.5)) +
  scale_fill_colorblind() +
  facet_wrap(vars(zeroMthlyInc))
marital_inc_p
# higher prop unmarried

# gender
sex_inc_p = ggplot(Data, aes(x=Gender, group=Approved, colour=Approved, fill=Approved)) +
  geom_bar(position='fill') +
  facet_wrap(vars(zeroMthlyInc))
sex_inc_p


cont = plot_grid(age_inc_p, acc_bal_inc_p, cred_mthInc_p, labels = c('Fig. C', 'Fig. D', 'Fig. E'), nrow = 1)
bar = plot_grid(default_inc_p, home_inc_p, marital_inc_p, occ_inc0_p, labels = c('Fig. F', 'Fig. G', 'Fig. H', 'Fig. I'), nrow = 1)

ggsave("box_multi.png", cont, width = 15, height = 5)
ggsave("bar_multi.png", bar, width = 20, height = 5)


