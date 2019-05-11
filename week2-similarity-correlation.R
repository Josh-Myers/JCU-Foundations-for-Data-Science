# week 2 FDS - foundations for data science
# Data Representation

test <-read.csv("data/Example_Tan_Et_al.csv") # Read CSV file with comma separated values and period as decimal separator
names(test) <-c("Refund", "Marital_Status", "Taxable_Income", "Cheat") # Change variable names
test$Refund<-factor(test$Refund, levels = c("Y","N"), labels = c("Yes", "No")) # Change categorical values for Refund
test$Cheat<-factor(test$Cheat, levels = c("Y","N"), labels = c("Yes", "No")) # Change categorical values for Cheat
test

X <- data.frame(col1 = rep(1,10), col2 = seq(from = 10, to = 55, by = 5), col3 = 5:14)
X <- as.matrix(X)  

# Euclidian distance
eucl_dist = function(x, y) {
  sqrt(sum((x - y)^2))
}

x1 = c(2, -1, 1, 0)
x2 = c(7, 0, -4, 8)
x3 = c(4, 3, 5, 2)
x4 = c(5, 10, -1, 5)

eucl_dist(x4, x3)

# Manhatten (round the block) distance
man_dist = function(x, y) {
 sum(abs(x - y)) 
}

# suprema (maximum) distance
sup_dist = function(x, y) {
  max(abs(x - y))
}

# built in r function
# can use built in dist function and choose type eucl, man, max


X <-data.frame(col1 = rep(1,10), col2 = seq(from = 10, to = 55, by = 5), col3 = 5:14)
X <-as.matrix(X)
X

# suprema distance by hand
max( abs(X[3,] -X[7,]) ) 

# suprema distance using r fun
dist(X, method = "maximum", diag= TRUE, upper = TRUE)

## Euclidian distance of iris dataset
Iris <-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data",
                  header = FALSE, sep= ",", dec= ".") # Same as read.csv()
Iris <-as.matrix(Iris[,1:4]) # Convert data.frameto matrix...
euc_dist = as.matrix(dist(Iris, method = "euclidean"))

# Practice with ionosphere data
ion_df = read.csv('data/ionosphere.data', header = F)
# remove class labels
ion = as.matrix(ion_df[1:10,-35])

eucl_d = dist(ion, method = 'euclidean', diag = T, upper = T)
man_d = dist(ion, method = 'manhattan', diag = T, upper = T)
sup_d = dist(ion, method = 'maximum', diag = T, upper = T)

eucl_d
man_d
sup_d


x1 = c(2, NA, 1, 0)
x2 = c(7, 0, -4, 8)
x3 = c(NA, 3, NA, 2)
x4 = c(NA, 10 , -1, 5)
x = rbind(x1, x2, x3, x4)
dist(x)

dist(rbind(x1, x3))

C <- matrix(c(0.5, 1, 1, 0.5, 1, 1, 1, 1.5, 1.5, 1), 5, 2, byrow = TRUE) # Cluster C
v1 <- c(mean(C[,1]),mean(C[,2])) # Compute Centre of Cluster C (2-dimensional mean) 
cov_C <- cov(C) # Compute Covariance Matrix of Cluster C

data_E = c(0.8, 0.27, 0.27, 0.11)
cov_E = matrix(data, nrow = 2, byrow = T)
 
x <- c(2,1) # Point x
dxv1 <- mahalanobis(x2, v1, cov_C) 


# correlation
p <- c(1,	-3,	0,	4,	1,	0,	3)
q <- c(1,	1,	4, -2,	3, -1,	4)
cov(p,q)/(sd(p)*sd(q))  # same as cor(p, q, method = "pearson")

# cosine similarity
# Computing Cosine Similarity in R 
p <- c(3, 2, 0, 5, 0, 0, 0, 2, 0, 0)
q <- c(1, 0, 0, 0, 0, 0, 0, 1, 0, 2)
cos_pq <- (p%*%q)/sqrt((p%*%p)*(q%*%q))

# Or Alternatively:
cos_pq <- sum(p*q)/sqrt(sum(p^2)*sum(q^2))

# Computing Spearman correlation in R:
p <- c(1,	-3,	0,	4,	1,	0,	3)
q <- c(1,	1,	4, -2,	3, -1,	4)
cor(p, q, method = "spearman")


# Computing Gower's distance
# create data
s = 'single'
m = 'married'
d = 'divorced'
n = 0
y = 1
refund = c(1, 0, 0, 1, 0, 0, 1, 0, 0, 0)
marital_status = c(s, m, s, m, d, m, d, s, m, s)
income = c(125, 100, 70, 120, 95, 60, 220, 85, 75, 90)
cheat = c(n, n, n, n, y, n, n, y, n, y)
df = cbind.data.frame(refund, marital_status, income, cheat)
df$marital_status = factor(df$marital_status, levels = c('single', 'married', 'divorced'), ordered = T)

# make factor from 0 to 1
df$marital_status = as.numeric(df$marital_status)
df$marital_status = df$marital_status - 1
df$marital_status = df$marital_status / 2 # now it is 0, 0.5, 1 ie in range 0 to 1

# rescale income: minus min and divide by range (max - min)
min(income)
max(income)
max(income) - min(income)

df$income = df$income - 60
df$income = df$income / 160

# calculate similarity between subjects 5 and 7
df[5,]
df[7,]

# similarity between 5 and 7
# binary so just 1 if same otherwise 0
o51 = 0 # o51 is first variable for subject 5
o71 = 1
# so
s571 = 0

# this one is ordinal so treat as numeric 1 - |x - y|
o52 = 1
o72 = 1

s572 = 1 - abs(1-1) # 1

# income is numeric
o53 = 0.21
o73 = 1

s573 = 1 - abs(s53 - s73)

# Cheat is binary
o54 = 1
o74 = 0
# they are different so 0
s574 = 0

S = (s571+s572+s573+s574)/4

# similarity between subjects 6 and 8
df[6,]
df[8,]

# var1 - binary
# they are same so 
s1 = 1

# var2 - ordinal - treat as numeric
s2 = 1 - abs(0.5-0) # 0.5

# var3 = numeric
s3 = 1 - abs(0-0.15625)

# var4 = binary
# they are different so 0
s4 = 0

S = (s1+s2+s3+s4)/4

# can use daisy() function from cluster package
library(cluster)

refund = c(1, 0, 0, 1, 0, 0, 1, 0, 0, 0)
marital_status = c(s, m, s, m, d, m, d, s, m, s)
income = c(125, 100, 70, 120, 95, 60, 220, 85, 75, 90)
cheat = c(n, n, n, n, y, n, n, y, n, y)
df = cbind.data.frame(refund, marital_status, income, cheat)
df$marital_status = factor(df$marital_status, levels = c('single', 'married', 'divorced'), ordered = T)


dis = daisy(df, metric = 'gower', type = list(symm = c(1, 4)))
# daisy calculates the dissimilartiy - need to convert to similarity
# d = 1-s ; s = 1-d
(sim = 1 - dis)
