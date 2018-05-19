# Clearing the work space
rm(list = ls())

# Reading the data
train = read.csv('train.csv')
sum(is.na(train))
# No NA's in the data set

length(colnames(train))
head(colnames(train))

# summary of times taken for testing
summary(train$y)
boxplot(train$y)

# Function to get the type of columns
getintcols = function(x,p = "integer"){
  
  vec = character()
  for(i in 1:ncol(x)){
    vec[i] = class(x[,i])
  }
  if(p=="integer"){a = which(vec == "integer" | vec == "numeric")}
  if(p=="character"){a = which(vec == "character")}
  if(p=="factor"){a = which(vec == "factor")}
  if(p=="logical"){a = which(vec == "logical")}
  
  # print(a)
  print(colnames(train[,a]))
}

# 370 columns are integer type and
# 8 columns are factor type
length(getintcols(train,"integer"))
length(getintcols(train,"factor"))

# All IDs are unique
length(unique(train$ID))


a = integer()
for(i in 11:378){
 a[i-10] = length(unique(train[,i]))
}
table(a)
which(a == 1)

temp = train[,11:378]
# Just wanted to check the variance distribution
# but do not use it as we are dealing with
# factor data
# # Principal component Analysis
# # First 25 PC's can be considered
# temp = train[,11:378]
# pca = prcomp(temp)
# screeplot(pca,type="lines",npcs = 200)
# # First 25 PC's can be considered
# factors <- pca$x[,1:25]
# head(factors)
# summary(factors)

m = rowSums(temp)
summary(m)
table(m)
m = as.matrix(m)
colnames(m) = 'feats'

boxplot(m)
# On an average 58 features are being used

# Let us check the times taken by the car 
# for different number of features
# we shall bin the timings

# 38,53,58,63,77 ---> features binning
times = train$y
times = as.data.frame(times)

times$features = m  
times$bin = NA
times$bin =                      
ifelse((times$features > 77),6,
       ifelse((times$features <=77 & times$features >63),5,
              ifelse((times$features <=63 & times$features >58),4,
                     ifelse((times$features <=58 & times$features >53),3,
                            ifelse((times$features <=53 & times$features >38),2,1)))))

table(times$bin)

library(ggplot2)
ggplot(times,aes(as.factor(bin),times,color = as.factor(bin))) + 
  geom_point() + 
  geom_boxplot() +
  ylim(0,200)
# For more number of features more testing time is required

for(i in 3:10){
 print(unique(train[,i]))
}


# Convert all the variables except y & ID into factors


