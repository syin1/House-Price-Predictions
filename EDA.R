library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)

train <- read.csv("./data/train.csv", stringsAsFactors = F)
test <- read.csv("./data/test.csv", stringsAsFactors = F)


###### Data size and structure ######
dim(train) #[1] 1460   81
str(train[,c(1:10, 81)]) #display first 10 variables and the response variable

test_labels <- test$Id #keeping the test IDs in a vector, for later use
test$Id <- NULL
train$Id <- NULL

test$SalePrice <- NA
all <- rbind(train, test)
dim(all) #[1] 2919   80
#####################################



##### Exploring the most important variables - START #####

# 1. Sale Price - Response

# sale prices are right skewed
ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

summary(all$SalePrice)


# The most important numeric predictors
numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables') # There are 37 numeric variables

all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))

#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

# 2. Overall Quality
# Overall Quality has the highest correlation with SalePrice among the numeric variables (0.79)
ggplot(data=all[!is.na(all$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

# 3. Above Grade (Ground) Living Area (square feet)
# The numeric variable with the second highest correlation with SalesPrice is the Above Grade Living Area
ggplot(data=all[!is.na(all$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), '')))

# potential outliers
all[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverallQual')]
##### Exploring the most important variables - END #####



##### Missing data, label encoding, and factorizing variables - START #####


##### Missing data, label encoding, and factorizing variables - END #####

