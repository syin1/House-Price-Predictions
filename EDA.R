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

##### Exploring the most important variables #####

# 1. Sale Price - Response

# sale prices are right skewed
ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

summary(all$SalePrice)


# 2. Numeric Variables in relation to sale price
numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables') # There are 37 numeric variables






