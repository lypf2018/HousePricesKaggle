# House Prices

## 0.Introduction

# we use the dataset: House Prices: Advanced Regression Techniques https://www.kaggle.com/c/house-prices-advanced-regression-techniques/overview
# We put it on the url: https://github.com/lypf2018/HousePricesKaggle/raw/master/dataset/train.csv
# There are 1460 rows and 81 columns and the last column “SalePrice” is the output variable

## Read Dataset
HousePrices <- read.csv("https://github.com/lypf2018/HousePricesKaggle/raw/master/dataset/train.csv")

## View data and names
View(HousePrices)
names(HousePrices)

## 1.Preprocessing Data

### 1.1 Remove NA Values
probNA <- function(x) {
  sum(is.na(x))/length(x)
}

probNACol <- apply(HousePrices, 2, probNA)

thirtyPctUpNaColNames <- names(probNACol[probNACol>0.3])

# Obviously, there are many NA values(percentage of NA values bigger than 30%) in above 5 columns. We decided to remove these columns then remove all other NA values.

preprocessedHousePrices <- HousePrices
preprocessedHousePrices[thirtyPctUpNaColNames]
preprocessedHousePrices[thirtyPctUpNaColNames] <- NULL
preprocessedHousePrices <- na.omit(preprocessedHousePrices)

### 1.2 Remove Non-numeric Freatures
# Since there are enough features(80 features) in our data, we decide to directly remove all non-numeric features. The numeric features also include some categorical data, which use numbers to present different categories.
isNonNumericFeature <- function(x) {
  any(is.na(as.numeric(x)))
}

nonNumericCol <- apply(preprocessedHousePrices, 2, isNonNumericFeature)

names(nonNumericCol[nonNumericCol])

preprocessedHousePrices[names(nonNumericCol[nonNumericCol])] <- NULL
preprocessedHousePrices <- na.omit(preprocessedHousePrices)

## 2. SalesPrice Analysis
summary(preprocessedHousePrices$SalePrice)
hist(preprocessedHousePrices$SalePrice, ylim = c(0, 8*10^-6), probability = TRUE)
lines(density(preprocessedHousePrices$SalePrice))

# **finding:**
#   1. From summary of SalesPrice: Minimal house price is largger than 0, so it would not destroy our model.
#
# 2. From histogram of SalesPrice: the distribution
#
# a. Deviate from the normal distribution.
# b. Have appreciable positive skewness.
# c. Show peakedness.
#
# It seems that there are few extremly rich peolple bought very expensive houses, which makes the distribution has a long tail on the right, that is, positive skewness.


## 2. Correlation Analysis

### 2.1 Correlation Plot

require(MASS)
require(ISLR)
require(corrplot)

M <- cor(preprocessedHousePrices[,-1])
corMat <- as.data.frame(corrplot(M,method = "circle"))

### Numbered correlation
names(corMat) <- names(preprocessedHousePrices[,-1])

### Find out which attributes have correlation of more than 50% with SalePrice
row.names(corMat)[abs(corMat$SalePrice) > 0.50]

corrplot(cor(preprocessedHousePrices[row.names(corMat)[abs(corMat$SalePrice) > 0.50]]), method = "number")

# **finding:**
# From the  correlation plot we can see the 11 features with the strongest effect(correlation > 0.5) on SalePrice:
# 
# 1.OverallQual: Rates the overall material and finish of the house. (1 very poor - 10 very excellent).
# 
# 2.YearBuilt: Original construction date.
# 
# 3.YearRemodAdd: Remodel date (same as construction date if no remodeling or additions).
# 
# 4.TotalBsmtSF: Total square feet of basement area.
# 
# 5.X1stFlrSF: First Floor square feet.
# 
# 6.GrLivArea: Above grade (ground) living area square feet.
# 
# 7.FullBath: Full bathrooms above grade.
# 
# 8.TotRmsAbvGrd: Total rooms above grade (does not include bathrooms).
# 
# 9.GarageYrBlt: Year garage was built.
# 
# 10.GarageCars: Size of garage in car capacity.
# 
# 11.GarageArea: Size of garage in square feet.
# 
# All of them are positive correlations.
# 
# We can see there are also some strong relations between these features. For example, correlation between TotalBsmtSF and X1stFlrSF is 0.91, correlation between GrLivArea and TotRmsAbvGrd is 0.83. That make sense since a big basement area is always together with a big area first floor, and the size of the living area will most likely be a constraint on the number of rooms above ground.


### 2.2 Scatter Plots

# We can print a matrix of scatter plots to see what the relationships between features look like.

require(GGally)
corr.idx <- row.names(corMat)[abs(corMat$SalePrice) > 0.5]
lm.plt <- function(data, mapping, ...){
  plt <- ggplot(data = data, mapping = mapping) + 
    geom_point(shape = 20, alpha = 0.7, color = 'darkseagreen') +
    geom_smooth(method=loess, fill="red", color="red") +
    geom_smooth(method=lm, fill="blue", color="blue") +
    theme_minimal()
  return(plt)
}
ggpairs(preprocessedHousePrices, corr.idx[c(12,1:6)], lower = list(continuous = lm.plt))
ggpairs(preprocessedHousePrices, corr.idx[c(12,7:11)], lower = list(continuous = lm.plt))

### 2.3 Single Feature Analysis 

#### 2.3.1 categorical feature
lm.fit = lm(SalePrice~OverallQual, data = preprocessedHousePrices)
summary(lm.fit) 
boxplot(preprocessedHousePrices$SalePrice~preprocessedHousePrices$OverallQual)
abline(lm.fit,lwd = 2, col="red")

# OverallQual:Rates the overall material and finish of the house. (1 very poor - 10 very excellent).
# 
# Frome above statistics and graph we can see: 
#   
# The relationship between Salesprice and OverallQual is linear relationship and OverallQual is an important factor.
# 
# When overall quality of houses are around medium quality(rate:3-7), house prices are concentrated. However, when overall quality of houses are above excellent(rate:9-10), house prices are dispersed, which means the range of house prices can be very large (more than 6e+05). This may be because some other factors like the living area, number of rooms and overall conditions.

lm.fit2 = lm(SalePrice~GarageCars, data = preprocessedHousePrices)
summary(lm.fit2) 
boxplot(preprocessedHousePrices$SalePrice~preprocessedHousePrices$GarageCars)
abline(lm.fit2,lwd = 2, col="red")

# **finding:**
# When car capacity in one garage is 4, the house price decreases, which means people may think it's unnecessary to have such a big garage in a house.

#### 2.3.2 numeric feature

lm.fit3 = lm(SalePrice~GrLivArea, data = preprocessedHousePrices)
summary(lm.fit3) 
plot(preprocessedHousePrices$GrLivArea, preprocessedHousePrices$SalePrice)
abline(lm.fit3,lwd = 2, col="red")

# **finding**：
# This distribution is almost perfect linear.

lm.fit4 = lm(SalePrice~TotalBsmtSF, data = preprocessedHousePrices)
summary(lm.fit4)
plot(preprocessedHousePrices$TotalBsmtSF, preprocessedHousePrices$SalePrice)
abline(lm.fit4,lwd = 2, col="red")

### 2.4 Multiple Feature Analysis 

lm.fit5 = lm(SalePrice~OverallQual+GarageCars+GrLivArea, data = preprocessedHousePrices)
summary(lm.fit5)

lm.fit6 = lm(SalePrice~OverallQual+GarageCars+GrLivArea+TotalBsmtSF+BsmtUnfSF+MSSubClass, data = preprocessedHousePrices)
summary(lm.fit6)

## 3.Compare Models

anova(lm.fit,lm.fit5)
AIC(lm.fit,lm.fit5)

# **finding:**
# From above statitics we can see:
# 
# result of fuction anova : p-value is 2.2e-16
# 
# result of AIC: AIC value of model is smaller
# 
# The multiple linear regression model with three varibles(OverallQual, GarageCars, GrLivArea) is better than linear regression model with only one varible(OverallQual).

anova(lm.fit5,lm.fit6)
AIC(lm.fit5,lm.fit6)

# **finding:**
#   From above statitics we can see: 
#   
#   result of fuction anova : p-value is 2.2e-16
# 
# result of AIC: AIC value of model is smaller
# 
# The multiple linear regression model with six varibles(OverallQual, GarageCars, GrLivArea, TotalBsmtSF, BsmtUnfSF, MSSubClass) is better than multiple linear regression model with three varibles(OverallQual, GarageCars, GrLivArea).

