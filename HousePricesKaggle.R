# House Prices

## Read Dataset
HousePrices <- read.csv("https://github.com/lypf2018/HousePricesKaggle/raw/master/dataset/train.csv")

## View data and names
View(HousePrices)
names(HousePrices)

## Preprocessing Data
### Preprocessing NA
probNA <- function(x) {
  sum(is.na(x))/length(x)
}

probNACol <- apply(HousePrices, 2, probNA)

names(probNACol[probNACol>0.3])

preprocessedHousePrices <- HousePrices

preprocessedHousePrices[names(probNACol[probNACol>0.3])] <- NULL
preprocessedHousePrices <- na.omit(preprocessedHousePrices)

### Preprocessing Non-numeric, just omit the feature
isNonNumericFeature <- function(x) {
  is.na(as.numeric(x[1]))
}

nonNumericCol <- apply(preprocessedHousePrices, 2, isNonNumericFeature)

names(nonNumericCol[nonNumericCol])

preprocessedHousePrices[names(nonNumericCol[nonNumericCol])] <- NULL

## Visualizing and Plotting the Data

### find correlation between variables and see how they are correlated to each other and to the output variable
require(corrplot)

M <- cor(preprocessedHousePrices[,-1])
corrplot(M, method = "circle")

### Numbered correlation
corMat <- as.data.frame(corrplot(M,method = "number"))

names(corMat) <- names(preprocessedHousePrices[,-1])

### Find out which attributes have correlation of more than 50% with SalePrice
row.names(corMat)[abs(corMat$SalePrice) > 0.50]

