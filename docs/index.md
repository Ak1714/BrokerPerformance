## <b><span style="font-family: Book Antiqua; font-size: 0.8em;">Project Layout</b>

    mkdocs.yml    # The configuration file.
    docs/
        index.md  # The documentation homepage with project summary
		Broker_Performance.md # Predicting broker performance
		LCDA_Part2.md # Clustering & PCA
		LCDA_Part3.md # Building models


## <b><span style="font-family: Book Antiqua; font-size: 0.75em;">Summary</b>
<span style="font-family: Times New Roman; font-size: 1em;">Objective: To evaluate and predict brokers performance for an insurance company so that the company can better manage future plans, and objectives. This can be done by partitioning brokers into segments by applying a clustering methodologies based on the most recent years of information (ending w/ 2018). Then we need to predict if the gross written premium will either increase or decrease for 2018 and 2019.
<br><span style="font-family: Times New Roman; font-size: 1em;">Tasks for this project are;
<br>1. Broker Segmentation
<br>2. Build a robust machine learning model to forecast whether “Growth Written Premium” will increase or decrease for FY 2019 


## <b><span style="font-family: Book Antiqua; font-size: 0.75em;">Data Preprocessing</b>
<span style="font-family: Times New Roman; font-size: 1em;">We begin by loading all necessary libraries and setting a random seed generator
``` r
library(dplyr)
library(reshape2)
library(caret)
library(rpart)
library(cluster)
library(rgl) # 3d
library(fpc)
library(dummies)
library(ggplot2)
library(ROCR)
library(randomForest)
library(GGally)


set.seed(12345)
```

## <b><span style="font-family: Book Antiqua; font-size: 0.75em;">Loading data</b>
<span style="font-family: Times New Roman; font-size: 1em;">The data has 30 columns and a total of 189 rows. The .csv file was read using the “read.table” function and saved as .rda file. 


``` r
load("alchemyBrokerData.rda")
```
<span style="font-family: Times New Roman; font-size: 1em;">We can find the total number of missing values by using the sum(is.na()) function; we can see the data has 1323 NAs.

``` r
# Calculating NAs
sum(is.na(brokerData))
```

    ## [1] 1323

``` r
# Checking the number of NULLs in submissions from 2015 to 2018 (max NAs in 2015)
sum(is.na(brokerData$Submissions_2015))
sum(is.na(brokerData$Submissions_2016))
sum(is.na(brokerData$Submissions_2017))
sum(is.na(brokerData$Submissions_2018))
```

    ## [1] 60

    ## [1] 45

    ## [1] 30

    ## [1] 8

## <b><span style="font-family: Book Antiqua; font-size: 0.75em;">Handling missing values</b>

<span style="font-family: Times New Roman; font-size: 1em;">Some of the techniques for imputing missing values are; deleting all rows that contained NA’s, imputing missing data with the mean or mode, and/or replacing missing values with zero's.
However, none of these are appropriate for this case which brings us to the next best option: using either a classification or regression method for predicting and imputing the missing information to handle the missing values.

``` r
# Creating a data frame with all the required variables
myBDF <- brokerData %>% dplyr::select(Submissions_2014:AvgTIV_2018)

# We will use the entire dataset for prediction of NAs and then get rid of the excess columns (2013 & 2014)

# 2015
sub2015Rpart <- rpart(myBDF$Submissions_2015 ~ ., data=myBDF)
sub2015pred <- round(predict(sub2015Rpart, newdata=myBDF, type="vector"))
myBDF$Submissions_2015[is.na(myBDF$Submissions_2015)] <- 
  sub2015pred[is.na(myBDF$Submissions_2015)]

# 2016
sub2016Rpart <- rpart(myBDF$Submissions_2016 ~ ., data=myBDF)
sub2016pred <- round(predict(sub2016Rpart, newdata=myBDF, type="vector"))
myBDF$Submissions_2016[is.na(myBDF$Submissions_2016)] <- 
  sub2016pred[is.na(myBDF$Submissions_2016)]

# 2017 
sub2017Rpart <- rpart(myBDF$Submissions_2017 ~ ., data=myBDF)
sub2017pred <- round(predict(sub2017Rpart, newdata=myBDF, type="vector"))
myBDF$Submissions_2017[is.na(myBDF$Submissions_2017)] <- 
  sub2017pred[is.na(myBDF$Submissions_2017)]

# 2018 
sub2018Rpart <- rpart(myBDF$Submissions_2018 ~ ., data=myBDF)
sub2018pred <- round(predict(sub2018Rpart, newdata=myBDF, type="vector"))
myBDF$Submissions_2018[is.na(myBDF$Submissions_2018)] <- 
  sub2018pred[is.na(myBDF$Submissions_2018)]

# 2015
quote2015RPART <- rpart(formula = QuoteCount_2015 ~ ., data=myBDF)
quote2015pred <- round(predict(quote2015RPART, newdata=myBDF))
myBDF$QuoteCount_2015[is.na(myBDF$QuoteCount_2015)] <- 
  quote2015pred[is.na(myBDF$QuoteCount_2015)]

# 2016
quote2016RPART <- rpart(formula = QuoteCount_2016 ~ ., data=myBDF)
quote2016pred <- round(predict(quote2016RPART, newdata=myBDF))
myBDF$QuoteCount_2016[is.na(myBDF$QuoteCount_2016)] <- 
  quote2016pred[is.na(myBDF$QuoteCount_2016)]

# 2017 
quote2017RPART <- rpart(formula = QuoteCount_2017 ~ ., data=myBDF)
quote2017pred <- round(predict(quote2017RPART, newdata=myBDF))
myBDF$QuoteCount_2017[is.na(myBDF$QuoteCount_2017)] <- 
  quote2017pred[is.na(myBDF$QuoteCount_2017)]


# 2018
quote2018RPART <- rpart(formula = QuoteCount_2018 ~ ., data=myBDF)
quote2018pred <- round(predict(quote2018RPART, newdata=myBDF))
myBDF$QuoteCount_2018[is.na(myBDF$QuoteCount_2018)] <- 
  quote2018pred[is.na(myBDF$QuoteCount_2018)]

# Policy count

# 2015
policy2015RPART <- rpart(formula = PolicyCount_2015 ~ ., data=myBDF)
pol2015pred <- round(predict(policy2015RPART, newdata=myBDF))
myBDF$PolicyCount_2015[is.na(myBDF$PolicyCount_2015)] <- 
  pol2015pred[is.na(myBDF$PolicyCount_2015)]

# 2016
policy2016RPART <- rpart(formula = PolicyCount_2016 ~ ., data=myBDF)
pol2016pred <- round(predict(policy2016RPART, newdata=myBDF))
myBDF$PolicyCount_2016[is.na(myBDF$PolicyCount_2016)] <- 
  pol2016pred[is.na(myBDF$PolicyCount_2016)]

# 2017 
policy2017RPART <- rpart(formula = PolicyCount_2017 ~ ., data=myBDF)
pol2017pred <- round(predict(policy2017RPART, newdata=myBDF))
myBDF$PolicyCount_2017[is.na(myBDF$PolicyCount_2017)] <- 
  pol2017pred[is.na(myBDF$PolicyCount_2017)]

# 2018 
policy2018RPART <- rpart(formula = PolicyCount_2018 ~ ., data=myBDF)
pol2018pred <- round(predict(policy2018RPART, newdata=myBDF))
myBDF$PolicyCount_2018[is.na(myBDF$PolicyCount_2018)] <- 
  pol2018pred[is.na(myBDF$PolicyCount_2018)]

# AvgQuote

# 2015
avgq2015RPART <- rpart(formula = AvgQuote_2015 ~ ., data=myBDF)
avgq2015pred <- (predict(avgq2015RPART, newdata=myBDF))
myBDF$AvgQuote_2015[is.na(myBDF$AvgQuote_2015)] <- 
  avgq2015pred[is.na(myBDF$AvgQuote_2015)]

# 2016
avgq2016RPART <- rpart(formula = AvgQuote_2016 ~ ., data=myBDF)
avgq2016pred <- (predict(avgq2016RPART, newdata=myBDF))
myBDF$AvgQuote_2016[is.na(myBDF$AvgQuote_2016)] <- 
  avgq2016pred[is.na(myBDF$AvgQuote_2016)]

# 2017 
avgq2017RPART <- rpart(formula = AvgQuote_2017 ~ ., data=myBDF)
avgq2017pred <- (predict(avgq2017RPART, newdata=myBDF))
myBDF$AvgQuote_2017[is.na(myBDF$AvgQuote_2017)] <- 
  avgq2017pred[is.na(myBDF$AvgQuote_2017)]

# 2018 
avgq2018RPART <- rpart(formula = AvgQuote_2018 ~ ., data=myBDF)
avgq2018pred <- (predict(avgq2018RPART, newdata=myBDF))
myBDF$AvgQuote_2018[is.na(myBDF$AvgQuote_2018)] <- 
  avgq2018pred[is.na(myBDF$AvgQuote_2018)]

# TIV

# 2015
TIV2015RPART <- rpart(formula = AvgTIV_2015 ~ ., data=myBDF)
TIV2015pred <- round(predict(TIV2015RPART, newdata=myBDF))
myBDF$AvgTIV_2015[is.na(myBDF$AvgTIV_2015)] <- 
  TIV2015pred[is.na(myBDF$AvgTIV_2015)]

# 2016
TIV2016RPART <- rpart(formula = AvgTIV_2016 ~ ., data=myBDF)
TIV2016pred <- round(predict(TIV2016RPART, newdata=myBDF))
myBDF$AvgTIV_2016[is.na(myBDF$AvgTIV_2016)] <- 
  TIV2016pred[is.na(myBDF$AvgTIV_2016)]

# 2017 
TIV2017RPART <- rpart(formula = AvgTIV_2017 ~ ., data=myBDF)
TIV2017pred <- round(predict(TIV2017RPART, newdata=myBDF))
myBDF$AvgTIV_2017[is.na(myBDF$AvgTIV_2017)] <- 
  TIV2017pred[is.na(myBDF$AvgTIV_2017)]

# 2018 
TIV2018RPART <- rpart(formula = AvgTIV_2018 ~ ., data=myBDF)
TIV2018pred <- round(predict(TIV2018RPART, newdata=myBDF))
myBDF$AvgTIV_2018[is.na(myBDF$AvgTIV_2018)] <- 
  TIV2018pred[is.na(myBDF$AvgTIV_2018)]

# GWP

# 2015
GWP2015RPART <- rpart(formula = GWP_2015 ~ ., data=myBDF)
GWP2015pred <- (predict(GWP2015RPART, newdata=myBDF))
myBDF$GWP_2015[is.na(myBDF$GWP_2015)] <- 
  GWP2015pred[is.na(myBDF$GWP_2015)]

# 2016
GWP2016RPART <- rpart(formula = GWP_2016 ~ ., data=myBDF)
GWP2016pred <- (predict(GWP2016RPART, newdata=myBDF))
myBDF$GWP_2016[is.na(myBDF$GWP_2016)] <- 
  GWP2016pred[is.na(myBDF$GWP_2016)]

# 2017 
GWP2017RPART <- rpart(formula = GWP_2017 ~ ., data=myBDF)
GWP2017pred <- (predict(GWP2017RPART, newdata=myBDF))
myBDF$GWP_2017[is.na(myBDF$GWP_2017)] <- 
  GWP2017pred[is.na(myBDF$GWP_2017)]

# 2018 
GWP2018RPART <- rpart(formula = GWP_2018 ~ ., data=myBDF)
GWP2018pred <- (predict(GWP2018RPART, newdata=myBDF))
myBDF$GWP_2018[is.na(myBDF$GWP_2018)] <- 
  GWP2018pred[is.na(myBDF$GWP_2018)]
```


