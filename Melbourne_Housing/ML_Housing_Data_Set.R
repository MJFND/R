#=======================================================================================
#
# File:        ML_Housing_Data_Set.R
# Author:      Junaid Effendi
# Description: Predicting House Prices using XGBoost from Caret Package in R.
#
#=======================================================================================

#install and load required packages
install.packages(c("gridExtra","ggplot2","e1071", "caret", "doSNOW", "ipred", "xgboost", "dplyr"))
library(e1071)
library(caret)
library(doSNOW)
library(ipred)
library(xgboost)
library(dplyr)
library(ggplot2)
library(gridExtra)

#=================================================================
# Load Data
#=================================================================
train <- read.csv("Aus_housing.csv", stringsAsFactors = FALSE)

#=================================================================
# Data Cleaning 
#=================================================================
#checking no. of NA
sapply(train, function(x) sum(is.na(x)))

#removing missing records based on Price and Bedroom2
train <- train[!is.na(train$Price),]
train <- train[!is.na(train$Bedroom2),]

# replacing 0 with NA
nrow(subset(train, BuildingArea == 0))
train$BuildingArea[train$BuildingArea == 0] <- NA
nrow(subset(train, Landsize == 0))
train$Landsize[train$Landsize == 0] <- NA

#=================================================================
# Data Analysis
#=================================================================
str(train)
summary(train)

# For plotting continious individual plots
ggplot(aes(Price),data=train)+
  geom_histogram()

ggplot(aes(YearBuilt),data=train)+
  geom_histogram()

# Multiple Plots
a <- ggplot(aes(Rooms),data=train)+
  geom_bar()

b <- ggplot(aes(Bedroom2),data=subset(train, Bedroom2 < 20))+
  geom_bar()+
  scale_x_discrete(breaks=seq(0,8,2), limits= c(0:8))

c <- ggplot(aes(Bathroom),data=train)+
  geom_bar()

d <- ggplot(aes(Car),data=train)+
  geom_bar()

grid.arrange(a, b, c, d, ncol=2, nrow=2)

# For plotting two continious variables
# Date plot
ggplot(aes(as.Date(Date,"%d/%m/%Y"),Price), data= train)+
  geom_point()

# Multi plot
w <- ggplot(aes(Landsize,Price), data= train)+
  geom_point(alpha=1/3, aes(color= Type))+
  coord_cartesian(xlim = 0:10000)

x <- ggplot(aes(Landsize,Price), data= train)+
  geom_point(alpha=1/3, aes(color= Method))+
  coord_cartesian(xlim = 0:10000)

y <- ggplot(aes(BuildingArea,Price), data= train)+
  geom_point(alpha=1/5, aes(color= Type))+
  coord_cartesian(xlim = 0:1000, ylim = 0:5e+06)

z <- ggplot(aes(BuildingArea,Price), data= train)+
  geom_point(alpha=1/5, aes(color= Method))+
  coord_cartesian(xlim = 0:1000, ylim = 0:5e+06)

grid.arrange(w, x, y, z, ncol=2, nrow=2)

ggplot(aes(Landsize,Rooms), data= train)+
  geom_point(aes(color=factor(Bathroom)), size = 2)+
  coord_cartesian(xlim = 0:2500)+
  scale_colour_brewer(palette = "Set1")

ggplot(aes(Lattitude,Longitude), data= train)+
  geom_point(stat="identity", aes(color= Price), 
             size = 4,  position = position_jitter(height = 0))

ggplot(aes(Lattitude,Longitude), data= train)+
  geom_point(alpha=1/5, aes(color= CouncilArea))


#=================================================================
# Feature Adjustment
#=================================================================
#feature selection
train <- subset(train, select = c(Rooms, Bedroom2, Bathroom, Car, Type,
                                  Method, BuildingArea, YearBuilt, CouncilArea,
                                  Lattitude, Longitude, Price)) 

summary(train$Price)


# Set up factors.
train$Type <- as.factor(train$Type)
train$Method <- as.factor(train$Method)
train$CouncilArea <- as.factor(train$CouncilArea)
str(train)

#=================================================================
# Impute Missing Values
#=================================================================
# For back up of values
temp = data.frame(train$CouncilArea)
train <- subset(train, select = -c(CouncilArea))

# Transform all feature to dummy variables.
dummy.vars <- dummyVars(~ ., data = train[, -1])
train.dummy <- predict(dummy.vars, train[, -1])
View(train.dummy)

# Now, impute!
pre.process <- preProcess(train.dummy, method = "bagImpute")
imputed.data <- predict(pre.process, train.dummy)
View(imputed.data)

#Assignign imputed values to original dataset
train$BuildingArea <- imputed.data[, 12]
train$YearBuilt <- imputed.data[, 13]
train$Lattitude <- imputed.data[, 14]
train$Longitude <- imputed.data[, 15]

#Adding back the CouncilArea
train$CouncilArea <- temp$train.CouncilArea

View(train)

#=================================================================
# Split Data
#=================================================================

# Spliting data 70/30 for training and testing.
set.seed(54321)
indexes <- createDataPartition(train$Price,
                               times = 1,
                               p = 0.7,
                               list = FALSE)
housing.train <- train[indexes,]
housing.test <- train[-indexes,]


#=================================================================
# Train Model
#=================================================================

# Using 10-fold cross validation repeated 3 times
# Using a grid search for optimal model hyperparamter values.
train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              search = "grid")


# Leverage a grid search of hyperparameters for xgboost. See 
# the following presentation for more information:
# https://www.slideshare.net/odsc/owen-zhangopen-sourcetoolsanddscompetitions1
tune.grid <- expand.grid(eta = c(0.05, 0.075, 0.1),
                         nrounds = c(50, 75, 100),
                         max_depth = 6:8,
                         min_child_weight = c(2.0, 2.25, 2.5),
                         colsample_bytree = c(0.3, 0.4, 0.5),
                         gamma = 0,
                         subsample = 1)
View(tune.grid)


# For Parallel Processing use doSNOW
# Tune the number of thread or core based on your PC
cl <- makeCluster(4, type = "SOCK")

# Register cluster so that caret will know to train in parallel.
registerDoSNOW(cl)

# Train the model
caret.cv <- train(Price ~ ., 
                  data = housing.train,
                  method = "xgbTree",
                  tuneGrid = tune.grid,
                  trControl = train.control)

#Close connection
stopCluster(cl)

# Examine caret's processing results
caret.cv


#=================================================================
# Test and Evaluate Model
#=================================================================
#Making Predictions
preds <- predict(caret.cv, housing.test)

#Evaluation using RMSE, R-Squared
postResample(preds,housing.test$Price)

#Evaluation using min max accuracy and MAPE
actuals_preds <- data.frame(cbind(actuals=housing.test$Price, predicteds=preds))
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)


# For examining overfitting lets evaluate training dataset
preds1 <- predict(caret.cv, housing.train)
postResample(preds1,housing.train$Price)
actuals_preds1 <- data.frame(cbind(actuals=housing.train$Price, predicteds=preds1))  # make actuals_predicteds dataframe.
head(actuals_preds1)
min_max_accuracy1 <- mean(apply(actuals_preds1, 1, min) / apply(actuals_preds1, 1, max))  
mape1 <- mean(abs((actuals_preds1$predicteds - actuals_preds1$actuals))/actuals_preds1$actuals)



