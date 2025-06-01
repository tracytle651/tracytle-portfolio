#Kaggle Housing Project
#Machine Learning - Spring 2023
#Thuong Le 

#Load relevant packages from user library
library(dplyr) 
library(tidyr)
library(ggplot2) 
library(onehot)  
library(MASS)
library(e1071) 
library(tidyverse)
library(car) 
library(MASS) 
library(glmnet)
library(tree)
library(gbm)
library(randomForest)

# Configure select() so that it works with MASS package loaded
select <- dplyr::select

#Read two sets into R
train <-read.csv("train.csv")
test <-read.csv("test.csv")

#Combine training and test datasets
test$SalePrice <- NA
train$isTrain <- 1
test$isTrain <- 0
combined_set <- rbind(train,test)

cleaned_combined_set <- combined_set

#Set the seed to create reproducible results 
set.seed(123)

#Let's see if there exists any duplicate observations.
count(cleaned_combined_set[duplicated(cleaned_combined_set) == TRUE, ])
#No duplicates.

###INITIAL DATA CLEANING

##COLUMN 1: Id 
sum(is.na(cleaned_combined_set$Id))
#0 missing value 
#Id is just an index for each individual observation, so no further action is needed.

##COLUMN 2: MSSubClass
sum(is.na(cleaned_combined_set$MSSubClass))
summary(factor(cleaned_combined_set$MSSubClass))
#0 missing value
#To avoid the problem later in the modeling stage, in which level "150"
#of MSSubclass belongs to the training dataset but does not exist in 
#the test dataset, I merge this level to the nearby level to avoid the issue.
cleaned_combined_set$MSSubClass[cleaned_combined_set$MSSubClass == "150"] <- 160
cleaned_combined_set$MSSubClass <- as.factor(cleaned_combined_set$MSSubClass)

##COLUMN 3: MSZoning
sum(is.na(cleaned_combined_set$MSZoning))
#There are 4 missing values
#Therefore, I replace the missing values by the most frequent level for MSZoning
summary(factor(cleaned_combined_set$MSZoning))
#RL is the most frequent value for MSZoning, so I assign it to missing values
cleaned_combined_set$MSZoning[is.na(cleaned_combined_set$MSZoning)] <- "RL"
sum(is.na(cleaned_combined_set$MSZoning))
#Now we don't have any missing values
#Change the MSZoning "C (all)" level to "C" to match the data description.
cleaned_combined_set$MSZoning <- as.factor(cleaned_combined_set$MSZoning)
levels(cleaned_combined_set$MSZoning)[levels(cleaned_combined_set$MSZoning) == "C (all)"] <- "C"
summary(factor(cleaned_combined_set$MSZoning))

##COLUMN 4: LotFrontage
cleaned_combined_set$LotFrontage <- as.numeric(cleaned_combined_set$LotFrontage)
sum(is.na(cleaned_combined_set$LotFrontage))
#There are 486 missing values
#I replace missing values with the mean that corresponds to the category of MSZoning.
#Then, I calculate the mean value of LotFrontage for each factor of MSZoning when
#LotFrontage is not an NA value
cleaned_combined_set %>%
  group_by(MSZoning) %>%
  select(MSZoning, LotFrontage) %>%
  summarize(mean = mean(LotFrontage, na.rm = TRUE))
#Mean LotFrontage for each type of MSZoning
#  C         65.6
#  FV        59.5
#  RH        55.4
#  RL        74.1
#  RM        52.2
#Impute the mean LotFrontage by MSZoning into all NAs of LotFrontage
cleaned_combined_set$LotFrontage[which((cleaned_combined_set$MSZoning == "C" & is.na(cleaned_combined_set$LotFrontage)))] <- 65.6
cleaned_combined_set$LotFrontage[which((cleaned_combined_set$MSZoning == "FV" & is.na(cleaned_combined_set$LotFrontage)))] <- 59.5
cleaned_combined_set$LotFrontage[which((cleaned_combined_set$MSZoning == "RH" & is.na(cleaned_combined_set$LotFrontage)))] <- 55.4
cleaned_combined_set$LotFrontage[which((cleaned_combined_set$MSZoning == "RL" & is.na(cleaned_combined_set$LotFrontage)))] <- 74.1
cleaned_combined_set$LotFrontage[which((cleaned_combined_set$MSZoning == "RM" & is.na(cleaned_combined_set$LotFrontage)))] <- 52.2
sum(is.na(cleaned_combined_set$LotFrontage))
#Identify outliers.
boxplot(cleaned_combined_set$LotFrontage)
ggplot(cleaned_combined_set, aes(SalePrice, LotFrontage)) + geom_point()
#Outliers of LotFrontage seem to occur around 300.
#Replace outliers with mean.
cleaned_combined_set$LotFrontage <- ifelse(cleaned_combined_set$LotFrontage > 300, 
                                           mean(cleaned_combined_set$LotFrontage), 
                                           cleaned_combined_set$LotFrontage)
#Skewness 
ggplot(cleaned_combined_set, aes(LotFrontage)) + geom_histogram()
skewness(cleaned_combined_set$LotFrontage)
#The value is approximately 0.652, which is between 0.5 and 1, 
#so it is moderately skewed.
#No transformation

##COLUMN 5: LotArea
cleaned_combined_set$LotArea <- as.numeric(cleaned_combined_set$LotArea)
sum(is.na(cleaned_combined_set$LotArea))
#No missing value.
#Identify outliers.
boxplot(cleaned_combined_set$LotArea)
ggplot(cleaned_combined_set, aes(SalePrice, LotArea)) + geom_point()
#Outliers seem to occur around 100,000.
#Replace outliers with mean.
cleaned_combined_set$LotArea <- ifelse(cleaned_combined_set$LotArea > 100000, 
                                       mean(cleaned_combined_set$LotArea), 
                                       cleaned_combined_set$LotArea)
#Skewness
ggplot(cleaned_combined_set, aes(LotArea)) + geom_histogram()
skewness(cleaned_combined_set$LotArea)
#The value is 3.75, highly skewed.
#The histogram also displays right skewness.
#Try log transformation.
ggplot(cleaned_combined_set, aes(log10(LotArea))) + geom_histogram()
skewness(log10(cleaned_combined_set$LotArea))
#The value is -0.788, only moderately skewed, which is good 
cleaned_combined_set$LotArea <- log10(cleaned_combined_set$LotArea)

##COLUMN 6: Street
sum(is.na(cleaned_combined_set$Street))
summary(factor(cleaned_combined_set$Street))
#No missing value or typo
#Only 12 out of 2919 cases are Grvl, meanwhile the rest are Pave.
#Therefore, Street should be removed because 
#it won't provide any useful insights in the modeling.
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(Street))

##COLUMN 7: Alley
sum(is.na(cleaned_combined_set$Alley))
summary(factor(cleaned_combined_set$Alley))
#There are 2721 missing values, which means "No alley access" according to 
#the description
#However, given that 2721 out of 2919 obs belong to "No alley access", 
#I am removing this varable
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(Alley))

##COLUMN 8: LotShape
sum(is.na(cleaned_combined_set$LotShape))
summary(factor(cleaned_combined_set$LotShape))
#0 mising value or typo
cleaned_combined_set$LotShape<- factor(cleaned_combined_set$LotShape, order = TRUE, 
                                       levels = c("IR3", "IR2", "IR1", "Reg"))

##COLUMN 9: LandContour
sum(is.na(cleaned_combined_set$LandContour))
summary(factor(cleaned_combined_set$LandContour))
#No missing value or typo
cleaned_combined_set$LandContour <- as.factor(cleaned_combined_set$LandContour)
#Given that 2622 observations belong to "Lv1", it won't provide much insightful 
#information, I will remove this variable
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(LandContour))

##COLUMN 10: Utilities
sum(is.na(cleaned_combined_set$Utilities))
summary(factor(cleaned_combined_set$Utilities))
#There are 2 missing values
#In addition, only one case is NoSeWa, and the rest are AllPub.
##Therefore, Utilities should be removed because it won't provide any useful 
#insights in the modeling.
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(Utilities))

##COLUMN 11: LotConfig
sum(is.na(cleaned_combined_set$LotConfig))
summary(factor(cleaned_combined_set$LotConfig))
#No missing value or typo
cleaned_combined_set$LotConfig <- as.factor(cleaned_combined_set$LotConfig)

##COLUMN 12: LandSlope
sum(is.na(cleaned_combined_set$LandSlope))
summary(factor(cleaned_combined_set$LandSlope))
#No missing value or typo
#LandSlop might be considered for removal 
#because 2778 cases belong to Gtl factor, where as only 16 cases belong to Sev
#and 125 case belong to Mod out of 2919 obs
#Including LandSplope won't provide useful information.
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(LandSlope))

##COLUMN 13: Neighborhood
sum(is.na(cleaned_combined_set$Neighborhood))
summary(factor(cleaned_combined_set$Neighborhood))
#No missing value or typo
cleaned_combined_set$Neighborhood <- as.factor(cleaned_combined_set$Neighborhood)

##COLUMN 14: Condition1
sum(is.na(cleaned_combined_set$Condition1))
summary(factor(cleaned_combined_set$Condition1))
#No missing value
cleaned_combined_set$Condition1 <- as.factor(cleaned_combined_set$Condition1)
#Condition1 should be removed because
#2511 out of 2919 cases belong to the Norm factor.
#Including Condition1 in the model won't provide much insightful information.
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(Condition1))

##COLUMN 15: Condition2
sum(is.na(cleaned_combined_set$Condition2))
summary(factor(cleaned_combined_set$Condition2))
#No missing value or typo
#Condition2 should be removed because
#2889 out of 2919 cases belong to the Norm factor.
#Including Condition2 in the model won't provide new information.
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(Condition2))

##COLUMN 16: BldgType
sum(is.na(cleaned_combined_set$BldgType))
#No missing value 
summary(factor(cleaned_combined_set$BldgType))
#There is an error/typo in "Twnhs" because it doesn't match with TwnhsI in the data set description.
#I will replace values "Twnhs" with "TWnhsI" because there are no values for "TWnhsI".
cleaned_combined_set$BldgType[cleaned_combined_set$BldgType == "Twnhs"] <- "TwnhsI"
cleaned_combined_set$BldgType <- as.factor(cleaned_combined_set$BldgType)
summary(factor(cleaned_combined_set$BldgType))
#2425 observations belong to "1Fam", which won't provide much insightful information
#I will remove this variable
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(BldgType))

##COLUMN 17: HouseStyle
sum(is.na(cleaned_combined_set$HouseStyle))
summary(factor(cleaned_combined_set$HouseStyle))
#No missing value or typo
cleaned_combined_set$HouseStyle <- as.factor(cleaned_combined_set$HouseStyle)

##COLUMN 18: OverallQual
sum(is.na(cleaned_combined_set$OverallQual))
summary(factor(cleaned_combined_set$OverallQual))
#No missing value or typo
cleaned_combined_set$OverallQual <- factor(cleaned_combined_set$OverallQual, order = TRUE, 
                                           levels = c("1", "2", "3","4", "5", "6", "7", "8","9","10"))

##COLUMN 19: OverallCond
sum(is.na(cleaned_combined_set$OverallCond))
summary(factor(cleaned_combined_set$OverallCond))
#No missing value
cleaned_combined_set$OverallCond <- factor(cleaned_combined_set$OverallCond, order = TRUE, 
                                           levels = c("1", "2", "3","4", "5", "6", "7", "8","9","10"))

##COLUMN 20: YearBuilt
sum(is.na(cleaned_combined_set$YearBuilt))
#No missing value
#Create a new variable called "HomeAge"
cleaned_combined_set$HomeAge <- (2022 - cleaned_combined_set$YearBuilt) +1

##COLUMN 21: YearRemodAdd
sum(is.na(cleaned_combined_set$YearRemodAdd))
cleaned_combined_set$YearRemodAdd <- as.numeric(cleaned_combined_set$YearRemodAdd)
#No missing value
#Create new variable called "AgeSinceRemod"
cleaned_combined_set$AgeSinceRemod <- (2022 - cleaned_combined_set$YearRemodAdd) +1
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(YearRemodAdd))

##COLUMN 22: RoofStyle
sum(is.na(cleaned_combined_set$RoofStyle))
summary(factor(cleaned_combined_set$RoofStyle))
#No missing value
cleaned_combined_set$RoofStyle <- as.factor(cleaned_combined_set$RoofStyle)
#Considering removing this variable since 2310 observations belong to "Gable",
#Which won't provide much insightful information
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(RoofStyle))

##COLUMN 23: RoofMatl
sum(is.na(cleaned_combined_set$RoofMatl))
summary(factor(cleaned_combined_set$RoofMatl))
#No missing value or typo
#RoofMatl should be removed because
#2876 out of 2919 cases belong to the CompShg factor.
#Including RoofMatl in the model won't provide much new information.
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(RoofMatl))

##COLUMN 24: Exterior1st
sum(is.na(cleaned_combined_set$Exterior1st))
#There is 1 missing value and no typo
summary(factor(cleaned_combined_set$Exterior1st))
#Assign the most frequent value of this variable, which is "VinylSd"
#to the missing value
cleaned_combined_set$Exterior1st[is.na(cleaned_combined_set$Exterior1st)] <- "VinylSd"
cleaned_combined_set$Exterior1st <- as.factor(cleaned_combined_set$Exterior1st)

##COLUMN 25: Exterior2nd
sum(is.na(cleaned_combined_set$Exterior2nd))
#There is 1 missing value and no typo
summary(factor(cleaned_combined_set$Exterior2nd))
#Assign the most frequent value of this variable, which is "VinylSd",
#to the missing value
cleaned_combined_set$Exterior2nd[is.na(cleaned_combined_set$Exterior2nd)] <- "VinylSd"
cleaned_combined_set$Exterior2nd <- as.factor(cleaned_combined_set$Exterior2nd)

#Check whether Exterior1st and Exterior2nd are the same thing or maybe have any correlations
ggplot(cleaned_combined_set, aes(Exterior1st, SalePrice)) + geom_point()
ggplot(cleaned_combined_set, aes(Exterior2nd, SalePrice)) + geom_point()
#The plots are approximately identical.
#Let's see the frequency of each level.
summary(factor(cleaned_combined_set$Exterior1st))
summary(factor(cleaned_combined_set$Exterior2nd))
#Frequencies between two variables for the same level are similar. 
#I will remove one the two variables
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(Exterior2nd))

##COLUMN 26: MasVnrType
sum(is.na(cleaned_combined_set$MasVnrType))
summary(factor(cleaned_combined_set$MasVnrType))
#24 missing values and no typo
#Replace missing values with the most frequent value, which is "None" in this case.
cleaned_combined_set$MasVnrType[is.na(cleaned_combined_set$MasVnrType)] <- "None"
cleaned_combined_set$MasVnrType <- as.factor(cleaned_combined_set$MasVnrType)

##COLUMN 27: MasVnrArea
sum(is.na(cleaned_combined_set$MasVnrArea))
#23 missing values
#MasVnrArea and MasVnrType are related according to the description.
#Need to view the fields with missing values and decide what to do.
cleaned_combined_set %>%
  select(MasVnrType, MasVnrArea) %>%
  filter(is.na(MasVnrArea))
#Missing values of MasVnrArea correspond to MasVnrType = None (i.e., no Masonry veneer).
#So we need to replace missing values with 0.
cleaned_combined_set$MasVnrArea[is.na(cleaned_combined_set$MasVnrArea)] <- 0
#outliers
boxplot(cleaned_combined_set$MasVnrArea)
ggplot(cleaned_combined_set, aes(SalePrice, MasVnrArea)) + geom_point()
#Outliers seem to occur around 1500.
#Replace outliers with mean.
cleaned_combined_set$MasVnrArea <- ifelse(cleaned_combined_set$MasVnrArea > 1500, 
                                          mean(cleaned_combined_set$MasVnrArea), 
                                          cleaned_combined_set$MasVnrArea)
#Skewness
ggplot(cleaned_combined_set, aes(MasVnrArea)) + geom_histogram()
skewness(cleaned_combined_set$MasVnrArea)
#The value is 2.509, highly skewed.
#Apply a logarithm transformation 
#Because this variable contain many 0 values,
#I use value + 1 to make sure x is greater than zero.
skewness(log(cleaned_combined_set$MasVnrArea +1, 10))
ggplot(cleaned_combined_set, aes(log(MasVnrArea +1, 10)))+ geom_histogram()
#skewness has been greatly improved.
cleaned_combined_set$MasVnrArea <- log(cleaned_combined_set$MasVnrArea +1, 10)

##COLUMN 28: ExterQual
sum(is.na(cleaned_combined_set$ExterQual))
summary(factor(cleaned_combined_set$ExterQual))
#0 missing value or typo
cleaned_combined_set$ExterQual <- factor(cleaned_combined_set$ExterQual, order = TRUE, 
                                         levels = c("Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 29: ExterCond
sum(is.na(cleaned_combined_set$ExterCond))
summary(factor(cleaned_combined_set$ExterCond))
#0 missing value
cleaned_combined_set$ExterCond <- as.factor(cleaned_combined_set$ExterCond)
#2538 observations belong to "TA", which won't provide much insightful information
#I will remove this variable
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(ExterCond))

##COLUMN 30: Foundation
sum(is.na(cleaned_combined_set$Foundation))
summary(factor(cleaned_combined_set$Foundation))
#0 missing value
cleaned_combined_set$Foundation <- as.factor(cleaned_combined_set$Foundation)

##COLUMN 31: BsmtQual
sum(is.na(cleaned_combined_set$BsmtQual))
summary(factor(cleaned_combined_set$BsmtQual))
#81 NAs but these NAs do not mean missing values. 
#They actually represent "No Basement" according to data description.
#Replace NA with "No Basement" to avoid confusion.
cleaned_combined_set$BsmtQual[is.na(cleaned_combined_set$BsmtQual)] <- "No Basement"
cleaned_combined_set$BsmtQual <- factor(cleaned_combined_set$BsmtQual, order = TRUE, 
                                        levels = c("No Basement","Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 32: BsmtCond
sum(is.na(cleaned_combined_set$BsmtCond))
summary(factor(cleaned_combined_set$BsmtCond))
#82 NAs, similar to COLUMN 31, these NAs do not mean missing values. 
#They actually represent "No Basement" according to data description file.
#Replace NA with "No Basement" to avoid confusion.
cleaned_combined_set$BsmtCond[is.na(cleaned_combined_set$BsmtCond)] <- "No Basement"
cleaned_combined_set$BsmtCond <- as.factor(cleaned_combined_set$BsmtCond)

#Check whether BsmtCond and BsmtQual are the same thing or maybe have any correlations
ggplot(cleaned_combined_set, aes(BsmtCond, SalePrice)) + geom_point()
ggplot(cleaned_combined_set, aes(BsmtQual, SalePrice)) + geom_point()
#The plots are identical.
#Let's see the frequency of each level.
summary(factor(cleaned_combined_set$BsmtCond))
summary(factor(cleaned_combined_set$BsmtQual))
#They are the same, therefore, I will remove one of the two variables
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(BsmtCond))

##COLUMN 33: BsmtExposure
sum(is.na(cleaned_combined_set$BsmtExposure))
summary(factor(cleaned_combined_set$BsmtExposure))
#82 NAs, they in fact represent "No Basement" rather than missing values.
#replace NA with "No Basement" to avoid confusion.
cleaned_combined_set$BsmtExposure[is.na(cleaned_combined_set$BsmtExposure)] <- "No Basement"
cleaned_combined_set$BsmtExposure <- factor(cleaned_combined_set$BsmtExposure, order = TRUE, 
                                            levels = c("No Basement","No", "Mn", "Av", "Gd"))

##COLUMN 34:BsmtFinType1
sum(is.na(cleaned_combined_set$BsmtFinType1))
summary(factor(cleaned_combined_set$BsmtFinType1))
#79 NAs but they actually represent "No Basement" according to data description.
#To avoid confusion, replace NA with "No Basement".
cleaned_combined_set$BsmtFinType1[is.na(cleaned_combined_set$BsmtFinType1)] <- "No Basement"
cleaned_combined_set$BsmtFinType1 <- factor(cleaned_combined_set$BsmtFinType1, order = TRUE, 
                                            levels = c("No Basement","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))

##COLUMN 35:BsmtFinSF1
cleaned_combined_set$BsmtFinSF1 <- as.numeric(cleaned_combined_set$BsmtFinSF1)
sum(is.na(cleaned_combined_set$BsmtFinSF1))
#There is 1 missing value
#BsmtFinSF1 and BsmtFinType1 are related according to the description.
cleaned_combined_set %>%
  select(BsmtFinSF1, BsmtFinType1) %>%
  filter(is.na(BsmtFinSF1))
#Missing value of BsmtFinSF1 corresponds to BsmtFinType1 = No Basement
#So we need to replace the missing value with 0.
cleaned_combined_set$BsmtFinSF1[is.na(cleaned_combined_set$BsmtFinSF1)] <- 0
#Outliers
boxplot(cleaned_combined_set$BsmtFinSF1)
ggplot(data = cleaned_combined_set, aes(SalePrice , BsmtFinSF1)) + geom_point()
#Outliers seem to be around 4000
#Replace outliers with mean.
cleaned_combined_set$BsmtFinSF1 <- ifelse(cleaned_combined_set$BsmtFinSF1 > 4000, 
                                          mean(cleaned_combined_set$BsmtFinSF1),
                                          cleaned_combined_set$BsmtFinSF1)
#Check skewness 
ggplot(data = cleaned_combined_set, aes(BsmtFinSF1)) + geom_histogram()
skewness(cleaned_combined_set$BsmtFinSF1)
#The value is 0.850, moderately skewed.
#No transformation

##COLUMN 36: BsmtFinType2
sum(is.na(cleaned_combined_set$BsmtFinType2))
summary(factor(cleaned_combined_set$BsmtFinType2))
#80 NAs but they actually represent "No Basement" according to data description.
#Replace NAs with "No Basement" to avoid confusion.
cleaned_combined_set$BsmtFinType2[is.na(cleaned_combined_set$BsmtFinType2)] <- "No Basement"
cleaned_combined_set$BsmtFinType2 <- factor(cleaned_combined_set$BsmtFinType2, order = TRUE, 
                                            levels = c("No Basement","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))
#However, since 2493 observations belong to "Unf", it won't provide
#much insightful information
#I will remove this variable
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(BsmtFinType2))

##COLUMN 37: BsmtFinSF2
cleaned_combined_set$BsmtFinSF2 <- as.numeric(cleaned_combined_set$BsmtFinSF2)
sum(is.na(cleaned_combined_set$BsmtFinSF2))
#There is 1 missing value
#Missing value of BsmtFinSF2 corresponds to BsmtFinType2 = No Basement
#So we need to replace the missing value with 0.
cleaned_combined_set$BsmtFinSF2[is.na(cleaned_combined_set$BsmtFinSF2)] <- 0
#Outliers
boxplot(cleaned_combined_set$BsmtFinSF2)
ggplot(data = cleaned_combined_set, aes(SalePrice, BsmtFinSF2)) + geom_point()
#Outliers seem to be around 1250.
#Replace outliers with mean.
cleaned_combined_set$BsmtFinSF2 <- ifelse(cleaned_combined_set$BsmtFinSF2 > 1250, 
                                          mean(cleaned_combined_set$BsmtFinSF2),
                                          cleaned_combined_set$BsmtFinSF2)
#Check skewness 
ggplot(data = cleaned_combined_set, aes(BsmtFinSF2)) + geom_histogram()
skewness(cleaned_combined_set$BsmtFinSF2)
#The value is 3.987, highly skewed.
summary(factor(cleaned_combined_set$BsmtFinSF2))
#However, since this variable has 2572 cases of 0 value out of 2919 obs, it won't provide
#much new information in the modeling stage, and no transformation
#is needed. Rather, BsmtFinSF2 should be considered for removal.
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(BsmtFinSF2))

##COLUMN 38: BsmtUnfSF
cleaned_combined_set$BsmtUnfSF <- as.numeric(cleaned_combined_set$BsmtUnfSF)
sum(is.na(cleaned_combined_set$BsmtUnfSF))
#There is 1 missing value
#Since BsmtUnfSF is related to BsmtFinType1 and BsmtFinType2,
#we need to check
cleaned_combined_set %>%
  select(BsmtUnfSF, BsmtFinType1) %>%
  filter(is.na(BsmtUnfSF))
#Missing value of BsmtUnfSF corresponds to No Basement
#So we need to replace the missing value with 0.
cleaned_combined_set$BsmtUnfSF[is.na(cleaned_combined_set$BsmtUnfSF)] <- 0
#Outliers
boxplot(cleaned_combined_set$BsmtUnfSF)
ggplot(data = cleaned_combined_set, aes(SalePrice, BsmtUnfSF)) + geom_point()
#Outliers seem to be around 2000
#Replace outliers with mean.
cleaned_combined_set$BsmtUnfSF <- ifelse(cleaned_combined_set$BsmtUnfSF > 2000, 
                                         mean(cleaned_combined_set$BsmtUnfSF),
                                         cleaned_combined_set$BsmtUnfSF)
#Check skewness 
ggplot(data = cleaned_combined_set, aes(BsmtUnfSF)) + geom_histogram()
skewness(cleaned_combined_set$BsmtUnfSF)
#The value is 0.869, moderately skewed.
#No transformation

##COLUMN 39: TotalBsmtSF
cleaned_combined_set$TotalBsmtSF <- as.numeric(cleaned_combined_set$TotalBsmtSF)
sum(is.na(cleaned_combined_set$TotalBsmtSF))
#There is 1 missing value
#Since TotalBsmtSF is related to BsmtFinType1 and BsmtFinType2,
#we need to check
cleaned_combined_set %>%
  select(TotalBsmtSF, BsmtFinType1) %>%
  filter(is.na(TotalBsmtSF))
#Missing value of TotalBsmtSF corresponds to No Basement
#So we need to replace the missing value with 0.
cleaned_combined_set$TotalBsmtSF[is.na(cleaned_combined_set$TotalBsmtSF)] <- 0
#Outliers
boxplot(cleaned_combined_set$TotalBsmtSF)
ggplot(data = cleaned_combined_set, aes(SalePrice, TotalBsmtSF)) + geom_point()
#Outliers seem to be above 4000
#Replace outliers with mean.
cleaned_combined_set$TotalBsmtSF <- ifelse(cleaned_combined_set$TotalBsmtSF > 4000,
                                           mean(cleaned_combined_set$TotalBsmtSF),
                                           cleaned_combined_set$TotalBsmtSF)
#Skewness 
ggplot(data = cleaned_combined_set, aes(x = TotalBsmtSF)) + geom_histogram()
skewness(cleaned_combined_set$TotalBsmtSF)
#The value is 0.443, moderately skewed.
#No transformation

##COLUMN 40: Heating
sum(is.na(cleaned_combined_set$Heating))
summary(factor(cleaned_combined_set$Heating))
#No missing value or typo
#2874 out of 2919 cases belong to the GasA category.
#Heating should be removed because it won't
#provide much useful information in the modeling stage.
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(Heating))

##COLUMN 41: HeatingQC
sum(is.na(cleaned_combined_set$HeatingQC))
summary(factor(cleaned_combined_set$HeatingQC))
#No missing value or typo
cleaned_combined_set$HeatingQC <- factor(cleaned_combined_set$HeatingQC, order = TRUE, 
                                         levels = c("Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 42: CentralAir
sum(is.na(cleaned_combined_set$CentralAir))
#No missing value
cleaned_combined_set$CentralAir <- as.factor(cleaned_combined_set$CentralAir)
summary(cleaned_combined_set$CentralAir)
#2723 out of 2919 cases belong to the Y category.
#CentralAir should be removed because it won't
#provide much useful information in the modeling stage.
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(CentralAir))

##COLUMN 43: Electrical
sum(is.na(cleaned_combined_set$Electrical))
summary(factor(cleaned_combined_set$Electrical))
#1 missing value
#Replace the NA with SBrkr which appears the most.
cleaned_combined_set$Electrical[is.na(cleaned_combined_set$Electrical)] <- "SBrkr"
#Since 2672 observations belong to Electrical, it won't provide much insightful information
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(Electrical))

##COLUMN 44: X1stFlrSF
cleaned_combined_set$X1stFlrSF <- as.numeric(cleaned_combined_set$X1stFlrSF)
sum(is.na(cleaned_combined_set$X1stFlrSF))
#No missing value
#Outliers
boxplot(cleaned_combined_set$X1stFlrSF)
ggplot(data = cleaned_combined_set, aes(SalePrice, X1stFlrSF)) + geom_point()
#Outliers seem to be around 3000
#Replace outliers with mean.
cleaned_combined_set$X1stFlrSF <- ifelse(cleaned_combined_set$X1stFlrSF > 3000,
                                         mean(cleaned_combined_set$X1stFlrSF),
                                         cleaned_combined_set$X1stFlrSF)
#Skewness 
ggplot(data = cleaned_combined_set, aes(x = X1stFlrSF)) + geom_histogram()
skewness(cleaned_combined_set$X1stFlrSF)
#The value is 0.821, moderately skewed.
#No transformation

##COLUMN 45: X2ndFlrSF
cleaned_combined_set$X2ndFlrSF <- as.numeric(cleaned_combined_set$X2ndFlrSF)
sum(is.na(cleaned_combined_set$X2ndFlrSF))
#No missing value
#Outliers
boxplot(cleaned_combined_set$X2ndFlrSF)
ggplot(data = cleaned_combined_set, aes(SalePrice, X2ndFlrSF)) + geom_point()
#Outliers seem to be around 2000
#Replace outliers with mean.
cleaned_combined_set$X2ndFlrSF <- ifelse(cleaned_combined_set$X2ndFlrSF > 2000,
                                         mean(cleaned_combined_set$X2ndFlrSF),
                                         cleaned_combined_set$X2ndFlrSF)
#Skewness 
ggplot(data = cleaned_combined_set, aes(x = X2ndFlrSF)) + geom_histogram()
skewness(cleaned_combined_set$X2ndFlrSF)
#The value is 0.84999, moderately skewed.
#No transformation

##COLUMN 46: LowQualFinSF
cleaned_combined_set$LowQualFinSF <- as.numeric(cleaned_combined_set$LowQualFinSF)
sum(is.na(cleaned_combined_set$LowQualFinSF))
#No missing value
#Outliers
boxplot(cleaned_combined_set$LowQualFinSF)
ggplot(data = cleaned_combined_set, aes(SalePrice, LowQualFinSF)) + geom_point()
#This variable may contain too many cases with the same value, 
#which won't provide much useful information during data modeling.
summary(factor(cleaned_combined_set$LowQualFinSF))
#2879 cases are 0 value.
#Skewness 
ggplot(data = cleaned_combined_set, aes(x = LowQualFinSF)) + geom_histogram()
skewness(cleaned_combined_set$LowQualFinSF)
#The value is 12.082, highly skewed.
#I will remove this variable so I won't do any transformations
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(LowQualFinSF))

##COLUMN 47: GrLivArea
cleaned_combined_set$GrLivArea <- as.numeric(cleaned_combined_set$GrLivArea)
sum(is.na(cleaned_combined_set$GrLivArea))
#No missing value
#Outliers
boxplot(cleaned_combined_set$GrLivArea)
ggplot(data = cleaned_combined_set, aes(SalePrice, GrLivArea)) + geom_point()
#Outliers seem to be around 4000
#Replace outliers with mean.
cleaned_combined_set$GrLivArea <- ifelse(cleaned_combined_set$GrLivArea > 4000,
                                         mean(cleaned_combined_set$GrLivArea),
                                         cleaned_combined_set$GrLivArea)
#Skewness 
ggplot(data = cleaned_combined_set, aes(x = GrLivArea)) + geom_histogram()
skewness(cleaned_combined_set$GrLivArea)
#The value is 0.874, moderately skewed.
#No transformation

##COLUMN 48: BsmtFullBath
cleaned_combined_set$BsmtFullBath <- as.numeric(cleaned_combined_set$BsmtFullBath)
sum(is.na(cleaned_combined_set$BsmtFullBath))
summary(factor(cleaned_combined_set$BsmtFullBath))
#There are 2 missing values
#We can check BsmtFullBath according to BsmtQual
cleaned_combined_set %>%
  select(BsmtFullBath, BsmtQual) %>%
  filter(is.na(BsmtFullBath))
#NA actually means No Basement, therefore we assign 0 to avoid confusions
cleaned_combined_set$BsmtFullBath[is.na(cleaned_combined_set$BsmtFullBath)] <- 0
ggplot(data = cleaned_combined_set, aes(x = BsmtFullBath)) + geom_histogram()
skewness(cleaned_combined_set$BsmtFullBath)
#The value is 0.6245, moderately skewed.
#No transformation

##COLUMN 49: BsmtHalfBath
cleaned_combined_set$BsmtHalfBath <- as.numeric(cleaned_combined_set$BsmtHalfBath)
sum(is.na(cleaned_combined_set$BsmtHalfBath))
#There are 2 missing values
cleaned_combined_set %>%
  select(BsmtHalfBath, BsmtQual) %>%
  filter(is.na(BsmtHalfBath))
#Again, NA actually represents No Basement, so we assign 0 to missing values
#to avoid confusion
cleaned_combined_set$BsmtHalfBath[is.na(cleaned_combined_set$BsmtHalfBath)] <- 0
#Outliers
boxplot(cleaned_combined_set$BsmtHalfBath)
ggplot(data = cleaned_combined_set, aes(SalePrice, BsmtHalfBath)) + geom_point()
#Check skewness
skewness(cleaned_combined_set$BsmtHalfBath)
#The value is 3.9296, highly skewed.
#I am not going to try any tranformation because
#later I will combine BsmtHalfBath with HalfBath to create a new variable.

##COLUMN 50: FullBath
cleaned_combined_set$FullBath <- as.numeric(cleaned_combined_set$FullBath)
sum(is.na(cleaned_combined_set$FullBath))
summary(factor(cleaned_combined_set$FullBath))
#No missing value
#Outliers
ggplot(data = cleaned_combined_set, aes(SalePrice, FullBath)) + geom_point()
skewness(cleaned_combined_set$FullBath)
#The value is 0.1675, approximately symmetrical.

##COLUMN 51: HalfBath
cleaned_combined_set$HalfBath <- as.numeric(cleaned_combined_set$HalfBath)
sum(is.na(cleaned_combined_set$HalfBath))
summary(factor(cleaned_combined_set$HalfBath))
#No missing value
#Outliers
ggplot(data = cleaned_combined_set, aes(SalePrice, HalfBath)) + geom_point()
skewness(cleaned_combined_set$HalfBath)
#The value is 0.694, moderately skewed.

##COLUMN 52: BedroomAbvGr
cleaned_combined_set$BedroomAbvGr <- as.numeric(cleaned_combined_set$BedroomAbvGr)
sum(is.na(cleaned_combined_set$BedroomAbvGr))
summary(factor(cleaned_combined_set$BedroomAbvGr))
#No missing value
#Outliers
boxplot(cleaned_combined_set$BedroomAbvGr)
ggplot(data = cleaned_combined_set, aes(SalePrice, BedroomAbvGr)) + geom_point()
#Only 1 observation belongs to "8", therefore, I will merge it with the nearby
#level, which is "6"
cleaned_combined_set$BedroomAbvGr[cleaned_combined_set$BedroomAbvGr == "8"] <- 6
summary(factor(cleaned_combined_set$BedroomAbvGr))
#Check skewness
skewness(cleaned_combined_set$BedroomAbvGr)
#The value is 0.267, approximately symmetrical.

##COLUMN 53: KitchenAbvGr
cleaned_combined_set$KitchenAbvGr <- as.numeric(cleaned_combined_set$KitchenAbvGr)
sum(is.na(cleaned_combined_set$KitchenAbvGr))
summary(factor(cleaned_combined_set$KitchenAbvGr))
#No missing value
#2785 out of 2919 cases share the same value (i.e., 1), 
#which won't provide much useful insights in the modeling stage.
#KitchenAvbGr should be removed.
skewness(cleaned_combined_set$KitchenAbvGr)
#The value is 4.3, highly skewed.
#KitchenAbvGr should be removed so I won't try any transformation.
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(KitchenAbvGr))

##COLUMN 54: KitchenQual
sum(is.na(cleaned_combined_set$KitchenQual))
summary(factor(cleaned_combined_set$KitchenQual))
#There is 1 missing value
#I replace the missing values by the most frequent value 
#TA is the most frequent value for KitchenQual, so I assign it to the missing value
cleaned_combined_set$KitchenQual[is.na(cleaned_combined_set$KitchenQual)] <- "TA"
cleaned_combined_set$KitchenQual <- factor(cleaned_combined_set$KitchenQual, order = TRUE, 
                                           levels = c("Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 55: TotRmsAbvGrd
cleaned_combined_set$TotRmsAbvGrd <- as.numeric(cleaned_combined_set$TotRmsAbvGrd)
sum(is.na(cleaned_combined_set$TotRmsAbvGrd))
summary(factor(cleaned_combined_set$TotRmsAbvGrd))
#No missing value
ggplot(data = cleaned_combined_set, aes(SalePrice, TotRmsAbvGrd)) + geom_point()
#I will merge levels that have only one observations with the nearby levels
cleaned_combined_set$TotRmsAbvGrd[cleaned_combined_set$TotRmsAbvGrd == "2"] <- 3
cleaned_combined_set$TotRmsAbvGrd[cleaned_combined_set$TotRmsAbvGrd == "13"] <- 12
cleaned_combined_set$TotRmsAbvGrd[cleaned_combined_set$TotRmsAbvGrd == "14"] <- 12
cleaned_combined_set$TotRmsAbvGrd[cleaned_combined_set$TotRmsAbvGrd == "15"] <- 12
summary(factor(cleaned_combined_set$TotRmsAbvGrd))
#Check skewness
skewness(cleaned_combined_set$TotRmsAbvGrd)
#The value is 0.705, moderately skewed.
#No transformation

##COLUMN 56: Functional
sum(is.na(cleaned_combined_set$Functional))
summary(factor(cleaned_combined_set$Functional))
#There are 2 missing values
#I replace missing values with the most frequent values, which is Typ
cleaned_combined_set$Functional[is.na(cleaned_combined_set$Functional)] <- "Typ"
summary(factor(cleaned_combined_set$Functional))
cleaned_combined_set$Functional <- factor(cleaned_combined_set$Functional, order = TRUE, 
                                          levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod","Min2","Min1","Typ"))
#However, there are 2719 out of 2919 observations belonging to Typ, which means
#it won't provide much insightful information
#This variable should be considered for removal
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(Functional))

##COLUMN 57: Fireplaces
cleaned_combined_set$Fireplaces <- as.numeric(cleaned_combined_set$Fireplaces)
sum(is.na(cleaned_combined_set$Fireplaces))
summary(factor(cleaned_combined_set$Fireplaces))
#No missing value
#Outliers
ggplot(data = cleaned_combined_set, aes(SalePrice, Fireplaces)) + geom_point()
skewness(cleaned_combined_set$Fireplaces)
#The value is 0.733, moderately skewed.

##COLUMN 58: FireplaceQu
sum(is.na(cleaned_combined_set$FireplaceQu))
summary(factor(cleaned_combined_set$FireplaceQu))
#1420 NAs but they mean "No fireplace" according to the description
#Replace NAs with "No fireplace" to avoid confusion.
cleaned_combined_set$FireplaceQu[is.na(cleaned_combined_set$FireplaceQu)] <- "No fireplace"
cleaned_combined_set$FireplaceQu <- factor(cleaned_combined_set$FireplaceQu, order = TRUE, 
                                           levels = c("No fireplace", "Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 59: GarageType
sum(is.na(cleaned_combined_set$GarageType))
summary(factor(cleaned_combined_set$GarageType))
#157 NAs but they mean "No Garage" according to the description
#Replace NAs with "No Garage" to avoid confusion.
cleaned_combined_set$GarageType[is.na(cleaned_combined_set$GarageType)] <- "No Garage"
cleaned_combined_set$GarageType <- as.factor(cleaned_combined_set$GarageType)

##COLUMN 60: GarageYrBlt
sum(is.na(cleaned_combined_set$GarageYrBlt))
#159 NAs
#Replace NAs with means and round them to be whole numbers.
#However, GarageYrBlt might be closely related to YearBuilt (House Year Built)
cleaned_combined_set$YearBuilt <- as.numeric(cleaned_combined_set$YearBuilt)
cor.test(cleaned_combined_set$YearBuilt, cleaned_combined_set$GarageYrBlt)
cleaned_combined_set$YearBuilt <- as.factor(cleaned_combined_set$YearBuilt)
#If we compute the correlation between GarageYrBlt and YearBuilt,
#the value is 0.8431, high correlation which will cause collinearity issue
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(GarageYrBlt))
#Now I remove YearBuilt as we already had HomeAge
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(YearBuilt))

##COLUMN 61: GarageFinish
sum(is.na(cleaned_combined_set$GarageFinish))
summary(factor(cleaned_combined_set$GarageFinish))
#159 NAs but they mean "No Garage" not missing values.
#Replace NA with "No Garage".
cleaned_combined_set$GarageFinish[is.na(cleaned_combined_set$GarageFinish)] <- "No Garage"
cleaned_combined_set$GarageFinish <- factor(cleaned_combined_set$GarageFinish, order = TRUE, 
                                            levels = c("No Garage", "Unf", "RFn", "Fin"))

##COLUMN 62: GarageCars
sum(is.na(cleaned_combined_set$GarageCars))
#There is 1 missing value
#Since GarageCars and GarageFinish are related to each other, we need to check
cleaned_combined_set %>%
  select(GarageCars, GarageFinish) %>%
  filter(is.na(GarageCars))
#So NA refers to No Garage
#I assign 0 to NA to avoid confusion
cleaned_combined_set$GarageCars[is.na(cleaned_combined_set$GarageCars)] <- 0
cleaned_combined_set$GarageCars <- as.numeric(cleaned_combined_set$GarageCars)
#Outliers
ggplot(data = cleaned_combined_set, aes(SalePrice, GarageCars)) + geom_point()
#No obvious outliers
skewness(cleaned_combined_set$GarageCars)
#The value is -0.219, approximately symmetrical.

##COLUMN 63: GarageArea
sum(is.na(cleaned_combined_set$GarageArea))
#There is 1 missing value
#Since GarageArea and GarageFinish are related to each other, we need to check
cleaned_combined_set %>%
  select(GarageArea, GarageFinish) %>%
  filter(is.na(GarageArea))
#So NA refers to No Garage
#I assign 0 to NA to avoid confusion
cleaned_combined_set$GarageArea[is.na(cleaned_combined_set$GarageArea)] <- 0
cleaned_combined_set$GarageArea <- as.numeric(cleaned_combined_set$GarageArea)
#Outliers
boxplot(cleaned_combined_set$GarageArea)
ggplot(data = cleaned_combined_set, aes(SalePrice, GarageArea)) + geom_point()
#Outliers seem to be around 1250.
#Replace outliers with mean.
cleaned_combined_set$GarageArea <- ifelse(cleaned_combined_set$GarageArea >= 1250,
                                          mean(cleaned_combined_set$GarageArea),
                                          cleaned_combined_set$GarageArea)
skewness(cleaned_combined_set$GarageArea)
#The value is 0.112, approximately symmetrical.

##COLUMN 64: GarageQual
sum(is.na(cleaned_combined_set$GarageQual))
summary(factor(cleaned_combined_set$GarageQual))
#159 NAs but they mean "No Garage" not missing values according to the description
#Replace NA with "No Garage".
cleaned_combined_set$GarageQual[is.na(cleaned_combined_set$GarageQual)] <- "No Garage"
cleaned_combined_set$GarageQual <- factor(cleaned_combined_set$GarageQual, order = TRUE, 
                                          levels = c("No Garage", "Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 65: GarageCond
sum(is.na(cleaned_combined_set$GarageCond))
summary(factor(cleaned_combined_set$GarageCond))
#159 NAs but they mean "No Garage" not missing values.
#Replace NA with "No Garage".
cleaned_combined_set$GarageCond[is.na(cleaned_combined_set$GarageCond)] <- "No Garage"
cleaned_combined_set$GarageCond <- factor(cleaned_combined_set$GarageCond, order = TRUE, 
                                          levels = c("No Garage", "Po", "Fa", "TA", "Gd", "Ex"))
####
#However, we need to check if GarageCond and GarageQual the same thing or at least closely related
ggplot(cleaned_combined_set, aes(GarageCond, SalePrice)) + geom_point()
ggplot(cleaned_combined_set, aes(GarageQual, SalePrice)) + geom_point()
#The plots are approximately identical.
#Let's see the frequency of each level.
summary(cleaned_combined_set$GarageCond)
summary(cleaned_combined_set$GarageQual)
#Frequencies between two variables for the same level are similar. 
#Consider removing one the two variables.
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(GarageCond))
####
#After careful consideration, I will also remove GarageQual since 2604 out of 2919
#belong to "TA", which won't provide much insightful information
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(GarageQual))

##COLUMN 66: PavedDrive
sum(is.na(cleaned_combined_set$PavedDrive))
summary(factor(cleaned_combined_set$PavedDrive))
#No missing valua or typos.
cleaned_combined_set$PavedDrive <- as.factor(cleaned_combined_set$PavedDrive)
#However, 2641 observations belong to Y, which won't provide much insightful information
#I will remove this variable
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(PavedDrive))

##COLUMN 67: WoodDeckSF
cleaned_combined_set$WoodDeckSF <- as.numeric(cleaned_combined_set$WoodDeckSF)
sum(is.na(cleaned_combined_set$WoodDeckSF))
#No missing value.
#Outliers
boxplot(cleaned_combined_set$WoodDeckSF)
ggplot(data = cleaned_combined_set, aes(SalePrice, WoodDeckSF)) + geom_point()
#Outliers seem to be above 800
#Replace outliers with mean.
cleaned_combined_set$WoodDeckSF <- ifelse(cleaned_combined_set$WoodDeckSF > 800,
                                          mean(cleaned_combined_set$WoodDeckSF),
                                          cleaned_combined_set$WoodDeckSF)
#Skewness
ggplot(data = cleaned_combined_set, aes(WoodDeckSF)) + geom_histogram()
skewness(cleaned_combined_set$WoodDeckSF)
#The value is 1.44594, highly skewed.
#Try log transformation.
ggplot(data = cleaned_combined_set, aes(log10(WoodDeckSF+1))) + geom_histogram()
skewness(log10(cleaned_combined_set$WoodDeckSF +1))
#Skewness has been greatly improved.
cleaned_combined_set$WoodDeckSF <- log10(cleaned_combined_set$WoodDeckSF +1)

##COLUMN 68: OpenPorchSF
cleaned_combined_set$OpenPorchSF <- as.numeric(cleaned_combined_set$OpenPorchSF)
sum(is.na(cleaned_combined_set$OpenPorchSF))
#No missing value.
#Outliers
boxplot(cleaned_combined_set$OpenPorchSF)
ggplot(data = cleaned_combined_set, aes(SalePrice, OpenPorchSF)) + geom_point()
#Outliers seem to be around 400.
#Replace outliers with mean.
cleaned_combined_set$OpenPorchSF <- ifelse(cleaned_combined_set$OpenPorchSF > 400,
                                           mean(cleaned_combined_set$OpenPorchSF),
                                           cleaned_combined_set$OpenPorchSF)
#Skewness
ggplot(data = cleaned_combined_set, aes(OpenPorchSF)) + geom_histogram()
skewness(cleaned_combined_set$OpenPorchSF)
#The value is 1.834, highly skewed.
#Try log transformation.
ggplot(data = cleaned_combined_set, aes(log10(OpenPorchSF+1))) + geom_histogram()
skewness(log10(cleaned_combined_set$OpenPorchSF +1))
#Skewness has been greatly improved.
cleaned_combined_set$OpenPorchSF <- log10(cleaned_combined_set$OpenPorchSF +1)

##COLUMN 69: EnclosedPorch
cleaned_combined_set$EnclosedPorch <- as.numeric(cleaned_combined_set$EnclosedPorch)
sum(is.na(cleaned_combined_set$EnclosedPorch))
#No missing value 
#Outliers
boxplot(cleaned_combined_set$EnclosedPorch)
ggplot(data = cleaned_combined_set, aes(SalePrice, EnclosedPorch)) + geom_point()
#Outliers seem to be above 400.
#Replace outliers with mean.
cleaned_combined_set$EnclosedPorch <- ifelse(cleaned_combined_set$EnclosedPorch > 400,
                                             mean(cleaned_combined_set$EnclosedPorch),
                                             cleaned_combined_set$EnclosedPorch)
#Skewness
ggplot(data = cleaned_combined_set, aes(EnclosedPorch)) + geom_histogram()
skewness(cleaned_combined_set$EnclosedPorch)
#The value is 2.914, highly skewed.
summary(factor(cleaned_combined_set$EnclosedPorch))
#However, I noticed that 2460 observations belong to 0 value, I will remove this variable
#as it won't provide much insightful information
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(EnclosedPorch))


##COLUMN 70: X3SsnPorch
cleaned_combined_set$X3SsnPorch <- as.numeric(cleaned_combined_set$X3SsnPorch)
sum(is.na(cleaned_combined_set$X3SsnPorch))
#Outliers
boxplot(cleaned_combined_set$X3SsnPorch)
ggplot(data = cleaned_combined_set, aes(SalePrice, X3SsnPorch)) + geom_point() 
#Outliers seem to be around 500.
#Replace outliers with mean.
cleaned_combined_set$X3SsnPorch <- ifelse(cleaned_combined_set$X3SsnPorch > 500,
                                          mean(cleaned_combined_set$X3SsnPorch),
                                          cleaned_combined_set$X3SsnPorch)
#Skewness
ggplot(data = cleaned_combined_set, aes(X3SsnPorch)) + geom_histogram()
skewness(cleaned_combined_set$X3SsnPorch)
#The value is 10.77, highly skewed.
summary(factor(cleaned_combined_set$X3SsnPorch))
#However, I noticed that 2882 observations belong to 0 value, I will remove this variable
#as it won't provide much insightful information
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(X3SsnPorch))

##COLUMN 71: ScreenPorch
cleaned_combined_set$ScreenPorch <- as.numeric(cleaned_combined_set$ScreenPorch)
sum(is.na(cleaned_combined_set$ScreenPorch))
#No missing value.
#Outliers
boxplot(cleaned_combined_set$ScreenPorch)
ggplot(data = cleaned_combined_set, aes(SalePrice, ScreenPorch)) + geom_point()
#Outliers seem to be around 400.
#Replace outliers with mean.
cleaned_combined_set$ScreenPorch <- ifelse(cleaned_combined_set$ScreenPorch > 400,
                                           mean(cleaned_combined_set$ScreenPorch),
                                           cleaned_combined_set$ScreenPorch)
#Skewness
ggplot(data = cleaned_combined_set, aes(ScreenPorch)) + geom_histogram()
skewness(cleaned_combined_set$ScreenPorch)
#The value is 3.945, highly skewed.
summary(factor(cleaned_combined_set$ScreenPorch))
#However, I noticed that 2663 observations belong to 0 value, I will remove this variable
#as it won't provide much insightful information
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(ScreenPorch))

##COLUMN 72: PoolArea
sum(is.na(cleaned_combined_set$PoolArea))
#No missing value.
summary(factor(cleaned_combined_set$PoolArea))
#However, 2906 out of 2919 case have the same value (i.e., 0), which
#won't provid much useful information in the modeling stage.
#PoolArea should be removed.
#Skewness
ggplot(data = cleaned_combined_set, aes(PoolArea)) + geom_histogram()
skewness(cleaned_combined_set$PoolArea)
#The value is 16.89, highly skewed.
#No transformation since I will remove it
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(PoolArea))

##COLUMN 73: PoolQC
sum(is.na(cleaned_combined_set$PoolQC))
summary(factor(cleaned_combined_set$PoolQC))
#2909 NAs but they mean "No Pool" according to the description.
#Replace NAs with "No Pool".
cleaned_combined_set$PoolQC[is.na(cleaned_combined_set$PoolQC)] <- "No Pool"
cleaned_combined_set$PoolQC <- factor(cleaned_combined_set$PoolQC, order = TRUE, 
                                      levels = c("No Pool", "Fa", "TA", "Gd", "Ex"))
#Similar to PoolArea, PoolQC should be removed since there are 2909 cases
#out of 2919 cases having the same value (i.e. No Pool) and it won't provide
#much insightful information
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(PoolQC))

##COLUMN 74: Fence
sum(is.na(cleaned_combined_set$Fence))
summary(factor(cleaned_combined_set$Fence))
#2348 NAs but they mean "No Fence" according to the description.
#Replace NAs with "No Fence"
cleaned_combined_set$Fence[is.na(cleaned_combined_set$Fence)] <- "No Fence"
cleaned_combined_set$Fence <- factor(cleaned_combined_set$Fence, order = TRUE, 
                                     levels = c("No Fence", "MnWw", "GdWo", "MnPrv", "GdPrv"))
#Since 2348 observations belong to "No Fence", it won't provide much insightful information
#I will remove this variable
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(Fence))

##COLUMN 75: MiscFeature
sum(is.na(cleaned_combined_set$MiscFeature))
summary(factor(cleaned_combined_set$MiscFeature))
#2814 NAs but they mean "None" according to the description.
#Replace NAs with "None" to avoid confusion.
cleaned_combined_set$MiscFeature[is.na(cleaned_combined_set$MiscFeature)] <- "None"
cleaned_combined_set$MiscFeature <- as.factor(cleaned_combined_set$MiscFeature)
summary(factor(cleaned_combined_set$MiscFeature))
#MiscFeature should be removed because 2814 out of 2919 cases share the same value,
#which won't generate much useful information in data modeling.
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(MiscFeature))

##COLUMN 76: MiscVal
sum(is.na(cleaned_combined_set$MiscVal))
#No missing value
#Outliers
boxplot(cleaned_combined_set$MiscVal)
ggplot(data = cleaned_combined_set, aes(SalePrice, MiscVal)) + geom_point()
#Outliers seem to be above 5000
#Replace outliers with mean.
cleaned_combined_set$MiscVal <- ifelse(cleaned_combined_set$MiscVal > 5000,
                                       mean(cleaned_combined_set$MiscVal),
                                       cleaned_combined_set$MiscVal)
#Skewness
ggplot(data = cleaned_combined_set, aes(MiscVal)) + geom_histogram()
skewness(cleaned_combined_set$MiscVal)
#The value is 11.74909, highly skewed.
summary(factor(cleaned_combined_set$MiscVal))
#MiscVal has 2816 out of 2919 cases with the same value (i.e., 0), 
#which won't provide much useful information during the modeling stage.
#This variable should be considered for removal
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(MiscVal))

##COLUMN 77: MoSold
sum(is.na(cleaned_combined_set$MoSold))
#No missing value 
#Similar to YearBuilt, it would be more reasonable to explain the coefficient of MoSold
#when MoSold is a categorical variable.
cleaned_combined_set$MoSold <- as.factor(cleaned_combined_set$MoSold)

##COLUMN 78: YrSold
sum(is.na(cleaned_combined_set$YrSold))
#No missing value 
#Similar to YearBuilt, it would be more reasonable to explain the coefficient of YrSold
#when YrSold is a categorical variable.
cleaned_combined_set$YrSold <- as.factor(cleaned_combined_set$YrSold)

##COLUMN 79: SaleType
sum(is.na(cleaned_combined_set$SaleType))
summary(factor(cleaned_combined_set$SaleType))
#There is 1 missing value
#Assign the most frequent value to the missing value, which is "WD"
cleaned_combined_set$SaleType[is.na(cleaned_combined_set$SaleType)] <- "WD"
cleaned_combined_set$SaleType <- as.factor(cleaned_combined_set$SaleType)
#However, since 2525 observations belong to "WD", it won't provide much insightful information
#I will remove this variable
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(SaleType))

##COLUMN 80: SaleCondition
sum(is.na(cleaned_combined_set$SaleCondition))
summary(factor(cleaned_combined_set$SaleCondition))
#No missing value or typos.
#However, 2402 observations belong to "Normal", which won't provide much insightful 
#information, I will remove this variable
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(SaleCondition))

##COLUMN 81: SalePrice
sum(is.na(cleaned_combined_set$SalePrice))
#1459 missing values come from the test set
#Assign 0 to missing values
cleaned_combined_set$SalePrice[is.na(cleaned_combined_set$SalePrice)] <- 0
skewness(cleaned_combined_set$SalePrice)
#The value is 1.098, highly skewed.
#Try log transformation.
skewness(log(cleaned_combined_set$SalePrice + 1, 10))
#The value now is 0.0059, which is a good value because now it is symmetrical
cleaned_combined_set$SalePrice <- log(cleaned_combined_set$SalePrice + 1, 10)

##New variables column

#Create a new variable/column called "HomeTotalSF" by combing 
#all the variables regarding the square feet of particular space.
cleaned_combined_set$HomeTotalSF <- cleaned_combined_set$TotalBsmtSF + cleaned_combined_set$X1stFlrSF + cleaned_combined_set$X2ndFlrSF
skewness(cleaned_combined_set$HomeTotalSF)
#0.679, only moderately skewed.
#I will remove TotalBsmtSF, X1stFlrSF, and X2ndFlrSF because they have been combined into a single variable.
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(TotalBsmtSF, X1stFlrSF, X2ndFlrSF))

#Create a new variable/column called "TotalFullBath" by combing 
#all the variables regarding the number of full bathrooms.
cleaned_combined_set$TotalFullBath <- cleaned_combined_set$BsmtFullBath + cleaned_combined_set$FullBath 
skewness(cleaned_combined_set$TotalFullBath)
#The value is 0.448, approximately symmetrical.
#I will remove redundant variables
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(BsmtFullBath, FullBath))

#Create a new variable/column called "TotalHalfBath" by combing 
#all the variables regarding the number of half bathrooms.
cleaned_combined_set$TotalHalfBath <- cleaned_combined_set$BsmtHalfBath + cleaned_combined_set$HalfBath
skewness(cleaned_combined_set$TotalHalfBath)
#The value is 0.7987, moderately skewed.
#I will remove redundant variables
cleaned_combined_set <- subset(cleaned_combined_set, select = -c(BsmtHalfBath, HalfBath))

###PLAIN VANILLA MODEL & PREDICTION

#I merged train and test set for facilitating the data cleaning.
#Now I split them again to to create the final model.
train <- cleaned_combined_set[cleaned_combined_set$isTrain==1,]
test <- cleaned_combined_set[cleaned_combined_set$isTrain==0,]

#Build a plain vanilla model to predict "SalePrice" using all predictors 
#in the training set
#Remove the "id" column in train first
train <- subset(train, select = -c(Id, isTrain))
model_plainvanila <- lm(formula = SalePrice ~., data = train)
summary(model_plainvanila)
#To avoid aliased coefficients in the model, I will use alias function first
alias(model_plainvanila)
#Remove alias error culprits
train <- subset(train, select = -c(BsmtFinType1))
test <- subset(test, select = -c(BsmtFinType1))
train <- subset(train, select = -c(GarageFinish))
test <- subset(test, select = -c(GarageFinish))
#Rerun the model
model_plainvanila <- lm(formula = SalePrice ~., data = train)

#Compute training MSE
MSE_train_plain <- mean((train$SalePrice - 10^model_plainvanila$fitted.values)^2)
MSE_train_plain

#Compute test MSE.
test_pred <- 10^(predict(model_plainvanila, test))
MSE_test_plain <- mean((test$SalePrice - test_pred)^2)
MSE_test_plain

#Generate predicted prices
inital_pred <- data.frame(Id = test$Id, SalePrice = test_pred)
write.csv(inital_pred, file = "pred_plainvanilla.csv", row.names = FALSE)

###ASSESS ISSUES & PERFORM PRESCRIPTIONS IF NEEDED

#Check for multicollinearity
vif(model_plainvanila)
#Since this is a large dataset (various dimensions), we will assess multicollinearity
#by looking at GVIF^(1/(2*Df))
#"MasVnrArea" seems to be the only culprit for multicollinearity with 
#the value 5.135208 (>5)
#I will remove this variable in both training and test datasets
train <- subset(train, select = -c(MasVnrArea))
test <- subset(test, select = -c(MasVnrArea))

#Rerun the model
model_plainvanila_2 <- lm(formula = SalePrice ~., data = train)
summary(model_plainvanila_2)

#Check for non-linearity issue
par(mfrow = c(1,1))
plot(model_plainvanila_2)
#The residual-fitted plot shows a horizontal pattern, which is good and indicates
#no non-linearity issue exists
#Therefore, no transformation is needed

#Check for heterscedasticity issue 
#(i.e., non-constant variance of error terms)
plot(model_plainvanila_2)
#We look at the Scale-Location plot
#Considering the overall pattern, which shows that the points are pretty equally 
#spread, we can conclude that no heterscedasticity issue exists
#Therefore, no transformation is needed

#Check for influential points
cooksD_cutoff <- 0.5
cooksD <- cooks.distance(model_plainvanila_2)
cooksD_dataframe <- data.frame(obs = names(cooksD), cooks = cooksD)
cooksD_dataframe[which(abs(cooksD) > cooksD_cutoff), ]
#Although we got a warning messages with the following points being
#high leverage points: 326, 376, 534, 949, 1012, 1188, 1371, it turns out
#they are not influential points. Therefore, no transformation is needed.

#Generate predicted prices to see if the score is imporved
test_prescriptions <- 10^(predict(model_plainvanila_2, test))
prescribed_pred <- data.frame(Id = test$Id, SalePrice = test_prescriptions)
write.csv(prescribed_pred, file = "pred_prescriptions.csv", row.names = FALSE)

###FEATURE SELECTION USING AIC
#Forward selection using AIC
#Because forward selection begins from the null model, I start from the intercept
#(the null model) and specify the scope
null_model <- lm(SalePrice ~ 1, data = train) 
Forward_AIC <- stepAIC(null_model, direction = "forward", 
                       scope = list(lower = null_model, 
                                    upper = ~ MSSubClass + MSZoning + LotFrontage + LotArea + 
                                      LotShape + LotConfig + Neighborhood + HouseStyle + OverallQual + 
                                      OverallCond + Exterior1st + MasVnrType + ExterQual + Foundation + 
                                      BsmtQual + BsmtExposure + BsmtFinSF1 + BsmtUnfSF + HeatingQC + 
                                      GrLivArea + BedroomAbvGr + KitchenQual + TotRmsAbvGrd + Fireplaces + 
                                      FireplaceQu + GarageType + GarageCars + GarageArea + WoodDeckSF + 
                                      OpenPorchSF + MoSold + YrSold + HomeAge + AgeSinceRemod + 
                                      HomeTotalSF + TotalFullBath + TotalHalfBath, k = 2))

summary(Forward_AIC) 
#Generate predicted prices to see if the score is imporved
Forward_AIC_predict <- 10^(predict(Forward_AIC, test))
Forward_AIC_pred <- data.frame(Id = test$Id, SalePrice = Forward_AIC_predict)
write.csv(Forward_AIC_pred, file = "pred_Forward_AIC.csv", row.names = FALSE)

#Backward selection using AIC
#Since backward selection begins from the full model and removes variables
#iteratively, I do not specify the null model
Backward_AIC <- stepAIC(model_plainvanila_2, direction = "backward", k = 2)
summary(Backward_AIC)
#Generate predicted prices to see if the score is imporved
Backward_AIC_predict <- 10^(predict(Backward_AIC, test))
Backward_AIC_pred <- data.frame(Id = test$Id, SalePrice = Backward_AIC_predict)
write.csv(Backward_AIC_pred, file = "pred_Backward_AIC.csv", row.names = FALSE)

#Hybrid selection using AIC
#Hybrid selection begins from the null model, but I already specified the null
#model above
Hybrid_AIC <- stepAIC(null_model, direction = "both", 
                       scope = list(lower = null_model, 
                                    upper = ~ MSSubClass + MSZoning + LotFrontage + LotArea + LotShape + 
                                      LotConfig + Neighborhood + HouseStyle + OverallQual + OverallCond + 
                                      Exterior1st + MasVnrType + ExterQual + Foundation + BsmtQual + 
                                      BsmtExposure + BsmtFinSF1 + BsmtUnfSF + HeatingQC + GrLivArea + 
                                      BedroomAbvGr + KitchenQual + TotRmsAbvGrd + Fireplaces + 
                                      FireplaceQu + GarageType + GarageCars + GarageArea + WoodDeckSF + 
                                      OpenPorchSF + MoSold + YrSold + HomeAge + AgeSinceRemod + 
                                      HomeTotalSF + TotalFullBath + TotalHalfBath, k = 2))

summary(Hybrid_AIC) 
#Generate predicted prices to see if the score is imporved
Hybrid_AIC_predict <- 10^(predict(Hybrid_AIC, test))
Hybrid_AIC_pred <- data.frame(Id = test$Id, SalePrice = Hybrid_AIC_predict)
write.csv(Hybrid_AIC_pred, file = "pred_Hybrid_AIC.csv", row.names = FALSE)

###FULL RIDGE REGRESSION
#Create an x matrix and a y vector for both training and test data sets
x_train <- model.matrix(SalePrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + 
                          LotShape + LotConfig + Neighborhood + HouseStyle + OverallQual + 
                          OverallCond + Exterior1st + MasVnrType + ExterQual + Foundation + 
                          BsmtQual + BsmtExposure + BsmtFinSF1 + BsmtUnfSF + HeatingQC + 
                          GrLivArea + BedroomAbvGr + KitchenQual + TotRmsAbvGrd + Fireplaces + 
                          FireplaceQu + GarageType + GarageCars + GarageArea + WoodDeckSF + 
                          OpenPorchSF + MoSold + YrSold + HomeAge + AgeSinceRemod + 
                          HomeTotalSF + TotalFullBath + TotalHalfBath, data = train)[ ,-1]
y_train <- train$SalePrice        
x_test <- model.matrix(SalePrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + 
                         LotShape + LotConfig + Neighborhood + HouseStyle + OverallQual + 
                         OverallCond + Exterior1st + MasVnrType + ExterQual + Foundation + 
                         BsmtQual + BsmtExposure + BsmtFinSF1 + BsmtUnfSF + HeatingQC + 
                         GrLivArea + BedroomAbvGr + KitchenQual + TotRmsAbvGrd + Fireplaces + 
                         FireplaceQu + GarageType + GarageCars + GarageArea + WoodDeckSF + 
                         OpenPorchSF + MoSold + YrSold + HomeAge + AgeSinceRemod + 
                         HomeTotalSF + TotalFullBath + TotalHalfBath, data = test)[ ,-1]
y_test <- test$SalePrice 
#Determine the best tuning parameter lambda through 10-fold cross-validation
set.seed(1)
cv_out_ridge <- cv.glmnet(x_train, y_train, alpha = 0,
                          type.measure = "mse", nfolds = 10)
plot(cv_out_ridge)
#Use the best lambda to run the ridge regression model on training data set
bestlam <- cv_out_ridge$lambda.min
model_ridge <- glmnet(x_train, y_train, alpha = 0,
                      standardize = TRUE, lambda = bestlam)
#Generate the coefficients estimates of the final Ridge regression model
model_ridge$beta
model_ridge$a0
#Generate predicted prices to see if the score is imporved
Ridge_predict <- 10^(predict(model_ridge, newx = x_test))
Ridge_pred <- data.frame(Id = test$Id, SalePrice = Ridge_predict)
write.csv(Ridge_pred, file = "pred_Ridge.csv", row.names = FALSE)

###FULL LASSO REGRESSION
#Use the x matrix and the y vector to determine the best tuning parameter 
#lambda for LASSO through 10-fold cross-validation.
set.seed(1)
cv_out_lasso <- cv.glmnet(x_train, y_train, alpha = 1,
                          type.measure = "mse", nfolds = 10)
plot(cv_out_lasso)
#Use the best lambda to run the LASSO model on the training data set.
bestlam_lasso <- cv_out_lasso$lambda.min
model_lasso <- glmnet(x_train, y_train, alpha = 1,
                      standardize = TRUE, lambda = bestlam_lasso)
#Generate the coefficients estimates of the final LASSO model.
model_lasso$beta
#Generate predicted prices to see if the score is imporved
Lasso_predict <- 10^(predict(model_ridge, newx = x_test))
Lasso_pred <- data.frame(Id = test$Id, SalePrice = Lasso_predict)
write.csv(Lasso_pred, file = "pred_Lasso.csv", row.names = FALSE)

###REGRESSION TREES WITH BAGGING
set.seed(1)
#Perform bagging approach with 500 bootstraps
Tree_Bagging <- randomForest(SalePrice ~ ., data = train,
                             ntrees = 500, mtry = 37, replace = TRUE,
                             importance = TRUE)
Tree_Bagging
#Summary of the importance of each variable
importance(Tree_Bagging)
varImpPlot(Tree_Bagging)
#HomeTotalSF, OverallQual, and Neighborhood are the top 3 important variables
#to predict SalePrice
#Generate predicted prices to see if the score is imporved
Bagging_predict <- 10^(predict(Tree_Bagging, test))
Bagging_pred <- data.frame(Id = test$Id, SalePrice = Bagging_predict)
write.csv(Bagging_pred, file = "pred_Bagging.csv", row.names = FALSE)

###REGRESSION TREES WITH RANDOM FORESTS
#Write a loop to see which mtry value gives the lowest test MSE.
Test_MSE_RF <- rep(NA, 36)
SalePrice_predict <-rep(NA, 36)
for (i in 1:36){
  set.seed(1)
  Tree_RF <- randomForest(SalePrice ~ ., data = train,
                          ntrees = 500, mtry = i, replace = TRUE,
                          importance = TRUE)
  SalePrice_predict <- 10^(predict(Tree_RF, test))
  Test_MSE_RF[i] <- mean((test$SalePrice - SalePrice_predict)^2)
  
}
which.min(Test_MSE_RF)
Test_MSE_RF[1]
#mtry = 1 yields the lowest test MSE
Tree_RF <- randomForest(SalePrice ~ ., data = train,
                        ntrees = 500, mtry = 1, replace = TRUE,
                        importance = TRUE)
Tree_RF
#Summary of the importance of each variable
importance(Tree_RF)
varImpPlot(Tree_RF)
#GrLivArea, HomeTotalSF, and BsmtFinSF1 are the top 3 important variables
#to predict SalePrice
#Generate predicted prices to see if the score is imporved
RF_predict <- 10^(predict(Tree_RF, test))
RF_pred <- data.frame(Id = test$Id, SalePrice = RF_predict)
write.csv(RF_pred, file = "pred_RF.csv", row.names = FALSE)

###REGRESSION TREES WITH BOOSTING
#Write a loop to output the optimal n.trees for each interaction.depth value
n_trees <- rep(0, 6)
min_cv_error <- rep(0, 6)
for(i in 1:6){
  set.seed(1) 
  Tree_Boosting <- gbm(SalePrice ~., data = train, distribution = "gaussian",
                       n.trees = 5000, interaction.depth = i, cv.folds = 10,
                       shrinkage = 0.01)
  n_trees[i] <- which.min(Tree_Boosting$cv.error)
  min_cv_error[i] <- Tree_Boosting$cv.error[which.min(Tree_Boosting$cv.error)]
  
}
which.min(min_cv_error)
#interaction.depth = 4 gives the lowest cv error
#The corrresponding trees are 1361
n_trees[4]
Tree_Boosting <- gbm(SalePrice ~., data = train, distribution = "gaussian",
                     n.trees = 1361, interaction.depth = 1, cv.folds = 10,
                     shrinkage = 0.01)
summary(Tree_Boosting)
#HomeTotalSF, OverallQual, and Neighborhood are the top 3 important variables
#to predict SalePrice
#Generate predicted prices to see if the score is imporved
Boosting_predict <- 10^(predict(Tree_Boosting, test))
Boosting_pred <- data.frame(Id = test$Id, SalePrice = Boosting_predict)
write.csv(Boosting_pred, file = "pred_Boosting2.csv", row.names = FALSE)

###SCORES
#Plain vanilla model = 0.12989
#Prescribed model = 0.12981
#Forward AIC method = 0.12874
#Backward AIC method = 0.12893
#Hybrid AIC method = 0.12853
#Lasso method = 0.12891
#Ridge method = 0.12891
#Bagging method = 0.14962
#Random forest = 0.17029 
#Boosting method = 0.13658
