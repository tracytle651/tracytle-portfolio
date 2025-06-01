#Admission Project
#Thuong Le
#Machine Learning - Spring 2023

#Call relevant packages from library
library(dplyr)
library(tidyverse)
library(caret)
library(e1071)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(DMwR2) 
library(tree)
library(car)

set.seed(1)

#Load original data into R.
clean_data <- read.csv("TU.csv")
#There are many blanks in the data, so I make blanks read as NA
clean_data[clean_data == ''] <- NA

###DATA CLEANING
#Column1 - ID
sum(is.na(clean_data$ID))
#No NA, but ID will be removed in the modeling stage.

#Column2 - train.test
sum(is.na(clean_data$train.test))#No NA.
levels(factor(clean_data$train.test))#No suspicious categories.
#train.test should be removed in the modeling stage.

#Column3 - Entry.Term..Application.
sum(is.na(clean_data$Entry.Term..Application.))#No NA.
levels(factor(clean_data$Entry.Term..Application.))#No suspicious categories.
clean_data$Entry.Term..Application. <- as.factor(clean_data$Entry.Term..Application.)

#Column4 - Admit.Type
sum(is.na(clean_data$Admit.Type))#No NA.
levels(factor(clean_data$Admit.Type))#has only one level
#Since the data set only has first years (i.e.,only one category), 
#Admit.Type should be removed.
clean_data <- subset(clean_data, select = -c(Admit.Type))

#Column5 - Permanent.Postal
sum(is.na(clean_data$Permanent.Postal))#162 NAs
#However, the column "Permanent.Geomarket" had already provided needed information
#regarding the postal codes of different states. Therefore, this column might be
#redundant, and we might just use "Permanent.Geomarket"
#I will remove this column
clean_data <- subset(clean_data, select = -c(Permanent.Postal))

#Column6 - Permanent.Country
sum(is.na(clean_data$Permanent.Country))#1 NA.
levels(factor(clean_data$Permanent.Country))#No suspicious categories.
summary(factor(clean_data$Permanent.Country))
clean_data[is.na(clean_data$Permanent.Country),]
#The ID with NA in Permanent.Country is 11148. 
#Since this person is a US citizen in the column "Citizenship.Status",
#Unites States is assigned to NA.
clean_data$Permanent.Country[is.na(clean_data$Permanent.Country)] <- "United States"
clean_data$Permanent.Country <- as.factor(clean_data$Permanent.Country)

#Column7 - Sex
sum(is.na(clean_data$Sex))#No NA.
levels(factor(clean_data$Sex))#No typos.
clean_data$Sex <- as.factor(clean_data$Sex)

#Column8 - Ethnicity
sum(is.na(clean_data$Ethnicity))#227 NAs.
levels(factor(clean_data$Ethnicity))#No questionable category.
#It is reasonable to impute NAs with "Not specified"
clean_data$Ethnicity[is.na(clean_data$Ethnicity)] <- "Not specified"
clean_data$Ethnicity <- as.factor(clean_data$Ethnicity)

#Column9 - Race
sum(is.na(clean_data$Race))#555 NAs.
#Impute NAs with "Not specified", similar to what we do for Ethnicity.
clean_data$Race[is.na(clean_data$Race)] <- "Not specified"
levels(factor(clean_data$Race))#No questionable category.
table(clean_data$Race)
#The current classification of Race is too detailed, which leads to very 
#low frequencies for some categories.
#I combine some of the categories because a category with a small number of cases won't have 
#a significant effect on the response.
clean_data$Race <- ifelse(clean_data$Race == "American Indian or Alaska Native", "American Indian or Alaska Native",
                          ifelse(clean_data$Race == "American Indian or Alaska Native, White", "American Indian or Alaska Native, White",
                                 ifelse(clean_data$Race == "Asian", "Asian",
                                        ifelse(clean_data$Race == "Asian, White", "Asian, White",
                                               ifelse(clean_data$Race == "Black or African American", "Black or African American",
                                                      ifelse(clean_data$Race == "Black or African American, White", "Black or African American, White",
                                                             ifelse(clean_data$Race == "Not specified", "Not specified",
                                                                    ifelse(clean_data$Race == "White", "White", "Other"))))))))
clean_data$Race <- as.factor(clean_data$Race)

#Column10 - Religion
sum(is.na(clean_data$Religion))#5483 NAs.
levels(factor(clean_data$Religion))#No questionable category.
#Because no religion and other are already included in current levels,
#it is more reasonable to impute NAs with "Not specified".
clean_data$Religion[is.na(clean_data$Religion)] <- "Not specified"
table(clean_data$Religion)
#Religion has lots of options, with some options having a very small number of cases. 
#I combine similar levels into one level, then I combine levels 
#with less than 100 cases into "Other" because a level accounting for low than 1% 
#of training set is very unlikely to have a significant effect on the response.
clean_data$Religion <- ifelse(clean_data$Religion == "Anglican", "Anglican",
                              ifelse(clean_data$Religion == "Baptist", "Baptist",
                                     ifelse(clean_data$Religion == "Bible Churches", "Christian",
                                            ifelse(clean_data$Religion == "Buddhism", "Buddhism",
                                                   ifelse(clean_data$Religion == "Christian", "Christian",
                                                          ifelse(clean_data$Religion == "Christian Reformed", "Christian",
                                                                 ifelse(clean_data$Religion == "Christian Scientist", "Christian",
                                                                        ifelse(clean_data$Religion == "Church of Christ", "Christian",
                                                                               ifelse(clean_data$Religion == "Church of God", "Christian",
                                                                                      ifelse(clean_data$Religion == "Hindu", "Hindu",
                                                                                             ifelse(clean_data$Religion == "Islam/Muslim", "Islam/Muslim",
                                                                                                    ifelse(clean_data$Religion == "Jewish", "Jewish",
                                                                                                           ifelse(clean_data$Religion == "Lutheran", "Lutheran",
                                                                                                                  ifelse(clean_data$Religion == "Methodist", "Methodist",
                                                                                                                         ifelse(clean_data$Religion == "Not specified", "Not specified",
                                                                                                                                ifelse(clean_data$Religion == "Non-Denominational", "Non-Denominational",
                                                                                                                                       ifelse(clean_data$Religion == "None", "None", 
                                                                                                                                              ifelse(clean_data$Religion == "Presbyterian", "Presbyterian",
                                                                                                                                                     ifelse(clean_data$Religion == "Presbyterian Church of America", "Presbyterian",
                                                                                                                                                            ifelse(clean_data$Religion == "Roman Catholic", "Roman Catholic", "Other"))))))))))))))))))))
clean_data$Religion <- as.factor(clean_data$Religion)

#Column11 - First_Source.Origin.First.Source.Date
sum(is.na(clean_data$First_Source.Origin.First.Source.Date))#No NA.
clean_data$First_Source.Origin.First.Source.Date <- as.Date(clean_data$First_Source.Origin.First.Source.Date, 
                                                            format="%m/%d/%Y")

#Column12 - Inquiry.Date
sum(is.na(clean_data$Inquiry.Date))#4579 NAs.
#I will deal with NAs later because I need to use this variable to create
#several new variables.
clean_data$Inquiry.Date <- as.Date(clean_data$Inquiry.Date, format="%m/%d/%Y")

#Column13 - Submitted
sum(is.na(clean_data$Submitted))#No NA.
clean_data$Submitted <- as.Date(clean_data$Submitted, format="%m/%d/%Y")

#Column11-13
#After viewing Column11-13, it would be interesting to see
#whether the differences between submission date and First_Source date
#the differences between submission date and inquiry date affect the response.
#The time difference between submission date and first_source date.
clean_data$Submit_FirstSource <- difftime(clean_data$Submitted, 
                                          clean_data$First_Source.Origin.First.Source.Date, 
                                          units = "weeks")
clean_data$Submit_Inquiry <- difftime(clean_data$Submitted, 
                                      clean_data$Inquiry.Date, units = "weeks")
clean_data$Submit_FirstSource <- round(clean_data$Submit_FirstSource, digits = 0)
clean_data$Submit_FirstSource <- as.numeric(clean_data$Submit_FirstSource)
clean_data$Submit_Inquiry <- round(clean_data$Submit_Inquiry, digits = 0)
clean_data$Submit_Inquiry <- as.numeric(clean_data$Submit_Inquiry)
#There are NAs in Inquiry.Date, 
#thus leading to NAs in Submit_Inquiry.
#Impute NAs in Submit_Inquiry with median values.
clean_data$Submit_Inquiry[is.na(clean_data$Submit_Inquiry)] <- median(clean_data$Submit_Inquiry,
                                                                      na.rm=TRUE)
#I remove Column11-13 in the modeling stage since they are used to construct new variables.   
clean_data <- subset(clean_data, select = -c(First_Source.Origin.First.Source.Date,
                                             Inquiry.Date,
                                             Submitted))

#Column14 - Application.Source
sum(is.na(clean_data$Application.Source))#No NA.
table(clean_data$Application.Source)#No questionable category.
clean_data$Application.Source <- as.factor(clean_data$Application.Source)

#Column15 - Decision.Plan
sum(is.na(clean_data$Decision.Plan))#No NA.
table(clean_data$Decision.Plan)#No questionable category.
clean_data$Decision.Plan <- as.factor(clean_data$Decision.Plan)    

#Column16 - Staff.Assigned.Name
#Based on variable description, this variable might not be useful and provide
#insightful information in the modeling.
#Also, some staffs already left Trinity.
#I will remove this variable
clean_data <- subset(clean_data, select = -c(Staff.Assigned.Name))

#Column17 - Legacy
sum(is.na(clean_data$Legacy))#13658 NAs.
table(clean_data$Legacy)#No questionable category.
#Impute NAs with "No Legacy"
clean_data$Legacy[is.na(clean_data$Legacy)] <- "No Legacy"
#Legacy has many options, leading some options to having 
#only a small number of cases.
#I will group all the options into 3 categories so that each category
#has the chance to affect the response.
clean_data$Legacy <- ifelse(clean_data$Legacy == "Legacy", "Legacy", 
                            ifelse(clean_data$Legacy == "No Legacy", "No Legacy",
                                   ifelse(grepl("Legacy, Opt Out",clean_data$Legacy), 
                                          "Legacy, Opt Out", "Legacy")))
clean_data$Legacy <- as.factor(clean_data$Legacy)

#Column18 - Athlete
sum(is.na(clean_data$Athlete))#13120 NAs.
table(clean_data$Athlete)#No questionable category.
#Impute NAs with "Non-Athlete"
clean_data$Athlete[is.na(clean_data$Athlete)] <- "Non-Athlete"
#Similar to Column17, Column18 has many categories with a few cases.
#Group all options into three categories: 
#Athlete, Non-Athlete, and Athlete, Opt Out.
clean_data$Athlete <- ifelse(clean_data$Athlete == "Athlete", "Athlete", 
                             ifelse(clean_data$Athlete == "Non-Athlete", "Non-Athlete",
                                    ifelse(grepl("Opt Out",clean_data$Athlete), 
                                           "Athlete, Opt Out", "Athlete")))
clean_data$Athlete <- as.factor(clean_data$Athlete)                                                                                                                                                                                               

#Column19 - Sport.1.Sport
sum(is.na(clean_data$Sport.1.Sport))#13120 NAs.
table(clean_data$Sport.1.Sport)#No questionable category.
#Impute NAs with "No Sport".
clean_data$Sport.1.Sport[is.na(clean_data$Sport.1.Sport)] <- "No Sport"
#Group sport men and sport women into one group
#so that each group has sufficient cases to have an impact on the response.
clean_data$Sport.1.Sport <- ifelse(clean_data$Sport.1.Sport == "Baseball", "Baseball", 
                                   ifelse(clean_data$Sport.1.Sport == "Softball", "Softball",
                                          ifelse(clean_data$Sport.1.Sport == "Football", "Football", 
                                                 ifelse(clean_data$Sport.1.Sport == "No Sport", "No Sport", 
                                                        ifelse(grepl("Basketball", clean_data$Sport.1.Sport), "Basketball",
                                                               ifelse(grepl("Cross Country", clean_data$Sport.1.Sport), "Cross Country",
                                                                      ifelse(grepl("Diving", clean_data$Sport.1.Sport), "Diving",
                                                                             ifelse(grepl("Golf", clean_data$Sport.1.Sport), "Golf",
                                                                                    ifelse(grepl("Soccer", clean_data$Sport.1.Sport), "Soccer",
                                                                                           ifelse(grepl("Swimming", clean_data$Sport.1.Sport), "Swimming",
                                                                                                  ifelse(grepl("Tennis", clean_data$Sport.1.Sport), "Tennis",
                                                                                                         ifelse(grepl("Track", clean_data$Sport.1.Sport), "Track", "Volleyball"))))))))))))
clean_data$Sport.1.Sport <- as.factor(clean_data$Sport.1.Sport)

#Column20 - Sport.1.Rating
sum(is.na(clean_data$Sport.1.Rating))#13120 NAs.
table(clean_data$Sport.1.Rating)#No questionable category.
#Impute NAs with "No Sport".
clean_data$Sport.1.Rating[is.na(clean_data$Sport.1.Rating)] <- "No Sport"
clean_data$Sport.1.Rating<- factor(clean_data$Sport.1.Rating, order = TRUE, 
                                   levels = c("No Sport", "Varsity", "Blue Chip", "Franchise"))

#Column21 - Sport.2.Sport
sum(is.na(clean_data$Sport.2.Sport))#14513 NAs.
table(clean_data$Sport.2.Sport)#No questionable category.
#impute NAs with "No 2ndSport".
clean_data$Sport.2.Sport[is.na(clean_data$Sport.2.Sport)] <- "No 2ndSport"
#The number of cases for each sport type is very small (< about 1% of the data set).
#It's better to group all options into 2 categories: 2ndSport vs. No 2ndSport.
clean_data$Sport.2.Sport <- ifelse(clean_data$Sport.2.Sport == "No 2ndSport", 
                                   "No 2ndSport", "2ndSport")
clean_data$Sport.2.Sport <- as.factor(clean_data$Sport.2.Sport)

#Column22 - Sport.2.Rating
sum(is.na(clean_data$Sport.2.Rating))#15085 NAs.
table(clean_data$Sport.2.Rating)#No questionable category.
#Only 58 out of 15143 observations are rated, which is less than 0.5% of the data set!
#Sport.2.Rating will not have much impact on the response.
#I will remove Column22 in the modeling stage.
clean_data <- subset(clean_data, select = -c(Sport.2.Rating))

#Column23 - Sport.3.Sport
sum(is.na(clean_data$Sport.3.Sport))#14907 NAs.
table(clean_data$Sport.3.Sport)#No questionable category.
#impute NAs with "No 3rdSport".
clean_data$Sport.3.Sport[is.na(clean_data$Sport.3.Sport)] <- "No 3rdSport"
#The number of cases for each sport type is very small (< 0.5% of the data set).
#It's better to group all options into 2 categories: 3rdSport vs. No 3rdSport.
clean_data$Sport.3.Sport <- ifelse(clean_data$Sport.3.Sport == "No 3rdSport", 
                                   "No 3rdSport", "3rdSport")
clean_data$Sport.3.Sport <- as.factor(clean_data$Sport.3.Sport)

#Column24 - Sport.3.Rating
sum(is.na(clean_data$Sport.3.Rating))#15140 NAs.
table(clean_data$Sport.3.Rating)#No questionable category.
#Only 3 out of 15143 observations are rated, which will not provide much insightful
#information. Therefore, I will remove Column24
clean_data <- subset(clean_data, select = -c(Sport.3.Rating))

#Column25 - Academic.Interest.1
sum(is.na(clean_data$Academic.Interest.1))#6 NAs.
table(clean_data$Academic.Interest.1)#No questionable category.
clean_data[is.na(clean_data$Academic.Interest.1),]
#Most of the NAs for Academic.Interest.1 have a value for Academic.Interest.2
#We may assign the corresponding values in Academic.Interest.2 
#to NAs in Academic.Interest.1 if Academic.Interest.2 has a value.
clean_data$Academic.Interest.1 <- ifelse(is.na(clean_data$Academic.Interest.1) == TRUE, 
                                         clean_data$Academic.Interest.2, 
                                         clean_data$Academic.Interest.1)
#For the remaining NAs in Academic.Interest.1, assign Undecided.
clean_data$Academic.Interest.1[is.na(clean_data$Academic.Interest.1)] <- "Undecided"
#Group Business related options into "Business".
clean_data$Academic.Interest.1 <- ifelse(grepl("Business", clean_data$Academic.Interest.1), "Business",
                                         ifelse(clean_data$Academic.Interest.1 == "Finance", "Business",
                                                ifelse(clean_data$Academic.Interest.1 == "Entrepreneurship", "Business", 
                                                       clean_data$Academic.Interest.1)))
#Group options with a low number of cases (< 100 cases) into "Others".
frequencies <-data.frame(table(clean_data$Academic.Interest.1))
frequencies
clean_data$Academic.Interest.1.Frequency <- NA
for (i in 1:nrow(clean_data)){
  for(j in 1:nrow(frequencies)){
    if (clean_data$Academic.Interest.1[i] == frequencies$Var1[j])
    {clean_data$Academic.Interest.1.Frequency[i] <- frequencies$Freq[j]}}
}

for (i in 1:nrow(clean_data)){
  if (clean_data$Academic.Interest.1.Frequency[i] < 100)
  {clean_data$Academic.Interest.1[i] <- "Other"}else{
    clean_data$Academic.Interest.1[i]
  }
}
clean_data$Academic.Interest.1 <- as.factor(clean_data$Academic.Interest.1)
#Drop Academic.Interest.1.Frequency in the modeling stage.
clean_data <- subset(clean_data, select = -c(Academic.Interest.1.Frequency))

#Column26 - Academic.Interest.2
sum(is.na(clean_data$Academic.Interest.2))#159 NAs.
#Replace repeated academic interests with Undecided, 
#then make NAs Undecided
clean_data$Academic.Interest.2 <- ifelse(clean_data$Academic.Interest.2 == clean_data$Academic.Interest.1, 
                                         "Undecided", clean_data$Academic.Interest.2)
clean_data$Academic.Interest.2[is.na(clean_data$Academic.Interest.2)] <- "Undecided"
table(clean_data$Academic.Interest.2)#No questionable category.
#Group Business related options into "Business".
clean_data$Academic.Interest.2 <- ifelse(grepl("Business", clean_data$Academic.Interest.2), "Business",
                                         ifelse(clean_data$Academic.Interest.2 == "Finance", "Business",
                                                ifelse(clean_data$Academic.Interest.2 == "Entrepreneurship", "Business", 
                                                       clean_data$Academic.Interest.2)))
#Group options with a low number of cases (<100 cases) into "Others".
frequencies <-data.frame(table(clean_data$Academic.Interest.2))
frequencies
clean_data$Academic.Interest.2.Frequency <- NA
for (i in 1:nrow(clean_data)){
  for(j in 1:nrow(frequencies)){
    if (clean_data$Academic.Interest.2[i] == frequencies$Var1[j])
    {clean_data$Academic.Interest.2.Frequency[i] <- frequencies$Freq[j]}}
}

for (i in 1:nrow(clean_data)){
  if (clean_data$Academic.Interest.2.Frequency[i] < 100)
  {clean_data$Academic.Interest.2[i] <- "Other"}else{
    clean_data$Academic.Interest.2[i]
  }
}
clean_data$Academic.Interest.2 <- as.factor(clean_data$Academic.Interest.2)
#Remember to drop Academic.Interest.2.Frequency in the modeling stage.
clean_data <- subset(clean_data, select = -c(Academic.Interest.2.Frequency))

#Column27 - First_Source.Origin.First.Source.Summary
sum(is.na(clean_data$First_Source.Origin.First.Source.Summary))#No NA.
table(clean_data$First_Source.Origin.First.Source.Summary)#No questionable category.
#Group options with a low number of cases (< 100) into "Other Sources".
frequencies <-data.frame(table(clean_data$First_Source.Origin.First.Source.Summary))
frequencies
clean_data$First_Source.Summary.Frequency <- NA
for (i in 1:nrow(clean_data)){
  for(j in 1:nrow(frequencies)){
    if (clean_data$First_Source.Origin.First.Source.Summary[i] == frequencies$Var1[j])
    {clean_data$First_Source.Summary.Frequency[i] <- frequencies$Freq[j]}}
}

for (i in 1:nrow(clean_data)){
  if (clean_data$First_Source.Summary.Frequency[i] < 100)
  {clean_data$First_Source.Origin.First.Source.Summary[i] <- "Other Sources"}else{
    clean_data$First_Source.Origin.First.Source.Summary[i]
  }
}
clean_data$First_Source.Origin.First.Source.Summary <- as.factor(clean_data$First_Source.Origin.First.Source.Summary)
#Remember to drop First_Source.Summary.Frequency in the modeling stage.
clean_data <- subset(clean_data, select = -c(First_Source.Summary.Frequency))
summary(factor(clean_data$First_Source.Origin.First.Source.Summary))

#Column28 - Total.Event.Participation
sum(is.na(clean_data$Total.Event.Participation))#No NA.
table(clean_data$Total.Event.Participation)#No questionable category.
#3, 4, 5 combined accounts for < 1% of the data set.
#Compared to the number of cases in 0, 1, and 2, the number of cases
#in 3, 4, and 5 won't be very useful in predicting the response.
#Factor the variable and group 3, 4, and 5 into "2 or more".
clean_data$Total.Event.Participation <- ifelse(clean_data$Total.Event.Participation > 2,
                                               2, clean_data$Total.Event.Participation)
#Convert int to char so that level name can be modified.
clean_data$Total.Event.Participation <- as.character(clean_data$Total.Event.Participation)
clean_data$Total.Event.Participation <- ifelse(clean_data$Total.Event.Participation == "2",
                                               "2 or more", clean_data$Total.Event.Participation)
clean_data$Total.Event.Participation <- as.factor(clean_data$Total.Event.Participation)

#Column29 - Count.of.Campus.Visits
sum(is.na(clean_data$Count.of.Campus.Visits))#No NA.
table(clean_data$Count.of.Campus.Visits)#No questionable category.
#Factor the variable and group 5, 6, and 8 into 4.
clean_data$Count.of.Campus.Visits <- ifelse(clean_data$Count.of.Campus.Visits > 4,
                                            4, clean_data$Count.of.Campus.Visits)
#Convert int to char so that I can modify level name.
clean_data$Count.of.Campus.Visits <- as.character(clean_data$Count.of.Campus.Visits)
clean_data$Count.of.Campus.Visits <- ifelse(clean_data$Count.of.Campus.Visits == "4",
                                            "4 or more", clean_data$Count.of.Campus.Visits)
clean_data$Count.of.Campus.Visits <- as.factor(clean_data$Count.of.Campus.Visits)

#Column30 - School..1.Organization.Category
sum(is.na(clean_data$School..1.Organization.Category))#38 NAs.
table(clean_data$School..1.Organization.Category)#No questionable category.
#Only 16 cases belong to College but 15089 cases belong to High School.
#I will remove this variable.
clean_data <- subset(clean_data, select = -c(School..1.Organization.Category))

#Column31 - School.1.Code
sum(is.na(clean_data$School.1.Code))#11879 NAs.
table(clean_data$School.1.Code)
#School Code will not matter much to produce insightful information.
#Additionally, there are 11879 missing values.
#I will remove Column 31 in the modeling stage.
clean_data <- subset(clean_data, select = -c(School.1.Code))

#Column32 - School.1.Class.Rank..Numeric.
sum(is.na(clean_data$School.1.Class.Rank..Numeric.))#8136 NAs.
#Column33 - School.1.Class.Size..Numeric.
sum(is.na(clean_data$School.1.Class.Size..Numeric.))#8136 NAs.
#Percentage rank can more accurately reflect a student's academic performance
#than numeric rank. 
#New Column - School.1.Top.Percent.in.Class
clean_data$School.1.Top.Percent.in.Class <- NA
clean_data$School.1.Top.Percent.in.Class <- 100 *(clean_data$School.1.Class.Rank..Numeric./clean_data$School.1.Class.Size..Numeric.)
#I will remove Column32 and Column33
clean_data <- subset(clean_data, select = -c(School.1.Class.Rank..Numeric.,
                                             School.1.Class.Size..Numeric.))

sum(is.na(clean_data$School.1.Top.Percent.in.Class))
#Impute the 8136 NAs based on Academic.Index column. 
#Since I need to handle NAs in School.1.Top.Percent.in.Class
#according Academic.Index, first let's see whether Academic.Index needs to cleaned.
sum(is.na(clean_data$Academic.Index))#829 NAs.
table(clean_data$Academic.Index)#No questionable level.
#Impute 829 NAs with the most common level.
clean_data$Academic.Index[is.na(clean_data$Academic.Index)] <- 3
#No missing values in Academic.Index now.
#Impute missing values in School.1.Top.Percent.in.Class based on Academic.Index.
clean_index_1 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 1) %>%
  mutate(School.1.Top.Percent.in.Class = replace(School.1.Top.Percent.in.Class, 
                                                 is.na(School.1.Top.Percent.in.Class), mean(School.1.Top.Percent.in.Class, na.rm=TRUE)))

clean_index_2 <- clean_data %>% 
  group_by(Academic.Index) %>% 
  filter(Academic.Index == 2) %>%
  mutate(School.1.Top.Percent.in.Class = replace(School.1.Top.Percent.in.Class, 
                                                 is.na(School.1.Top.Percent.in.Class), mean(School.1.Top.Percent.in.Class, na.rm=TRUE)))

clean_index_3 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 3) %>%
  mutate(School.1.Top.Percent.in.Class = replace(School.1.Top.Percent.in.Class, 
                                                 is.na(School.1.Top.Percent.in.Class), mean(School.1.Top.Percent.in.Class, na.rm=TRUE)))  

clean_index_4 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 4) %>%
  mutate(School.1.Top.Percent.in.Class = replace(School.1.Top.Percent.in.Class, 
                                                 is.na(School.1.Top.Percent.in.Class), mean(School.1.Top.Percent.in.Class, na.rm=TRUE)))    

clean_index_5 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 5) %>%
  mutate(School.1.Top.Percent.in.Class = replace(School.1.Top.Percent.in.Class, 
                                                 is.na(School.1.Top.Percent.in.Class), mean(School.1.Top.Percent.in.Class, na.rm=TRUE)))     

clean_data <- rbind(clean_index_1, clean_index_2, clean_index_3, clean_index_4, clean_index_5)
#Remove index dataframes
rm(clean_index_1, clean_index_2, clean_index_3, clean_index_4, clean_index_5)
#Later when implementing KNN method, I will use mutate()
#function to convert categorical variables to numeric variables, which ignores
#grouping variables. Therefore, to ensure Academic.Index will be converted into
#a numeric variable, I will ungroup clean_data.
clean_data <- clean_data %>% ungroup()
clean_data$Academic.Index <- as.factor(clean_data$Academic.Index)

#Column34 - School.1.GPA
#Remove this variable in the modeling stage
#because School.1.GPA.Recalculated is more accurate.
clean_data <- subset(clean_data, select = -c(School.1.GPA))

#Column35 - School.1.GPA.Scale
#Remove this variable in the modeling stage as it is irrelevant.
clean_data <- subset(clean_data, select = -c(School.1.GPA.Scale))

#Column36 - School.1.GPA.Recalculated
sum(is.na(clean_data$School.1.GPA.Recalculated))#0 
#Check skewness
skewness(clean_data$School.1.GPA.Recalculated)
ggplot(clean_data, aes(School.1.GPA.Recalculated)) + geom_histogram()
#Since it is moderately skewed, and it is understandable for the left skewness as 
#a lot of students got into Trinity with a high GPA (almost 4.0), it is unnecessary
#to do transformation.

#Column37 - School.2.Class.Rank..Numeric.
sum(is.na(clean_data$School.2.Class.Rank..Numeric.))#15143 NAs.
#All cases are blank. Remove this variable.
clean_data <- subset(clean_data, select = -c(School.2.Class.Rank..Numeric.))

#Column38 - School.2.Class.Size..Numeric.
sum(is.na(clean_data$School.2.Class.Size..Numeric.))#15143 NAs.
#All cases are blank. Remove this variable.
clean_data <- subset(clean_data, select = -c(School.2.Class.Size..Numeric.))

#Column39 - School.2.GPA
sum(is.na(clean_data$School.2.GPA))#15143 NAs.
#All cases are blank. Remove this variable.
clean_data <- subset(clean_data, select = -c(School.2.GPA))

#Column40 - School.2.GPA.Scale
sum(is.na(clean_data$School.2.GPA.Scale))#15143 NAs.
#All cases are blank. Remove this variable.
clean_data <- subset(clean_data, select = -c(School.2.GPA.Scale))

#Column41 - School.2.GPA.Recalculated
sum(is.na(clean_data$School.2.GPA.Recalculated))#15143 NAs.
#All cases are blank. Remove this variable.
clean_data <- subset(clean_data, select = -c(School.2.GPA.Recalculated))

#Column42 - School.3.Class.Rank..Numeric.
sum(is.na(clean_data$School.3.Class.Rank..Numeric.))#15143 NAs.
#All cases are blank. Remove this variable.
clean_data <- subset(clean_data, select = -c(School.3.Class.Rank..Numeric.))

#Column43 - School.3.Class.Size..Numeric.
sum(is.na(clean_data$School.3.Class.Size..Numeric.))#15143 NAs.
#All cases are blank. Remove this variable.
clean_data <- subset(clean_data, select = -c(School.3.Class.Size..Numeric.))

#Column44 - School.3.GPA
sum(is.na(clean_data$School.3.GPA))#15143 NAs.
#All cases are blank. Remove this variable.
clean_data <- subset(clean_data, select = -c(School.3.GPA))

#Column45 - School.3.GPA.Scale
sum(is.na(clean_data$School.3.GPA.Scale))#15143 NAs.
#All cases are blank. Remove this variable.
clean_data <- subset(clean_data, select = -c(School.3.GPA.Scale))

#Column46 - School.3.GPA.Recalculated
sum(is.na(clean_data$School.3.GPA.Recalculated))#15143 NAs.
#All cases are blank. Remove this variable.
clean_data <- subset(clean_data, select = -c(School.3.GPA.Recalculated))

#Column47 - ACT.Composite
sum(is.na(clean_data$ACT.Composite))#7502 NAs.
#For NA values, it is either because students submitted SAT scores instead of ACT,
#or when both ACT and SAT scores become optional. 
#First, let's see the lowest SAT score in the data set.
summary(clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section)
#The lowest SAT scores in the data set is 970
#I will convert SAT scores into ACT based on the ACT-SAT-Concordance-Table pdf.
clean_data$ACT.Composite <- ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1570, 36,
                                   ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1530, 35,
                                          ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1490, 34,
                                                 ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1450, 33,
                                                        ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1420, 32,
                                                               ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1390, 31,
                                                                      ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1360, 30,
                                                                             ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1330, 29,
                                                                                    ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1300, 28,
                                                                                           ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1260, 27,
                                                                                                  ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1230, 26,
                                                                                                         ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1200, 25,
                                                                                                                ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1160, 24,
                                                                                                                       ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1130, 23,
                                                                                                                              ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1100, 22,
                                                                                                                                     ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1060, 21,
                                                                                                                                            ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 1030, 20,
                                                                                                                                                   ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 990, 19,
                                                                                                                                                          ifelse(is.na(clean_data$ACT.Composite) & clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section > 960, 18, clean_data$ACT.Composite)))))))))))))))))))

sum(is.na(clean_data$ACT.Composite))#1724 NAs.
#There are still 1724 NAs because ACT and SAT scores have become optional since Fall 2021, 
#and some students do not submit both scores for admission. I will impute the mean value
#to the rest of the NAs values.
clean_data$ACT.Composite[is.na(clean_data$ACT.Composite)] <- mean(clean_data$ACT.Composite, na.rm = TRUE)
#Round score to the whole number
clean_data$ACT.Composite <- round(clean_data$ACT.Composite)
#Check skewness
skewness(clean_data$ACT.Composite)
ggplot(clean_data, aes(ACT.Composite)) + geom_histogram()
#The skewness is -0.327, which is good. No transformation needed.

#Column48 - ACT.English
#Column 49 - ACT.Reading
#Column50 - ACT.Math
#Column51 - ACT.Science.Reasoning
#Column52 - ACT.Writing
#Since ACT Composite is already a good indicator for ACT scores generally,
#scores on each section will not matter much to make analyses.
#Therefore, I will remove those variables.
clean_data <- subset(clean_data, select = -c(ACT.English, ACT.Reading,
                                             ACT.Math, ACT.Science.Reasoning,
                                             ACT.Writing))

#Column53 - SAT.I.CR...M
sum(is.na(clean_data$SAT.I.CR...M))#14569 NAs.
#Since this the SAT scores that have not been recentered and we will use ACT scores,
#I will remove this variable.
clean_data <- subset(clean_data, select = -c(SAT.I.CR...M))

#Column54 - SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
sum(is.na(clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section)) #6711 NAs
#We will use ACT scores for assessing the model, I will remove this SAT variable
clean_data <- subset(clean_data, select = -c(SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section))

#Column55 - Permanent.Geomarket
sum(is.na(clean_data$Permanent.Geomarket))#1 NA
clean_data[is.na(clean_data$Permanent.Geomarket),]
summary(factor(clean_data$Permanent.Geomarket))
#Impute the most frequent value to the missing values
clean_data$Permanent.Geomarket[is.na(clean_data$Permanent.Geomarket)] <- "TX-06"
#Check suspicious categories
levels(factor(clean_data$Permanent.Geomarket))
#It is better to group them since there are a lot of levels with very small number 
#of values out of 15143 observations.
#Group into Pacific
clean_data$Permanent.Geomarket <- ifelse(startsWith(clean_data$Permanent.Geomarket, "WA-") == TRUE, "Pacific",
                                         ifelse(startsWith(clean_data$Permanent.Geomarket, "OR-") == TRUE, "Pacific",
                                                ifelse(startsWith(clean_data$Permanent.Geomarket, "CA-") == TRUE, "Pacific",
                                                       ifelse(startsWith(clean_data$Permanent.Geomarket, "AK-") == TRUE, "Pacific",
                                                              ifelse(startsWith(clean_data$Permanent.Geomarket, "HI-") == TRUE, "Pacific",
                                                                     clean_data$Permanent.Geomarket)))))
#Group into Mountain
clean_data$Permanent.Geomarket <- ifelse(startsWith(clean_data$Permanent.Geomarket, "ID-") == TRUE, "Mountain",
                                         ifelse(startsWith(clean_data$Permanent.Geomarket, "NV-") == TRUE, "Mountain",
                                                ifelse(startsWith(clean_data$Permanent.Geomarket, "AZ-") == TRUE, "Mountain",
                                                       ifelse(startsWith(clean_data$Permanent.Geomarket, "MT-") == TRUE, "Mountain",
                                                              ifelse(startsWith(clean_data$Permanent.Geomarket, "WY-") == TRUE, "Mountain",
                                                                     ifelse(startsWith(clean_data$Permanent.Geomarket, "UT-") == TRUE, "Mountain",
                                                                            ifelse(startsWith(clean_data$Permanent.Geomarket, "CO-") == TRUE, "Mountain",
                                                                                   ifelse(startsWith(clean_data$Permanent.Geomarket, "NM-") == TRUE, "Mountain",
                                                                                          clean_data$Permanent.Geomarket))))))))
#Group into West North Central   
clean_data$Permanent.Geomarket <- ifelse(startsWith(clean_data$Permanent.Geomarket, "ND-") == TRUE, "West North Central",
                                         ifelse(startsWith(clean_data$Permanent.Geomarket, "SD-") == TRUE, "West North Central",
                                                ifelse(startsWith(clean_data$Permanent.Geomarket, "NE-") == TRUE, "West North Central",
                                                       ifelse(startsWith(clean_data$Permanent.Geomarket, "KS-") == TRUE, "West North Central",
                                                              ifelse(startsWith(clean_data$Permanent.Geomarket, "MN-") == TRUE, "West North Central",
                                                                     ifelse(startsWith(clean_data$Permanent.Geomarket, "IA-") == TRUE, "West North Central",
                                                                            ifelse(startsWith(clean_data$Permanent.Geomarket, "MO-") == TRUE, "West North Central",
                                                                                   clean_data$Permanent.Geomarket)))))))
#Group into West South Central
clean_data$Permanent.Geomarket <- ifelse(startsWith(clean_data$Permanent.Geomarket, "OK-") == TRUE, "West South Central",
                                         ifelse(startsWith(clean_data$Permanent.Geomarket, "TX-") == TRUE, "West South Central",
                                                ifelse(startsWith(clean_data$Permanent.Geomarket, "AR-") == TRUE, "West South Central",
                                                       ifelse(startsWith(clean_data$Permanent.Geomarket, "LA-") == TRUE, "West South Central",
                                                              clean_data$Permanent.Geomarket))))
#Group into East North Central
clean_data$Permanent.Geomarket <- ifelse(startsWith(clean_data$Permanent.Geomarket, "WI-") == TRUE, "East North Central",
                                         ifelse(startsWith(clean_data$Permanent.Geomarket, "IL-") == TRUE, "East North Central",
                                                ifelse(startsWith(clean_data$Permanent.Geomarket, "MI-") == TRUE, "East North Central",
                                                       ifelse(startsWith(clean_data$Permanent.Geomarket, "IN-") == TRUE, "East North Central",
                                                              ifelse(startsWith(clean_data$Permanent.Geomarket, "OH-") == TRUE, "East North Central",
                                                                     clean_data$Permanent.Geomarket)))))
#Group into East South Central
clean_data$Permanent.Geomarket <- ifelse(startsWith(clean_data$Permanent.Geomarket, "KY-") == TRUE, "East South Central",
                                         ifelse(startsWith(clean_data$Permanent.Geomarket, "TN-") == TRUE, "East South Central",
                                                ifelse(startsWith(clean_data$Permanent.Geomarket, "MS-") == TRUE, "East South Central",
                                                       ifelse(startsWith(clean_data$Permanent.Geomarket, "AL-") == TRUE, "East South Central",
                                                              clean_data$Permanent.Geomarket))))

#Group into Middle Atlantic
clean_data$Permanent.Geomarket <- ifelse(startsWith(clean_data$Permanent.Geomarket, "PA-") == TRUE, "Middle Atlantic",
                                         ifelse(startsWith(clean_data$Permanent.Geomarket, "NY-") == TRUE, "Middle Atlantic",
                                                ifelse(startsWith(clean_data$Permanent.Geomarket, "NJ-") == TRUE, "Middle Atlantic",
                                                       clean_data$Permanent.Geomarket)))

#Group into South Atlantic
clean_data$Permanent.Geomarket <- ifelse(startsWith(clean_data$Permanent.Geomarket, "WV-") == TRUE, "South Atlantic",
                                         ifelse(startsWith(clean_data$Permanent.Geomarket, "MD-") == TRUE, "South Atlantic",
                                                ifelse(startsWith(clean_data$Permanent.Geomarket, "DE-") == TRUE, "South Atlantic",
                                                       ifelse(startsWith(clean_data$Permanent.Geomarket, "DC-") == TRUE, "South Atlantic",
                                                              ifelse(startsWith(clean_data$Permanent.Geomarket, "VA-") == TRUE, "South Atlantic",
                                                                     ifelse(startsWith(clean_data$Permanent.Geomarket, "NC-") == TRUE, "South Atlantic",
                                                                            ifelse(startsWith(clean_data$Permanent.Geomarket, "SC-") == TRUE, "South Atlantic",
                                                                                   ifelse(startsWith(clean_data$Permanent.Geomarket, "GA-") == TRUE, "South Atlantic",
                                                                                          ifelse(startsWith(clean_data$Permanent.Geomarket, "FL-") == TRUE, "South Atlantic",
                                                                                                 clean_data$Permanent.Geomarket)))))))))
#Group into New England
clean_data$Permanent.Geomarket <- ifelse(startsWith(clean_data$Permanent.Geomarket, "ME-") == TRUE, "New England",
                                         ifelse(startsWith(clean_data$Permanent.Geomarket, "VT-") == TRUE, "New England",
                                                ifelse(startsWith(clean_data$Permanent.Geomarket, "NH-") == TRUE, "New England",
                                                       ifelse(startsWith(clean_data$Permanent.Geomarket, "MA-") == TRUE, "New England",
                                                              ifelse(startsWith(clean_data$Permanent.Geomarket, "CT-") == TRUE, "New England",
                                                                     ifelse(startsWith(clean_data$Permanent.Geomarket, "RI-") == TRUE, "New England",
                                                                            clean_data$Permanent.Geomarket))))))
#Group into International
clean_data$Permanent.Geomarket <- ifelse(startsWith(clean_data$Permanent.Geomarket, "INT-") == TRUE, "International",
                                         clean_data$Permanent.Geomarket)
#Check categories again
levels(factor(clean_data$Permanent.Geomarket))
summary(factor(clean_data$Permanent.Geomarket))
#There are still some undefined categories.
#First, PR-01 likely refers to Puerto Rico's congressional district 1. 
#It's classified as an "unincorporated territory," meaning the island is controlled 
#by the U.S. but is separate from the mainland. Puerto Ricans by birth have American 
#citizenship and can move freely between the island and the U.S. mainland. But 
#unlike Hawaii, Puerto Rico is not a state.
#For the rest of the codes, these codes represent the different regions of the 
#U.S. for military mail addresses, and are also seen as "unincorporated territories"
#US-AE refers to Armed Forces Europe, Africa, and Canada
#US-AP refers to Armed Forces Pacific
#US-GU refers to Guam
#US-MP refers to Northern Mariana Islands
#US-VI refers to United States Virgin Islands
#Additionally, they do not account for the large number of values in the data set.
#Therefore, it is reasonable to group these into International category.
clean_data$Permanent.Geomarket <- ifelse(clean_data$Permanent.Geomarket == "PR-01", "International",
                                         ifelse(clean_data$Permanent.Geomarket == "US-AE", "International",
                                                ifelse(clean_data$Permanent.Geomarket == "US-AP", "International",
                                                       ifelse(clean_data$Permanent.Geomarket == "US-GU", "International",
                                                              ifelse(clean_data$Permanent.Geomarket == "US-MP", "International",
                                                                     ifelse(clean_data$Permanent.Geomarket == "US-VI", "International",
                                                                            clean_data$Permanent.Geomarket))))))

#Check levels again                                                      
levels(factor(clean_data$Permanent.Geomarket))
#It looks good. No more grouping.
clean_data$Permanent.Geomarket <- as.factor(clean_data$Permanent.Geomarket)
#Since we are using Permanent.Geomarket now, I will remove Permanent.Country
clean_data <- subset(clean_data, select = -c(Permanent.Country))

#Column56 - Citizenship.Status
sum(is.na(clean_data$Citizenship.Status))#0 NA
levels(factor(clean_data$Citizenship.Status))#No suspicious categories.
clean_data$Citizenship.Status <- factor(clean_data$Citizenship.Status)

#Column57 - Academic.Index
sum(is.na(clean_data$Academic.Index))#0 NA
levels(factor(clean_data$Academic.Index))#No suspicious categories.
clean_data$Academic.Index <- factor(clean_data$Academic.Index)

#Column58 - Intend.to.Apply.for.Financial.Aid.
sum(is.na(clean_data$Intend.to.Apply.for.Financial.Aid.))#21 NAs
levels(factor(clean_data$Intend.to.Apply.for.Financial.Aid.))#No suspicious categories.
summary(factor(clean_data$Intend.to.Apply.for.Financial.Aid.))
#It is reasonable to assume that NA values probably indicate 0 (students who do not intend
#apply for financial aid), since if students do intend to apply for financial aid, 
#it should have been recorded as 1.
#Impute 0 to NA values
clean_data$Intend.to.Apply.for.Financial.Aid.[is.na(clean_data$Intend.to.Apply.for.Financial.Aid.)] <- "0"
clean_data$Intend.to.Apply.for.Financial.Aid. <- factor(clean_data$Intend.to.Apply.for.Financial.Aid.)

#Column59 - Merit.Award
sum(is.na(clean_data$Merit.Award))#0 NA
levels(factor(clean_data$Merit.Award))
summary(factor(clean_data$Merit.Award))
#There are a lot of categories that do not match the variable description because
#the description probably overlooked some levels.
#After careful consideration, I will recategorize all the levels into three main levels: 
#Full Ride, Non Full Ride and No Merit with the purpose of providing as much insightful
#information as possible, and to avoid aliased coefficients issues.
#First, for D category, the description probably overlooked the D18 and D20, as
#on the website, it indicates that Dean's Scholarship ranges from $18,000-$30,000
#(most recent record). Additionally, even if I combine them altogether, they still
#account for a small number of values in the data set. Hence, I will categorize them
#as Non Full Ride.
clean_data$Merit.Award[clean_data$Merit.Award == "D12.5"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "D18"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "D20"] <- "Non Full Ride"
#For I category, after careful consideration, I found out that each I level
#does not account for a large number of values within the data set. 
#I will combine all of them as Non Full Ride
clean_data$Merit.Award[clean_data$Merit.Award == "I5" | 
                         clean_data$Merit.Award == "I7.5" |
                         clean_data$Merit.Award == "I10" |
                         clean_data$Merit.Award == "I9" |
                         clean_data$Merit.Award == "I12" |
                         clean_data$Merit.Award == "I12.5" |
                         clean_data$Merit.Award == "I15" |
                         clean_data$Merit.Award == "I17" |
                         clean_data$Merit.Award == "I18" |
                         clean_data$Merit.Award == "I19" |
                         clean_data$Merit.Award == "I20" |
                         clean_data$Merit.Award == "I21" |
                         clean_data$Merit.Award == "I22" |
                         clean_data$Merit.Award == "I23" |
                         clean_data$Merit.Award == "I24" |
                         clean_data$Merit.Award == "I25" |
                         clean_data$Merit.Award == "I26" |
                         clean_data$Merit.Award == "I27" |
                         clean_data$Merit.Award == "I28" |
                         clean_data$Merit.Award == "I30" |
                         clean_data$Merit.Award == "I32" |
                         clean_data$Merit.Award == "I33" |
                         clean_data$Merit.Award == "I35" |
                         clean_data$Merit.Award == "I38" |
                         clean_data$Merit.Award == "I40" |
                         clean_data$Merit.Award == "I43" |
                         clean_data$Merit.Award == "I45" |
                         clean_data$Merit.Award == "I50" |
                         clean_data$Merit.Award == "I52" ] <- "Non Full Ride"
#Combine I0 and Z0, which means No Merit for international and domestic students
#respectively, because each contain only small number of values
clean_data$Merit.Award[clean_data$Merit.Award == "I0"] <- "No Merit" 
clean_data$Merit.Award[clean_data$Merit.Award == "Z0"] <- "No Merit" 
#Combine TTS and SEM since they are both indicate Full Ride scholarship
clean_data$Merit.Award[clean_data$Merit.Award == "TTS"] <- "Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "SEM"] <- "Full Ride"
#Additionally, X0 and Y0 indicates those who do not have to pay tuition fee 
#(due to being child of a Trinity-related person) on an exchange agreement. Since
#they account for only a tiny number of the total observations, it is reasonable
#to merge them into Full Ride category as well.
clean_data$Merit.Award[clean_data$Merit.Award == "X0"] <- "Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "Y0"] <- "Full Ride"
#For TT category, which means Trinity Tiger Award Scholarship, the description
#also overlooked some levels as the website said this is the scholarship up to
#$12,000. I will merge them into Non Full Ride.
clean_data$Merit.Award[clean_data$Merit.Award == "TT10"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "TT12"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "TT125"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "TT9"] <- "Non Full Ride"
#In terms of M category, the description also overlooked some levels. Since M24 and 
#M25 are approximately the same amount of money awarded ($24,000~$25,000)
#I will merge them into Non Full Ride
clean_data$Merit.Award[clean_data$Merit.Award == "M24"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "M25"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "M26"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "M27"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "M30"] <- "Non Full Ride"
#The description also overlooked some levels of the President's Scholarship (P) as the 
#website indicated that this scholarship has a range from $18,000-$30,000. Similarly,
#I will merge them into Non Full Ride.
clean_data$Merit.Award[clean_data$Merit.Award == "P17"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "P18"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "P23"] <- "Non Full Ride"
#I will do the similar thing for T category.
clean_data$Merit.Award[clean_data$Merit.Award == "T21"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "T22"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "T23"] <- "Non Full Ride"
clean_data$Merit.Award[clean_data$Merit.Award == "T25"] <- "Non Full Ride"
#Check again
levels(factor(clean_data$Merit.Award))
summary(factor(clean_data$Merit.Award))
clean_data$Merit.Award <- as.factor(clean_data$Merit.Award)

#Column60 - SAT.Concordance.Score..of.SAT.R.
#Column61 - ACT.Concordance.Score..of.SAT.R.
#Column62 - ACT.Concordance.Score..of.SAT.
#Column63 - Test.Optional
#Column64 - SAT.I.Critical.Reading
#Column65 - SAT.I.Math
#Column66 - SAT.I.Writing
#Column67 - SAT.R.Evidence.Based.Reading.and.Writing.Section
#Column68 - SAT.R.Math.Section
clean_data <- subset(clean_data, select = -c(SAT.Concordance.Score..of.SAT.R.,
                                             ACT.Concordance.Score..of.SAT.R.,
                                             ACT.Concordance.Score..of.SAT.,
                                             Test.Optional,
                                             SAT.I.Critical.Reading,
                                             SAT.I.Math,
                                             SAT.I.Writing,
                                             SAT.R.Evidence.Based.Reading.and.Writing.Section,
                                             SAT.R.Math.Section))

#Column69 - Decision
sum(is.na(clean_data$Decision))#0 NA
levels(factor(clean_data$Decision))#No supicious categories
clean_data$Decision <- factor(clean_data$Decision)

###SPLIT DATA
#Remove ID column
clean_data <- subset(clean_data, select = -c(ID))
#Split into training and test data set
Train <- clean_data[clean_data$train.test == "train",]
Test <- clean_data[clean_data$train.test == "test",]
#Remove train.test column
Train <- subset(Train, select = -c(train.test))
Test <- subset(Test, select = -c(train.test))

###LOGISTIC REGRESSION
#Construct a logistic model using all predictors in the training set
Logistic_model <- glm(Decision ~ ., family = binomial(link = "logit"),
                      data = Train) 
contrasts(Train$Decision)
summary(Logistic_model)
#Check for aliased coefficients issue
vif(Logistic_model)
alias(Logistic_model)
#Remove culprits
Train <- subset(Train, select = -c(Sport.1.Rating))
Train <- subset(Train, select = -c(Sport.1.Sport))
Test <- subset(Test, select = -c(Sport.1.Rating))
Test <- subset(Test, select = -c(Sport.1.Sport))
#Run the Logistic Model again and check for aliased coefficients issue
Logistic_model <- glm(Decision ~ ., family = binomial(link = "logit"),
                      data = Train)
alias(Logistic_model)
#R encoded 0 (not accepted the offer) as 0 and 1 (accepted the offer) as 1. 
#Therefore summary() outputs the probability that a student accepted the offer
#given X values, that is P(Y=MM|X).
#I will write a loop to determine the optimal cutoff probability 
Logistic_prob_test <- predict(Logistic_model, type = "response", Test)
Kappa <- rep(0,81)
cutoff <- rep(0, 81)
index <- 0
for (i in seq(from = 0.1, to = 0.9, by = 0.01)){
  set.seed(1)
  index <- index + 1
  Logistic_pred_test <- ifelse(Logistic_prob_test > i, "1", "0")
  Logistic_conting_test <- table(Logistic_pred_test, Test$Decision, 
                                 dnn = c("Predicted", "Actual"))
  Logistic_cm_test <- confusionMatrix(Logistic_conting_test)
  cutoff[index] <- i
  Kappa[index] <- Logistic_cm_test$overall["Kappa"]
}
cutoff[which.max(Kappa)]
Kappa[which.max(Kappa)]
#The optimal cutoff probability = 0.3
#Kappa (Logistic Regression) = 0.5350381

###KNN
#Since KNN does not accept non-numeric predictors, I convert categorical variables 
#into a numeric variable.
Train2 <- Train[ , -24] %>% mutate_if(is.factor, as.numeric)
Train2$Decision <- Train$Decision
Test2 <- Test[,-24] %>% mutate_if(is.factor, as.numeric)
Test2$Decision <- Test$Decision
#I will run a loop to see which k value generates the best Kappa for the test set
#and to see the max Kappa value.
library(class)
Kappa <- rep(0, 100)
for(i in 1:100){
  set.seed(1)
  nn_test <- knn(train = Train2, test = Test2, cl = Train2$Decision, k = i)
  nn_conting_test <- table(nn_test, Test2$Decision, 
                           dnn = c("Predicted", "Actual"))
  nn_cm_test <- confusionMatrix(nn_conting_test)
  Kappa[i] <- nn_cm_test$overall["Kappa"]
}
Kappa[which.max(Kappa)]
#Kappa (knn) = 0.1077172

###SIMPLE CLASSIFICATION TREE
#Fit a simple/basic classification tree.
simple_tree <- tree(Decision ~., split = "gini", Train)
summary(simple_tree)
#Display the tree
plot(simple_tree)
text(simple_tree, pretty = 0)
#Classification performance (Kappa) for the test set
simple_tree_pred_test <- predict(simple_tree, Test, type = "class")
simple_tree_conting_test <- table(simple_tree_pred_test, 
                                  Test$Decision, 
                                  dnn = c("Predicted", "Actual"))
simple_tree_conting_test
simple_tree_cm_test <- confusionMatrix(simple_tree_conting_test)
simple_tree_cm_test
simple_tree_cm_test$overall["Kappa"]
#Kappa (Simple Classification Tree) = 0.3849844  

###TREE PRUNING
#I will consider whether pruning the tree might lead to improved results.
set.seed(1)
#Perform cross-validation to determine the optimal level of tree complexity
cv_Tree <- cv.tree(simple_tree, FUN = prune.misclass, K = 10)
cv_Tree$size[which.min(cv_Tree$dev)]
plot(cv_Tree$size, cv_Tree$dev, type = "b")
#The output shows that the tree with 26 terminal nodes results in the
#lowest cv error.
#Apply the prune.misclass() function in order to prune the tree to obtain 
#the tree with 26 terminal nodes.
prune_tree <- prune.misclass(simple_tree, best = 26)
plot(prune_tree)
text(prune_tree, pretty = 0)
#Classification performance (Kappa) for the test set
prune_tree_pred_test <- predict(prune_tree, Test, type = "class")
prune_tree_conting_test <- table(prune_tree_pred_test, 
                                 Test$Decision, 
                                 dnn = c("Predicted", "Actual"))
prune_tree_conting_test
prune_tree_cm_test <- confusionMatrix(prune_tree_conting_test)
prune_tree_cm_test
prune_tree_cm_test$overall["Kappa"]
#Kappa (Pruned Tree) = 0.4192191
#The pruned tree performs better than the unpruned tree

###BAGGING
#Create 500 bootstrap datasets to perform bagging
set.seed(1)
Tree_Bagging <- randomForest(Decision ~ ., data = Train,
                             ntrees = 500, mtry = 26, split = "gini", 
                             replace = TRUE, importance = TRUE)
Tree_Bagging
plot(Tree_Bagging)
#The class.errors of 1, 0, and Both combined can be viewed in Tree_Bagging$err.rate 
#for each n.tree value.
Tree_Bagging$err.rate
#Find the optimal ntrees.
which.min(Tree_Bagging$err.rate[ , 1])
#The output is 189. In the plot, 500 is able to give us a low cv error, so we 
#do not have to change the value in the n.trees argument. 
#Overall summary of the importance of each predictor using importance().
importance(Tree_Bagging)
varImpPlot(Tree_Bagging)
#Classification performance (Kappa) for the test set
bag_test_pred <- predict(Tree_Bagging, Test, type = "class")
bag_conting_test <- table(bag_test_pred, Test$Decision, 
                          dnn = c("Predicted", "Actual"))
bag_conting_test
bag_cm_test <- confusionMatrix(bag_conting_test)
bag_cm_test
bag_cm_test$overall["Kappa"]
#Kappa (Bagging) = 0.4740288 

###RANDOM FOREST
#Perform RF using 500 bootstrap data sets and a loop to determine the optimal mtry
Test_Kappa_RF <- rep(0, 25)
for(i in 1:25){
  set.seed(1)
  Tree_RF <- randomForest(Decision ~ ., data = Train,
                          ntrees = 500, mtry = i, split = "gini", replace = TRUE,
                          importance = TRUE)
  Test_pred_RF <- predict(Tree_RF, Test, type = "class")
  RF_conting_test <- table(Test_pred_RF, Test$Decision, 
                           dnn = c("Predicted", "Actual"))
  RF_cm_test <- confusionMatrix(RF_conting_test)
  Test_Kappa_RF[i] <- RF_cm_test$overall["Kappa"]
}
which.max(Test_Kappa_RF)
#mtry = 8 gives the highest Kappa.
Test_Kappa_RF[which.max(Test_Kappa_RF)]
#Kappa (Random Forest) = 0.4991938

###BOOSTING
Train$Decision <- ifelse(Train$Decision == "0", 0, 1)
Test$Decision <- ifelse(Test$Decision == "0", 0, 1)
#Increase memory size
memory.size() #Checking the memory size
memory.limit() #Checking the set limit
memory.limit(size=56000)
#Write a loop to output the optimal n.trees for each interaction.depth value.
n_trees <- rep(0, 6)
min_cv_error <- rep(0, 6)
for(i in 1:6){
  set.seed(1)
  Tree_Boosting <- gbm(Decision ~., data = Train, distribution = "bernoulli",
                       n.trees = 5000, interaction.depth = i, cv.folds = 10,
                       shrinkage = 0.01)
  n_trees[i] <- which.min(Tree_Boosting$cv.error)
  min_cv_error[i] <- Tree_Boosting$cv.error[which.min(Tree_Boosting$cv.error)]
}
#Check which interaction.depth value gives the lowest cv error.
which.min(min_cv_error)
#interaction.depth = 3 yields the lowest cv error.
n_trees[3]
#Corresponding n_trees = 1737
#I will use the optimal n.trees and interaction.depth from the loop.
set.seed(1)
Tree_Boosting <- gbm(Decision ~., data = Train, 
                     distribution = "bernoulli",
                     n.trees = 1737, interaction.depth = 3,
                     shrinkage = 0.01)
summary(Tree_Boosting)
#We can see a relative influence plot and the relative influence statistics 
#of each variable in the model.
#Classification performance (Kappa) for the test set using the optimal n.trees and 
#interaction.depth from the loop.
#I will write a loop to determine the optimal cutoff probability 
boost_prob_test <- predict(Tree_Boosting, type = "response", 
                           Test)
Kappa <- rep(0,81)
cutoff_prob <- rep(0, 81)
index <- 0
for(i in seq(from = 0.1, to = 0.9, by = 0.01)){
  set.seed(1)
  index <- index + 1
  boost_pred_results_test <- ifelse(boost_prob_test > i, 1, 0)
  boost_conting_test <- table(boost_pred_results_test, Test$Decision, 
                              dnn = c("Predicted", "Actual"))
  boost_cm_test <- confusionMatrix(boost_conting_test)
  cutoff_prob[index] <- i
  Kappa[index] <- boost_cm_test$overall["Kappa"]
}
cutoff_prob[which.max(Kappa)]
Kappa[which.max(Kappa)]
#Optimal cutoff prob = 0.35
#Kappa (Boosting) = 0.5384955 

###SVM WITH LINEAR KERNEL
Train$Decision <- as.factor(Train$Decision)
Test$Decision <- as.factor(Test$Decision)
#I will use 10-fold cross-validation to determine the optimal cost.
#Since this is a large dataset with a large number of observations, R would run v
#very slow if we set the range of cost and run in one time. I will try individual
#values of cost to see which value would yield the lowest cross validation error
#I will choose the base = 2 and the range of the power is from -5 to 3
#I will start with the base = 2 and the power = -5
set.seed(1)
svc_best_cost_n5 <- tune(method = svm, train.x = Decision ~., train.y = NULL,
                         data = Train, kernel = "linear",
                         cost = 2^(-5))
svc_best_cost_n5
set.seed(1)
svc_best_cost_n4 <- tune(method = svm, train.x = Decision ~., train.y = NULL,
                         data = Train, kernel = "linear",
                         cost = 2^(-4))
svc_best_cost_n4
set.seed(1)
svc_best_cost_n3 <- tune(method = svm, train.x = Decision ~., train.y = NULL,
                         data = Train, kernel = "linear",
                         cost = 2^(-3))
svc_best_cost_n3
set.seed(1)
svc_best_cost_n2 <- tune(method = svm, train.x = Decision ~., train.y = NULL,
                         data = Train, kernel = "linear",
                         cost = 2^(-2))
svc_best_cost_n2
set.seed(1)
svc_best_cost_n1 <- tune(method = svm, train.x = Decision ~., train.y = NULL,
                         data = Train, kernel = "linear",
                         cost = 2^(-1))
svc_best_cost_n1
set.seed(1)
svc_best_cost_0 <- tune(method = svm, train.x = Decision ~., train.y = NULL,
                        data = Train, kernel = "linear",
                        cost = 2^(0))
svc_best_cost_0
set.seed(1)
svc_best_cost_1 <- tune(method = svm, train.x = Decision ~., train.y = NULL,
                      data = Train, kernel = "linear",
                      cost = 2^(1))
svc_best_cost_1
set.seed(1)
svc_best_cost_2 <- tune(method = svm, train.x = Decision ~., train.y = NULL,
                      data = Train, kernel = "linear",
                      cost = 2^(2))
svc_best_cost_2
set.seed(1)
svc_best_cost_3 <- tune(method = svm, train.x = Decision ~., train.y = NULL,
                        data = Train, kernel = "linear",
                        cost = 2^(3))
svc_best_cost_3
#When cost = 2^(-5), corresponding cross validation error = 0.1772
#When cost = 2^(-4), corresponding cross validation error = 0.168
#When cost = 2^(-3), corresponding cross validation error = 0.1615
#When cost = 2^(-2), corresponding cross validation error = 0.1604
#When cost = 2^(-1), corresponding cross validation error = 0.1604
#When cost = 2^(0), corresponding cross validation error = 0.1598
#When cost = 2^(1), corresponding cross validation error = 0.1596
#When cost = 2^(2), corresponding cross validation error = 0.1599
#When cost = 2^(3), corresponding cross validation error = 0.1599

#Cost = 2^(1) gives the lowest cross validation error
#Hence, optimal cost = 2^(1)
#Construct the support vector classifier model
best_svm_linear <- svc_best_cost_1$best.model
best_svm_linear
#Compute the coefficients of the best SVC model.
coefficients_best_svm_linear <- t(best_svm_linear$SV) %*% best_svm_linear$coefs
coefficients_best_svm_linear
#Compute the intercept of the best SVC model.
Intercept_best_svm_linear <- -best_svm_linear$rho
Intercept_best_svm_linear
#The 5 most important features of SVM linear model are: 
#AthleteAthlete..Opt.Out, LegacyLegacy..Opt.Out, Decision.PlanEarly.Decision.II,
#Decision.PlanEarly.Decision.I, Total.Event.Participation2.or.more
coeff_names <- rownames(coefficients_best_svm_linear)
coeff_abs <- abs(coefficients_best_svm_linear)
coeff_data <- data.frame(coeff_names, coeff_abs, row.names = NULL)
coeff_data[with(coeff_data, order(-coeff_abs)), ] 
#Test Kappa of the model 
svc_pred_test_best <- predict(best_svm_linear, Test)
svc_conting_test_best <- table(svc_pred_test_best, Test$Decision, 
                               dnn = c("Predicted", "Actual"))
svc_conting_test_best
svc_confu_test_best <- confusionMatrix(svc_conting_test_best)
svc_confu_test_best$overall["Kappa"]
#Kappa (SVM Linear) = 0.449827  

###SVM WITH POLYNOMIAL KERNEL
#Since R runs very slow for polynomial kernel, I choose to try each value of cost
#and degree to for the model. One thing I noticed was that when I tried the power of cost
#less than 0 (negative values), regardless of the degree, it will generate 
#approximately same cross validation errors, which are also pretty high (= 0.2192)
#Since it also takes time to run on each value, I will put some examples below.
#To make it convenient and to less-time consuming to run, I will only run the first
#example and put the rest in #.
set.seed(1)
svmp_best_coarse_n4_2 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                  data = Train, cost = 2^(-4), 
                                  degree = 2)
svmp_best_coarse_n4_2
#svmp_best_coarse_n3_2 <- tune.svm(x = Decision ~., kernel = "polynomial",
#                                  data = Train, cost = 2^(-3), 
#                                  degree = 2)
#svmp_best_coarse_n3_2
#svmp_best_coarse_n2_2 <- tune.svm(x = Decision ~., kernel = "polynomial",
#                                  data = Train, cost = 2^(-2), 
#                                  degree = 2)
#svmp_best_coarse_n2_2
#svmp_best_coarse_n1_2 <- tune.svm(x = Decision ~., kernel = "polynomial",
#                                 data = Train, cost = 2^(-1), 
#                                 degree = 2)
#svmp_best_coarse_n1_2
#svmp_best_coarse_n4_3 <- tune.svm(x = Decision ~., kernel = "polynomial",
#                                  data = Train, cost = 2^(-4), 
#                                  degree = 3)
#svmp_best_coarse_n4_3
#svmp_best_coarse_n4_4 <- tune.svm(x = Decision ~., kernel = "polynomial",
#                                  data = Train, cost = 2^(-4), 
#                                 degree = 4)
#svmp_best_coarse_n4_4
#svmp_best_coarse_n3_5 <- tune.svm(x = Decision ~., kernel = "polynomial",
#                                  data = Train, cost = 2^(-3), 
#                                  degree = 5)
#svmp_best_coarse_n3_5

#My choice for the cost range would be from 1 to 8
#and my choice for the degree range is from 2 to 5
#I will try each value of the power of cost with degree = 2 first
set.seed(1)
svmp_best_coarse_1_2 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(1), 
                                 degree = 2)
svmp_best_coarse_1_2
set.seed(1)
svmp_best_coarse_2_2 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(2), 
                                 degree = 2)
svmp_best_coarse_2_2
set.seed(1)
svmp_best_coarse_3_2 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(3), 
                                 degree = 2)
svmp_best_coarse_3_2
set.seed(1)
svmp_best_coarse_4_2 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(4), 
                                 degree = 2)
svmp_best_coarse_4_2
set.seed(1)
svmp_best_coarse_5_2 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(5), 
                                 degree = 2)
svmp_best_coarse_5_2
set.seed(1)
svmp_best_coarse_6_2 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(6), 
                                 degree = 2)
svmp_best_coarse_6_2
set.seed(1)
svmp_best_coarse_7_2 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(7), 
                                 degree = 2)
svmp_best_coarse_7_2
set.seed(1)
svmp_best_coarse_8_2 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(8), 
                                 degree = 2)
svmp_best_coarse_8_2
#Degree = 2, cost = 2^(1), corresponding cross validation error = 0.2186
#Degree = 2, cost = 2^(2), corresponding cross validation error = 0.1975
#Degree = 2, cost = 2^(3), corresponding cross validation error = 0.1855
#Degree = 2, cost = 2^(4), corresponding cross validation error = 0.1746
#Degree = 2, cost = 2^(5), corresponding cross validation error = 0.1647
#Degree = 2, cost = 2^(6), corresponding cross validation error = 0.1618
#Degree = 2, cost = 2^(7), corresponding cross validation error = 0.1606
#Degree = 2, cost = 2^(8), corresponding cross validation error = 0.1639
#Seemingly when degree = 2, cost = 2^(7) gives us the lowest cross validation error

#I will continue with other values of the degree, which includes 3, 4 and 5.
set.seed(1)
svmp_best_coarse_1_3 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(1), 
                                 degree = 3)
svmp_best_coarse_1_3
set.seed(1)
svmp_best_coarse_2_3 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(2), 
                                 degree = 3)
svmp_best_coarse_2_3
set.seed(1)
svmp_best_coarse_3_3 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(3), 
                                 degree = 3)
svmp_best_coarse_3_3
set.seed(1)
svmp_best_coarse_4_3 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(4), 
                                 degree = 3)
svmp_best_coarse_4_3
set.seed(1)
svmp_best_coarse_5_3 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(5), 
                                 degree = 3)
svmp_best_coarse_5_3
set.seed(1)
svmp_best_coarse_6_3 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(6), 
                                 degree = 3)
svmp_best_coarse_6_3
set.seed(1)
svmp_best_coarse_7_3 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(7), 
                                 degree = 3)
svmp_best_coarse_7_3
set.seed(1)
svmp_best_coarse_8_3 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(8), 
                                 degree = 3)
svmp_best_coarse_8_3
set.seed(1)
#Degree = 3, cost = 2^(1), corresponding cross validation error = 0.2192
#Degree = 3, cost = 2^(2), corresponding cross validation error = 0.2191
#Degree = 3, cost = 2^(3), corresponding cross validation error = 0.2185
#Degree = 3, cost = 2^(4), corresponding cross validation error = 0.214
#Degree = 3, cost = 2^(5), corresponding cross validation error = 0.1997
#Degree = 3, cost = 2^(6), corresponding cross validation error = 0.1863
#Degree = 3, cost = 2^(7), corresponding cross validation error = 0.1753
#Degree = 3, cost = 2^(8), corresponding cross validation error = 0.1655

set.seed(1)
svmp_best_coarse_1_4 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(1), 
                                 degree = 4)
svmp_best_coarse_1_4
set.seed(1)
svmp_best_coarse_2_4 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(2), 
                                 degree = 4)
svmp_best_coarse_2_4
set.seed(1)
svmp_best_coarse_3_4 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(3), 
                                 degree = 4)
svmp_best_coarse_3_4
set.seed(1)
svmp_best_coarse_4_4 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(4), 
                                 degree = 4)
svmp_best_coarse_4_4
set.seed(1)
svmp_best_coarse_5_4 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(5), 
                                 degree = 4)
svmp_best_coarse_5_4
set.seed(1)
svmp_best_coarse_6_4 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(6), 
                                 degree = 4)
svmp_best_coarse_6_4
set.seed(1)
svmp_best_coarse_7_4 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(7), 
                                 degree = 4)
svmp_best_coarse_7_4
set.seed(1)
svmp_best_coarse_8_4 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(8), 
                                 degree = 4)
svmp_best_coarse_8_4
set.seed(1)
#Degree = 4, cost = 2^(1), corresponding cross validation error = 0.2192
#Degree = 4, cost = 2^(2), corresponding cross validation error = 0.2192
#Degree = 4, cost = 2^(3), corresponding cross validation error = 0.2192
#Degree = 4, cost = 2^(4), corresponding cross validation error = 0.2189
#Degree = 4, cost = 2^(5), corresponding cross validation error = 0.2186
#Degree = 4, cost = 2^(6), corresponding cross validation error = 0.2174
#Degree = 4, cost = 2^(7), corresponding cross validation error = 0.2132
#Degree = 4, cost = 2^(8), corresponding cross validation error = 0.203

set.seed(1)
svmp_best_coarse_1_5 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(1), 
                                 degree = 5)
svmp_best_coarse_1_5
set.seed(1)
svmp_best_coarse_2_5 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(2), 
                                 degree = 5)
svmp_best_coarse_2_5
set.seed(1)
svmp_best_coarse_3_5 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(3), 
                                 degree = 5)
svmp_best_coarse_3_5
set.seed(1)
svmp_best_coarse_4_5 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(4), 
                                 degree = 5)
svmp_best_coarse_4_5
set.seed(1)
svmp_best_coarse_5_5 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(5), 
                                 degree = 5)
svmp_best_coarse_5_5
set.seed(1)
svmp_best_coarse_6_5 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(6), 
                                 degree = 5)
svmp_best_coarse_6_5
set.seed(1)
svmp_best_coarse_7_5 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(7), 
                                 degree = 5)
svmp_best_coarse_7_5
set.seed(1)
svmp_best_coarse_8_5 <- tune.svm(x = Decision ~., kernel = "polynomial",
                                 data = Train, cost = 2^(8), 
                                 degree = 5)
svmp_best_coarse_8_5
#Degree = 5, cost = 2^(1), corresponding cross validation error = 0.2192
#Degree = 5, cost = 2^(2), corresponding cross validation error = 0.2192
#Degree = 5, cost = 2^(3), corresponding cross validation error = 0.2192
#Degree = 5, cost = 2^(4), corresponding cross validation error = 0.2191
#Degree = 5, cost = 2^(5), corresponding cross validation error = 0.2191
#Degree = 5, cost = 2^(6), corresponding cross validation error = 0.2189
#Degree = 5, cost = 2^(7), corresponding cross validation error = 0.2186
#Degree = 5, cost = 2^(8), corresponding cross validation error = 0.2177

#Seemingly, degree = 2 and cost = 2^(7) are the most optimal values out of all
#values I have tried. Therefore, I will construct the model based on these values
best_svmp_coarse <- svmp_best_coarse_7_2$best.model
best_svmp_coarse
#Compute the coefficients of the best SVM (polynomial kernel) model.
coeff_best_svmp <- t(best_svmp_coarse$SV) %*% best_svmp_coarse$coefs
coeff_best_svmp
#Compute the intercept of the best SVM (polynomial kernel) model.
Intercept_best_svmp <- -best_svmp_coarse$rho
Intercept_best_svmp
#The 5 most important features are: 
#AthleteAthlete..Opt.Out, Decision.PlanEarly.Decision.I, Total.Event.Participation1,
#LegacyLegacy..Opt.Out, Total.Event.Participation2.or.more
coeff_names <- rownames(coeff_best_svmp)
coeff_abs <- abs(coeff_best_svmp)
coeff_data <- data.frame(coeff_names, coeff_abs, row.names = NULL)
coeff_data[with(coeff_data, order(-coeff_abs)), ] 
#Test Kappa of the model generated  
svmp_pred_test_best <- predict(best_svmp_coarse, Test)
svmp_conting_test_best <- table(svmp_pred_test_best, Test$Decision, 
                                dnn = c("Predicted", "Actual"))
svmp_conting_test_best
svmp_confu_test_best <- confusionMatrix(svmp_conting_test_best)
svmp_confu_test_best$overall["Kappa"]
#Kappa (SVM Polynomial) = 0.4671609 

###SVM WITH RADIAL KERNEL
#Again, this is a large dataset with 10,000 observations, which will make R
#run very slowly with SVM method. After careful consideration, I decided to narrow 
#the range of cost and gamma when searching for optimal values of them.
#I tried a few options before deciding the range of cost and gamma. To find the 
#optimal values, I decided the range of cost is from -2 to 3
#Similarly, the range of gamma is also from -2 to 3
#I will try with gamma = -2 and cost = -2:3 first
set.seed(1)
svmr_best_coarse_n2_n2 <- tune.svm(x = Decision ~., kernel = "radial",
                                   data = Train, cost = 2^(-2), 
                                   gamma = 2^(-2))
svmr_best_coarse_n2_n2
set.seed(1)
svmr_best_coarse_n1_n2 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(-1), 
                                  gamma = 2^(-2))
svmr_best_coarse_n1_n2
set.seed(1)
svmr_best_coarse_0_n2 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(0), 
                                  gamma = 2^(-2))
svmr_best_coarse_0_n2
set.seed(1)
svmr_best_coarse_1_n2 <- tune.svm(x = Decision ~., kernel = "radial",
                                   data = Train, cost = 2^(1), 
                                   gamma = 2^(-2))
svmr_best_coarse_1_n2
set.seed(1)
svmr_best_coarse_2_n2 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(2), 
                                  gamma = 2^(-2))
svmr_best_coarse_2_n2
set.seed(1)
svmr_best_coarse_3_n2 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(3), 
                                  gamma = 2^(-2))
svmr_best_coarse_3_n2
#Gamma = 2^(-2), cost = 2^(-2), corresponding cross validation error = 0.2192
#Gamma = 2^(-2), cost = 2^(-1), corresponding cross validation error = 0.2167
#Gamma = 2^(-2), cost = 2^(0), corresponding cross validation error = 0.197
#Gamma = 2^(-2), cost = 2^(1), corresponding cross validation error = 0.1872
#Gamma = 2^(-2), cost = 2^(2), corresponding cross validation error = 0.1862
#Gamma = 2^(-2), cost = 2^(3), corresponding cross validation error = 0.1863

#So far, Gamma = 2^(-2) and cost = 2^(2) yield the lowest validation error
#Let's check other options
set.seed(1)
svmr_best_coarse_n2_n1 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(-2), 
                                  gamma = 2^(-1))
svmr_best_coarse_n2_n1
set.seed(1)
svmr_best_coarse_n1_n1 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(-1), 
                                  gamma = 2^(-1))
svmr_best_coarse_n1_n1
set.seed(1)
svmr_best_coarse_0_n1 <- tune.svm(x = Decision ~., kernel = "radial",
                                   data = Train, cost = 2^(0), 
                                   gamma = 2^(-1))
svmr_best_coarse_0_n1
set.seed(1)
svmr_best_coarse_1_n1 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(1), 
                                  gamma = 2^(-1))
svmr_best_coarse_1_n1
set.seed(1)
svmr_best_coarse_2_n1 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(2), 
                                  gamma = 2^(-1))
svmr_best_coarse_2_n1
set.seed(1)
svmr_best_coarse_3_n1 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(3), 
                                  gamma = 2^(-1))
svmr_best_coarse_3_n1
set.seed(1)
svmr_best_coarse_4_n1 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(4), 
                                  gamma = 2^(-1))
svmr_best_coarse_4_n1
set.seed(1)
svmr_best_coarse_5_n1 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(5), 
                                  gamma = 2^(-1))
svmr_best_coarse_5_n1
#Gamma = 2^(-1), cost = 2^(-2), corresponding cross validation error = 0.2192
#Gamma = 2^(-1), cost = 2^(-2), corresponding cross validation error = 0.2192
#Gamma = 2^(-1), cost = 2^(0), corresponding cross validation error = 0.2192
#Gamma = 2^(-1), cost = 2^(1), corresponding cross validation error = 0.2192
#Gamma = 2^(-1), cost = 2^(2), corresponding cross validation error = 0.2192
#Gamma = 2^(-1), cost = 2^(3), corresponding cross validation error = 0.2192

set.seed(1)
svmr_best_coarse_n2_0 <- tune.svm(x = Decision ~., kernel = "radial",
                                   data = Train, cost = 2^(-2), 
                                   gamma = 2^(0))
svmr_best_coarse_n2_0
set.seed(1)
svmr_best_coarse_n1_0 <- tune.svm(x = Decision ~., kernel = "radial",
                                   data = Train, cost = 2^(-1), 
                                   gamma = 2^(0))
svmr_best_coarse_n1_0
set.seed(1)
svmr_best_coarse_0_0 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(0), 
                                  gamma = 2^(0))
svmr_best_coarse_0_0
set.seed(1)
svmr_best_coarse_1_0 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(1), 
                                  gamma = 2^(0))
svmr_best_coarse_1_0
set.seed(1)
svmr_best_coarse_2_0 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(2), 
                                  gamma = 2^(0))
svmr_best_coarse_2_0
set.seed(1)
svmr_best_coarse_3_0 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(3), 
                                  gamma = 2^(0))
svmr_best_coarse_3_0

set.seed(1)
svmr_best_coarse_n1_1 <- tune.svm(x = Decision ~., kernel = "radial",
                                 data = Train, cost = 2^(-1), 
                                 gamma = 2^(1))
svmr_best_coarse_n1_1
set.seed(1)
svmr_best_coarse_n2_1 <- tune.svm(x = Decision ~., kernel = "radial",
                                 data = Train, cost = 2^(-2), 
                                 gamma = 2^(1))
svmr_best_coarse_n2_1
set.seed(1)
svmr_best_coarse_0_1 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(0), 
                                  gamma = 2^(1))
svmr_best_coarse_0_1
set.seed(1)
svmr_best_coarse_1_1 <- tune.svm(x = Decision ~., kernel = "radial",
                                 data = Train, cost = 2^(1), 
                                 gamma = 2^(1))
svmr_best_coarse_1_1
set.seed(1)
svmr_best_coarse_2_1 <- tune.svm(x = Decision ~., kernel = "radial",
                                 data = Train, cost = 2^(2), 
                                 gamma = 2^(1))
svmr_best_coarse_2_1
set.seed(1)
svmr_best_coarse_3_1 <- tune.svm(x = Decision ~., kernel = "radial",
                                 data = Train, cost = 2^(3), 
                                 gamma = 2^(1))
svmr_best_coarse_3_1

set.seed(1)
svmr_best_coarse_n2_2 <- tune.svm(x = Decision ~., kernel = "radial",
                                 data = Train, cost = 2^(-2), 
                                 gamma = 2^(2))
svmr_best_coarse_n2_2
set.seed(1)
svmr_best_coarse_n1_2 <- tune.svm(x = Decision ~., kernel = "radial",
                                 data = Train, cost = 2^(-1), 
                                 gamma = 2^(2))
svmr_best_coarse_n1_2
set.seed(1)
svmr_best_coarse_0_2 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(0), 
                                  gamma = 2^(2))
svmr_best_coarse_0_2
set.seed(1)
svmr_best_coarse_1_2 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(1), 
                                  gamma = 2^(2))
svmr_best_coarse_1_2
set.seed(1)
svmr_best_coarse_2_2 <- tune.svm(x = Decision ~., kernel = "radial",
                                 data = Train, cost = 2^(2), 
                                 gamma = 2^(2))
svmr_best_coarse_2_2
set.seed(1)
svmr_best_coarse_3_2 <- tune.svm(x = Decision ~., kernel = "radial",
                                 data = Train, cost = 2^(3), 
                                 gamma = 2^(2))
svmr_best_coarse_3_2

set.seed(1)
svmr_best_coarse_n2_3 <- tune.svm(x = Decision ~., kernel = "radial",
                                 data = Train, cost = 2^(-2), 
                                 gamma = 2^(3))
svmr_best_coarse_n2_3

set.seed(1)
svmr_best_coarse_n1_3 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(-1), 
                                  gamma = 2^(3))
svmr_best_coarse_n1_3
set.seed(1)
svmr_best_coarse_0_3 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(0), 
                                  gamma = 2^(3))
svmr_best_coarse_0_3
set.seed(1)
svmr_best_coarse_1_3 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(1), 
                                  gamma = 2^(3))
svmr_best_coarse_1_3
set.seed(1)
svmr_best_coarse_2_3 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(2), 
                                  gamma = 2^(3))
svmr_best_coarse_2_3
set.seed(1)
svmr_best_coarse_3_3 <- tune.svm(x = Decision ~., kernel = "radial",
                                  data = Train, cost = 2^(3), 
                                  gamma = 2^(3))
svmr_best_coarse_3_3

#Gamma = 2^(0), cost = 2^(-2:3), all yield the same cross validation error = 0.2192
#Gamma = 2^(1), cost = 2^(-2:3), all yield the same cross validation error = 0.2192
#Gamma = 2^(2), cost = 2^(-2:3), all yield the same cross validation error = 0.2192
#Gamma = 2^(3), cost = 2^(-2:3), all yield the same cross validation error = 0.2192

#Hence, out of all the options I have tried, gamma = 2^(-2), cost = 2^(2) seem to be
#the most optimal values

#Construct the model based on the optimal cost and gamma
best_svmr_coarse <- svmr_best_coarse_2_n2$best.model
best_svmr_coarse
#Training Kappa of the best SVM (radial kernel) model
svmr_pred_train_best <- predict(best_svmr_coarse, Train)
svmr_conting_train_best <- table(svmr_pred_train_best, Train$Decision, 
                                 dnn = c("Predicted", "Actual"))
svmr_conting_train_best
svmr_confu_train_best <- confusionMatrix(svmr_conting_train_best)
svmr_confu_train_best$overall["Kappa"]
#Training Kappa = 1
#Test Kappa of the best SVM (radial kernel) model
svmr_pred_test_best <- predict(best_svmr_coarse, Test)
svmr_conting_test_best <- table(svmr_pred_test_best, Test$Decision, 
                                dnn = c("Predicted", "Actual"))
svmr_conting_test_best
svmr_confu_test_best <- confusionMatrix(svmr_conting_test_best)
svmr_confu_test_best$overall["Kappa"]
#Kappa (SVM Radial) = 0.3288783  
#Test Kappa is lower than Training Kappa, so seemingly this model is overfitted,
#and is not a good model

#REPORT ALL KAPPA SCORES
#Kappa (Logistic Regression) = 0.5350381
#Kappa (knn) = 0.1077172
#Kappa (Simple Classification Tree) = 0.3849844  
#Kappa (Pruned Tree) = 0.4192191
#Kappa (Bagging) = 0.4740288 
#Kappa (Random Forest) = 0.4991938
#Kappa (Boosting) = 0.5384955 
#Kappa (SVM Linear) = 0.449827  
#Kappa (SVM Polynomial) = 0.4671609 
#Kappa (SVM Radial) = 0.3288783  