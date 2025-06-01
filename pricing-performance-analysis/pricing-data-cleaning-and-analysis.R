library(dplyr)
library(lubridate)
library(readxl)
library(tidyr)

#PULLTHW
#Read PullThw
PullThw <- read.csv("PullThw.csv")
#Remove redundant columns
PullThw <- subset(PullThw, select = -c(PullThwID, ProdStoreid, 
                                       Userid, Dept, Category1, 
                                       Category2, DateLoaded))
#10 variables left
#Remove the off observation (Qty ~ 9m)
PullThw <- PullThw[-39957,]
#Remove the off observation (IfSoldValue ~ 14000, Goodwill also confirmed)
PullThw <- PullThw[PullThw$IfSoldValue != 14099, ]

#Calculate AvgDur
PullThw$AvgDur <- round(PullThw$TTLDur/PullThw$Qty,2)
PullThw <- subset(PullThw, select = -c(TTLDur))

#Filter out empty ("Unknown") ProdEmpid
PullThw <- PullThw %>% filter(!(PullThw$ProdEmpid == ""))

#Check if there are NAs
summary(PullThw)


#Remove negative durations
PullThw <- PullThw %>% filter(!(PullThw$AvgDur < 0))

#Check again if there are any blank cells or NAs
sum(is.na(PullThw))
colSums(PullThw == "")
#Looks good

#PricePoint variable
#From Majorie's answer, for open PP (PricePoint = 0), it was special prices
#and the IfSoldValue was also the PricePoint that the employee set
#Impute 0 values with the IfSoldValue
#Because IfSoldValue is the actual revenue (Qty*Price)
#PricePoint = IfSoldValue/Qty
PullThw$PricePoint[PullThw$PricePoint == 0] <- round(with(PullThw, IfSoldValue[PricePoint == 0] / Qty[PricePoint == 0]),2)

#SKU variable
summary(factor(PullThw$SKU))
#Regroup 
PullThw$SKU <- ifelse(grepl("Easter|EASTER|EasterBasket", PullThw$SKU), "Seasonal",
                      ifelse(grepl("Stuffed Animals|bin|gaylord", PullThw$SKU), "Wares", PullThw$SKU))

#Remove PPtype
PullThw <- subset(PullThw, select = -c(PPtype))
#Change data types 
PullThw$PullStoreid <- as.factor(PullThw$PullStoreid)
PullThw$ProdEmpid <- as.factor(PullThw$ProdEmpid)
PullThw$TransType <- as.factor(PullThw$TransType)
PullThw$DatePull <- as.Date(PullThw$DatePull)
PullThw$SKU <- as.factor(PullThw$SKU)
#PullThw$PricePoint <- as.numeric(PullThw$PricePoint)
summary(PullThw)
#Aggregate data
PullThw <- PullThw %>%
  filter(ProdEmpid != "Unknown") %>%
  group_by(PullStoreid, ProdEmpid, TransType, DatePull, SKU, PricePoint) %>%
  summarise(
    TotalQty = sum(Qty),
    AvgIfSoldValue = mean(IfSoldValue),
    AvgDur = mean(AvgDur)
  )
#Change data types 
PullThw$PullStoreid <- as.factor(PullThw$PullStoreid)
PullThw$ProdEmpid <- as.factor(PullThw$ProdEmpid)
PullThw$TransType <- as.factor(PullThw$TransType)
PullThw$DatePull <- as.Date(PullThw$DatePull)
PullThw$SKU <- as.factor(PullThw$SKU)
#PullThw$PricePoint <- as.numeric(PullThw$PricePoint)



#SELLTHW 
#Read SellThw
SellThw <- read.csv("SellThw.csv")
SellThw <- subset(SellThw, select = -c(sellThwID, Userid, OrgStoreid, 
                                       Category1, Category2, DateLoaded))

#Calculate AvgDur
SellThw$AvgDur <- round(SellThw$TTLDur/SellThw$Qty,2)
SellThw <- subset(SellThw, select = -c(TTLDur))
#Filter out empty ("Unknown") ProdEmpid
SellThw <- SellThw %>% filter(!(SellThw$ProdEmpid == ""))
#Fix a mismatch
SellThw$Storeid[SellThw$ProdEmpid == 1580000016 & SellThw$Storeid == 160] <- 158 #There's a mismatch for that row maybe due to their mistake
#Check for NAs
summary(factor(SellThw$SKU))
#1794 NAs. Not a lot compared to 1mil observations. Impute them with mean 
SellThw$AvgDur[is.na(SellThw$AvgDur)] <- mean(SellThw$AvgDur, na.rm = TRUE)

#Similar to PullThw, just a quick reminder to include explanation on why
#we're doing so here in the report :)
summary(factor(SellThw$SKU))
#SKU variable
SellThw$SKU <- ifelse(grepl("Easter|EASTER|EasterBasket", SellThw$SKU), "Seasonal",
                      ifelse(grepl("bike", SellThw$SKU), "Large Wares",
                             ifelse(grepl("Silverware|Stuffed Animals|bin|gaylord|NCWares", SellThw$SKU), "Wares", SellThw$SKU)))

#"Open" in PPType all have 0 in PricePoint column. No need to use PPType
if (all(SellThw$PricePoint[SellThw$PPtype == "Open"] == 0)) {
  print("Yes")
} else {
  print("No")
}
SellThw <- subset(SellThw, select = -c(PPtype))

#Change data types
SellThw$Storeid <- as.factor(SellThw$Storeid)
SellThw$ProdEmpid <- as.factor(SellThw$ProdEmpid)
SellThw$DateSold <- as.Date(SellThw$DateSold)
SellThw$SKU <- as.factor(SellThw$SKU)
#SellThw$PricePoint <- as.numeric(SellThw$PricePoint)
#Aggregate data
SellThw <- SellThw %>%
  filter(ProdEmpid != "Return") %>%
  group_by(Storeid, ProdEmpid, DateSold, SKU, PricePoint) %>%
  summarise(
    TotalQty = sum(Qty),
    AvgSoldValue = round(mean(SoldValue),2),
    AvgDur = round(mean(AvgDur),2)
  )


#Change data types
SellThw$Storeid <- as.factor(SellThw$Storeid)
SellThw$ProdEmpid <- as.factor(SellThw$ProdEmpid)
SellThw$DateSold <- as.Date(SellThw$DateSold)
SellThw$SKU <- as.factor(SellThw$SKU)
#SellThw$PricePoint <- as.numeric(SellThw$PricePoint)

#Pricing Effectiveness
SellThw$IfRevenue <- ifelse(SellThw$PricePoint != 0, SellThw$PricePoint * SellThw$TotalQty, NA)
summary(SellThw$IfRevenue)
#331187 NAs because they are Open PricePoint (PricePoint = 0)
options(scipen = 999)

SellThw <- SellThw %>% filter(!is.na(IfRevenue))
#Calculate Pricing Effectiveness
SellThw$PricingEffectiveness <- ifelse(SellThw$PricePoint != 0, round((SellThw$AvgSoldValue/SellThw$IfRevenue) * 100,2), NA)
#Make the outlier with 127% to 100%
SellThw$PricingEffectiveness[SellThw$PricingEffectiveness == 127.51] <- 100



#NEW TABBLE - EMP PRICING VARIANCE 
#EmpPricingVariance <- SellThw %>%
#  group_by(Storeid, ProdEmpid, SKU) %>%
#  summarise(Pricing_Variance = sd(PricePoint, na.rm = TRUE))
ProductionSkuAgent2 <- read.csv("ProductionSkuAgent.csv")
ProductionSkuAgent2$AvgProdvalue <- ProductionSkuAgent2$Prodvalue/ProductionSkuAgent2$ProdAmt
EmpPricingVariance <- ProductionSkuAgent2 %>%
  group_by(OrgStoreid, Emplid, ProdDate, sku) %>%
  summarise(Pricing_Variance = sd(AvgProdvalue, na.rm = TRUE),
            TotalSold = sum(ProdAmt))

summary(EmpPricingVariance)
#Remove NAs (which means the employee only sets 1 price for stuff in that SKU,
#which technically means they don't have pricing variance)
EmpPricingVariance <- EmpPricingVariance %>% filter(!(is.na(Pricing_Variance)))
EmpPricingVariance$Pricing_Variance <- as.numeric(EmpPricingVariance$Pricing_Variance)


#PRODUCTIONSKUAGENT
#Read ProductionSkuAgent 
ProductionSkuAgent <- read.csv("ProductionSkuAgent.csv")
ProductionSkuAgent <- subset(ProductionSkuAgent, select = -c(PPSKuAgnt, OrigProdDate, glacct, 
                                                             deptid, CategoryId1, CategoryId2, 
                                                             DateLoaded, Dateupdated))
summary(ProductionSkuAgent)
ProductionSkuAgent <- ProductionSkuAgent %>%
  select(-year, -month)
#Change data type
summary(ProductionSkuAgent)
ProductionSkuAgent$ProdDate <- as.Date(ProductionSkuAgent$ProdDate)
ProductionSkuAgent$Emplid <- as.factor(ProductionSkuAgent$Emplid)
ProductionSkuAgent$OrgStoreid <- as.factor(ProductionSkuAgent$OrgStoreid)

#SKU variable
ProductionSkuAgent$sku <- ifelse(grepl("Easter|EASTER|EasterBasket", ProductionSkuAgent$sku), "Seasonal",
                                 ifelse(grepl("bike", ProductionSkuAgent$sku), "Large Wares",
                                        ifelse(grepl("Silverware|tote|Stuffed Animals|bin|gaylord|NCWares", ProductionSkuAgent$sku), "Wares", ProductionSkuAgent$sku)))
summary(factor(ProductionSkuAgent$sku))
summary(factor(SellThw$SKU))
summary(factor(PullThw$SKU))

#Aggregate data
ProductionSkuAgent <- ProductionSkuAgent %>%
  group_by(ProdDate, Emplid, OrgStoreid, sku) %>%
  summarise(
    TotalProdAmt = sum(ProdAmt),
    AvgProdValue = mean(Prodvalue),
    TotalPullAmount = sum(PullAmt),
    TotalSoldAmt = sum(SoldAmt)
  )
#Remove the unrealistic amount of production
ProductionSkuAgent <- ProductionSkuAgent %>%
  filter(TotalProdAmt != 33401.00)
#Change data type
ProductionSkuAgent$ProdDate <- as.Date(ProductionSkuAgent$ProdDate)
ProductionSkuAgent$Emplid <- as.factor(ProductionSkuAgent$Emplid)
ProductionSkuAgent$OrgStoreid <- as.factor(ProductionSkuAgent$OrgStoreid)

#EMPLOYEE AND STORE
#Read Employee and Store
Employee <- read.csv("Employee.csv")
Store <- read_excel("Store.xls")
#Concatenate FirstName and LastName to create new variable called FullName
Employee$FullName <- paste(Employee$`FirstName`, Employee$`LastName`, sep = " ")
#Join two dfs
Employee_Store <- Employee %>%
  left_join(Store %>% mutate(StoreId = as.integer(Storeid)), by = "StoreId")



#Join Employee_Store to other tables to have Employee names and Store names
#PullThw

PullThw <- left_join(PullThw, Employee_Store %>% select(EmpId, Storeid, FullName), 
                     by = c("ProdEmpid" = "EmpId", "PullStoreid" = "Storeid"))
PullThw <- left_join(PullThw, Store %>% select(Storeid, StoreName), 
                     by = c("PullStoreid" = "Storeid"))
PullThw$FullName[PullThw$ProdEmpid == "OFLE1234"] <- "System Login"
#Deal with (not) Nathan (not) Tindel
PullThw <- PullThw %>%
  mutate(
    FullName = if_else(FullName == "(not) Nathan (not) Tindel", "Nathan Tindel", FullName),
    ProdEmpid = if_else(ProdEmpid == "1460000052", "1460000080", ProdEmpid)
  )

#SellThw

SellThw <- left_join(SellThw, Employee_Store %>% select(EmpId, Storeid, FullName), 
                     by = c("ProdEmpid" = "EmpId", "Storeid" = "Storeid"))
SellThw <- left_join(SellThw, Store %>% select(Storeid, StoreName), 
                     by = c("Storeid" = "Storeid"))
SellThw$FullName[SellThw$ProdEmpid == "OFLE1234"] <- "System Login"
SellThw <- SellThw %>%
  mutate(
    FullName = if_else(FullName == "(not) Nathan (not) Tindel", "Nathan Tindel", FullName),
    ProdEmpid = if_else(ProdEmpid == "1460000052", "1460000080", ProdEmpid)
  )
SellThw$FullName[is.na(SellThw$FullName)] <- "Nathan Tindel"


#ProductionSkuAgent

ProductionSkuAgent <- left_join(ProductionSkuAgent, Employee_Store %>% select(EmpId, Storeid, FullName), 
                                by = c("Emplid" = "EmpId", "OrgStoreid" = "Storeid"))
ProductionSkuAgent <- left_join(ProductionSkuAgent, Store %>% select(Storeid, StoreName), 
                                by = c("OrgStoreid" = "Storeid"))
ProductionSkuAgent$FullName[ProductionSkuAgent$Emplid == "OFLE1234"] <- "System Login"
ProductionSkuAgent <- ProductionSkuAgent %>%
  mutate(
    FullName = if_else(FullName == "(not) Nathan (not) Tindel", "Nathan Tindel", FullName),
    ProdEmpid = if_else(Emplid == "1460000052", "1460000080", Emplid)
  )


#EmpPricingVariance
EmpPricingVariance$OrgStoreid <- as.factor(EmpPricingVariance$OrgStoreid)
EmpPricingVariance <- left_join(EmpPricingVariance, Employee_Store %>% select(EmpId, Storeid, FullName), 
                                by = c("Emplid" = "EmpId", "OrgStoreid" = "Storeid"))
EmpPricingVariance <- left_join(EmpPricingVariance, Store %>% select(Storeid, StoreName), 
                                by = c("OrgStoreid" = "Storeid"))
EmpPricingVariance$FullName[EmpPricingVariance$Emplid == "OFLE1234"] <- "System Login"


#####################################################################
summary(EmpPricingVariance)
#These are the tables we gonna use
sum(is.na(PullThw))
sum(is.na(SellThw))
summary(SellThw)
sum(is.na(ProductionSkuAgent))
sum(is.na(EmpPricingVariance))

#write.csv(PullThw, "cleaned_PullThw.csv", row.names = FALSE)
#write.csv(SellThw, "cleaned_SellThw.csv", row.names = FALSE)
#write.csv(ProductionSkuAgent, "cleaned_ProductionSkuAgent.csv", row.names = FALSE)
#write.csv(EmpPricingVariance, "cleaned_EmpPricingVariance.csv", row.names = FALSE)
