library(Hmisc)
library(readr)
library(dplyr)
library(sqldf)
library(tidyr)
library(zoo)
train <- read_csv('C:/Users/harini/Downloads/train.csv (1)/train.csv')
#describe(train)

# train 
# 
# 9  Variables      878049  Observations
# --------------------------------------------------------------------------------------
#   Dates 
# n             missing              unique                Info 
# 878049                   0              389257                   1 
# Mean                 .05                 .10                 .25 
# 2009-03-16 08:25:41 2003-08-05 19:00:00 2004-03-05 23:00:00 2006-01-11 03:00:00 
# .50                 .75                 .90                 .95 
# 2009-03-07 16:00:00 2012-06-11 10:13:00 2014-03-17 13:02:36 2014-10-14 14:00:00 
# 
# lowest : 2003-01-06 00:01:00 2003-01-06 00:15:00 2003-01-06 00:20:00 2003-01-06 00:31:00 2003-01-06 00:33:00
# highest: 2015-05-13 22:58:00 2015-05-13 23:00:00 2015-05-13 23:30:00 2015-05-13 23:33:00 2015-05-13 23:53:00 
# --------------------------------------------------------------------------------------
#   Category 
# n missing  unique 
# 878049       0      39 
# 
# lowest : ARSON         ASSAULT       BAD CHECKS    BRIBERY       BURGLARY     
# highest: TRESPASS      VANDALISM     VEHICLE THEFT WARRANTS      WEAPON LAWS   
# --------------------------------------------------------------------------------------
#   Descript 
# n missing  unique 
# 878049       0     879 
# 
# lowest : ABANDONMENT OF CHILD                           ABORTION                                       ACCESS CARD INFORMATION, PUBLICATION OF        ACCESS CARD INFORMATION, THEFT OF              ACCIDENTAL BURNS                              
# highest: WEAPONS POSSESSION BY JUVENILE SUSPECT         WEARING MASK OR DISGUISE FOR UNLAWFUL PURPOSE  WEARING THE APPAREL OF OPPOSITE SEX TO DECEIVE WILLFUL CRUELTY TO CHILD                       YOUTH COURT                                    
# --------------------------------------------------------------------------------------
#   DayOfWeek 
# n missing  unique 
# 878049       0       7 
# 
# Friday Monday Saturday Sunday Thursday Tuesday Wednesday
# Frequency 133734 121584   126810 116707   125038  124965    129211
# %             15     14       14     13       14      14        15
# --------------------------------------------------------------------------------------
#   PdDistrict 
# n missing  unique 
# 878049       0      10 
# 
# BAYVIEW CENTRAL INGLESIDE MISSION NORTHERN  PARK RICHMOND SOUTHERN TARAVAL
# Frequency   89431   85460     78845  119908   105296 49313    45209   157182   65596
# %              10      10         9      14       12     6        5       18       7
# TENDERLOIN
# Frequency      81809
# %                  9
# --------------------------------------------------------------------------------------
#   Resolution 
# n missing  unique 
# 878049       0      17 
# 
# ARREST, BOOKED (206403, 24%), ARREST, CITED (77004, 9%) 
# CLEARED-CONTACT JUVENILE FOR MORE INFO (217, 0%) 
# COMPLAINANT REFUSES TO PROSECUTE (3976, 0%) 
# DISTRICT ATTORNEY REFUSES TO PROSECUTE (3934, 0%) 
# EXCEPTIONAL CLEARANCE (1530, 0%) 
# JUVENILE ADMONISHED (1455, 0%) 
# JUVENILE BOOKED (5564, 1%), JUVENILE CITED (3332, 0%) 
# JUVENILE DIVERTED (355, 0%), LOCATED (17101, 2%) 
# NONE (526790, 60%), NOT PROSECUTED (3714, 0%) 
# PROSECUTED BY OUTSIDE AGENCY (2504, 0%) 
# PROSECUTED FOR LESSER OFFENSE (51, 0%) 
# PSYCHOPATHIC CASE (14534, 2%), UNFOUNDED (9585, 1%) 
# --------------------------------------------------------------------------------------
#   Address 
# n missing  unique 
# 878049       0   23228 
# 
# lowest : 0 Block of  HARRISON ST 0 Block of 10TH AV      0 Block of 10TH ST      0 Block of 11TH ST      0 Block of 12TH AV     
# highest: ZENO PL / FOLSOM ST     ZOE ST / BRANNAN ST     ZOE ST / BRYANT ST      ZOE ST / FREELON ST     ZOE ST / WELSH ST       
# --------------------------------------------------------------------------------------
#   X 
# n missing  unique    Info    Mean     .05     .10     .25     .50     .75 
# 878049       0   34243       1  -122.4  -122.5  -122.5  -122.4  -122.4  -122.4 
# .90     .95 
# -122.4  -122.4 
# 
# lowest : -122.5 -122.5 -122.5 -122.5 -122.5
# highest: -122.4 -122.4 -122.4 -122.4 -120.5 
# --------------------------------------------------------------------------------------
#   Y 
# n missing  unique    Info    Mean     .05     .10     .25     .50     .75 
# 878049       0   34243       1   37.77   37.72   37.73   37.75   37.78   37.78 
# .90     .95 
# 37.79   37.80 
# 
# lowest : 37.71 37.71 37.71 37.71 37.71, highest: 37.82 37.82 37.82 37.82 90.00 
# --------------------------------------------------------------------------------------

#------------------------------------Data Cleaning--------------------------------
#------------------------------------Remove duplicate records------------------------  
duplicated(train) #gives TRUE for duplicated rows
duplicate <- data.frame(duplicated(train) %in% "TRUE")
count(duplicate)
summary(duplicate)
summary(train[!duplicated(train),])
train_new <- data.frame()
train %>% distinct
describe(train)
train_new <- unique(train) #--> Method 1 #train_new has unique rws of train
train_new = train %>% distinct  #--> Method 2
summary(train_new) #train_new has unique rws of train
train_new_1 = sqldf('SELECT DISTINCT * FROM train' ) #--> Method 3
sum(complete.cases(train_new_1))

# ----------------------------remove lines with " %"-------------------
# I <- grepl("^%", train_new)
train <- fill(train,train_new$Category) #,
fill(train_new$Descript,train_new$DayOfWeek ,train_new$PdDistrict)
fill(train_new$Resolution )#,train_new$Address,	train_new$X,	train_new$Y)

library(plyr)
dfi <- ddply(train_new,c("Category","Resolution","Descript","DayOfWeek","PdDistrict"), function(insp){sum(is.na(insp)) > 0} )
#sum(is.na(train_new$Resolution)
#------outlier detection------
boxplot.stats(train,train_new$Category$X, coef = 2)$out

# library(plyr)
# Hist <- count(train_new , 'Category')
# Hist<-table(Hist)
# barplot(Hist$freq,Hist$Category)
# Hist


category <- as.data.frame(table(train_new$Category))
names(category) <- c("Category", "Frequency")
attach(category)
nd <- category[order(-Frequency),]
barplot(nd$Frequency, names.arg=nd$Category, las=2, cex.names=0.8,main="Number of incidents per Category",ylim=c(0,200000))

train_new$Dates <- as.Date(train_new$Dates)
test$Dates  <- as.Date(test$Dates)
train_new$Time <- format(as.POSIXct(train_new$Dates, format="%Y-%m-%d %H:%M:%S"), format="%H:%M:%S")
test$Time  <- format(as.POSIXct(test$Dates, format="%Y-%m-%d %H:%M:%S"), format="%H:%M:%S")

dayofweek <- as.data.frame(table(train_new$DayOfWeek))
names(dayofweek) <- c("DayOfWeek", "Frequency")
attach(dayofweek)
nd <- dayofweek[order(-Frequency),]
barplot(nd$Frequency, names.arg=nd$DayOfWeek, las=2, cex.names=0.8,main="Number of incidents per weekday in the training set",ylim=c(0,140000))


pddistrict <- as.data.frame(table(train_new$PdDistrict))
names(pddistrict) <- c("PdDistrict", "Frequency")
attach(pddistrict)
nd <- pddistrict[order(-Frequency),]
barplot(nd$Frequency, names.arg=nd$PdDistrict, las=2, cex.names=0.8,main="Number of incidents per PdDistrict in the training set",ylim=c(0,200000))
test=read_csv('C:/Users/harini/Downloads/crime/test.csv/test.csv')

library(data.table)
train_model = data.table(category_predict = train_new$Category, x = train_new$X, y = train_new$X, Dates =train_new$Dates , time =train_new$Time)
setnames(train_model, names(train_model), c('category_predict', 'X', 'Y', 'Dates','time'))
test_model = data.table(x = test$X, y = test$X, Dates =test$Dates , time =test$Time)

head(test_model)




model = train_new$Category ~ train_new$X + train_new$Y + train_new$Dates + train_new$Time + train_new$PdDistrict +train_new$DayOfWeek
library(kknn)
subset = train_model[c(1:30000, 30024, 33954, 37298, 41097, 41980, 44479, 48707, 48837, 49306, 53715, 93717, 102637, 102645, 102678, 102919, 103517, 103712, 107734, 148476, 148476, 192191, 205046, 252094, 279792, 316491, 317527, 332821, 337881)]

knn_train = kknn(formula = model, train = subset, test=test_model, scale = T)
