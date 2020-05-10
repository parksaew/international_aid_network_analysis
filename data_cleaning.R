pacman::p_load(dplyr,
               plyr,
               haven,
               corrplot,
               stargazer,
               readr,
               ggplot2,
               texreg,
               psych,
               countrycode,
               readxl,
               multiwayvcov,
               lmtest,
               plm,
               igraph,
               texreg,
               gdata)



##2016
crs2016 <- read.csv("CRS 2016 data.txt", row.names=NULL, sep="|")

#checking to see that everything works
length(filter(crs2016, SectorCode == 150))
table(crs2016$Year)


##2015
crs2015 <- read.csv("CRS 2015 data.txt", row.names=NULL, sep="|")

##2014
crs2014 <- read.csv("CRS 2014 data.txt", row.names=NULL, sep="|")

##2013
crs2013 <- read.csv("CRS 2013 data.txt", row.names=NULL, sep="|")

##2012
crs2012 <- read.csv("CRS 2012 data.txt", row.names=NULL, sep="|")

##2011
crs2011 <- read.csv("CRS 2011 data.txt", row.names=NULL, sep="|")

##2010
crs2010 <- read.csv("CRS 2010 data.txt", row.names=NULL, sep="|")

##2009
crs2009 <- read.csv("CRS 2009 data.txt", row.names=NULL, sep="|")

##2008
crs2008 <- read.csv("CRS 2008 data.txt", row.names=NULL, sep="|")

##2007
crs2007 <- read.csv("CRS 2007 data.txt", row.names=NULL, sep="|")

##2006
crs2006 <- read.csv("CRS 2006 data.txt", row.names=NULL, sep="|")

##2004-5
crs2004_05 <- read.csv("CRS 2004-05 data.txt", row.names=NULL, sep="|")

##2002-3
crs2002_03 <- read.csv("CRS 2002-03 data.txt", row.names=NULL, sep="|")

##2000-1
crs2000_01 <- read.csv("CRS 2000-01 data.txt", row.names=NULL, sep="|")

##1995-1999
crs1999_95 <- read.csv("CRS 1995-99 data.txt", row.names=NULL, sep="|")

##1973-1994
crs1973_94 <- read.csv("CRS 1973-94 data.txt", row.names=NULL, sep="|")





#1946-2014: dyad data (has ideal points and affinity data (s2un, s3un)- refer to code book)
  #absidealdiff in dyad data contains the absolute distance between country 1 and country 2 posterior mean ideal point estimates.
  #uses COW codes
dyaddata <- read.table(
  "Dyadicdata.tab",
  sep="\t", header=TRUE)

#1946-2015: ideal points data - refer to code book
idealpoints <- read.table(
  "IdealpointsPublished.tab",
  sep="\t", header=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#let's try 2005 (and 2004 for reverse causality) - from my intuition, it seems that this would be least affected by 9/11 and the 2007 financial crisis
dyaddata2005 <- filter(dyaddata, year == 2005)
crs2005 <- filter(crs2004_05, Year == 2005)

crs2004 <- filter(crs2004_05, Year == 2004)


#### change country name to cowcode

#get a vector of the countries (character) that are included in dyaddata
  #we do this because OECD has their own country code 
  #we will try to see which countries are included in CRS but not dyaddata

countries_cow <- unique(c(dyaddata[,"ccode1"], dyaddata[,"ccode2"]))
countries_cow <- countries_cow[!countries_cow == ""]
countries_cow <- countries_cow[!is.na(countries_cow)]


countries_chr <- countrycode(countries_cow, "cown", "country.name", warn = TRUE)

##just using countrycode() actually worked nicely
  #donor

countries_cow_crsdonor <- unique(countrycode(crs2005$DonorName, "country.name", "cown", warn = TRUE ))
countries_cow_crsdonor <- countries_cow_crsdonor[!countries_cow_crsdonor == ""]
countries_cow_crsdonor <- countries_cow_crsdonor[!is.na(countries_cow_crsdonor)]

crs2005$ccodedonor <- countrycode(crs2005$DonorName, "country.name", "cown", warn = TRUE )
crs2004$ccodedonor <- countrycode(crs2004$DonorName, "country.name", "cown", warn = TRUE )


  #recipient
countries_cow_crsrecipient <- unique(countrycode(crs2005$RecipientName, "country.name", "cown", warn = TRUE ))
countries_cow_crsrecipient <- countries_cow_crsrecipient[!countries_cow_crsrecipient == ""]
countries_cow_crsrecipient <- countries_cow_crsrecipient[!is.na(countries_cow_crsrecipient)]

crs2005$ccoderecipient <- countrycode(crs2005$RecipientName, "country.name", "cown", warn = TRUE )
crs2004$ccoderecipient <- countrycode(crs2004$RecipientName, "country.name", "cown", warn = TRUE )
#we need to code Serbia separately, because cow code uses the same code for the former Yugoslavia
crs2005$ccoderecipient[crs2005$RecipientName == "Serbia"] <- 345 
crs2004$ccoderecipient[crs2004$RecipientName == "Serbia"] <- 345 

##let's make sure that only the non-countries were removed
  #donor
as.vector(unique(filter(crs2005, is.na(ccodedonor))$DonorName))

  #recipient
as.vector(unique(filter(crs2005, is.na(ccoderecipient))$RecipientName))
#looks good


####now lets clean the crs data for 2005 and 2004
  #info about CRS data: http://www.oecd.org/dac/stats/crsguide.htm

#filter just democracy aid (sector code 151 for "government and civil society- general)
  #none for 150 ("government and civil society")
crs2005_clean <- filter(crs2005, crs2005$SectorCode == 151)
crs2004_clean <- filter(crs2004, crs2004$SectorCode == 151)

#filter out all the non-countries
crs2005_clean <- filter(crs2005_clean, !is.na(ccodedonor) & !is.na(ccoderecipient))
crs2004_clean <- filter(crs2004_clean, !is.na(ccodedonor) & !is.na(ccoderecipient))

##remove columns we don't need
#for the aid data, just get the constant USD disbursement
crs2005_clean <- dplyr::select(crs2005_clean, Year, ccodedonor, ccoderecipient, usd_disbursement)
crs2004_clean <- dplyr::select(crs2004_clean, Year, ccodedonor, ccoderecipient, usd_disbursement)

crs2005_clean <- plyr::rename(crs2005_clean, c("Year"="year", 
                                         "ccodedonor" = "ccode1",
                                         "ccoderecipient" = "ccode2",
                                         "usd_disbursement" = "aidflow"))
crs2004_clean <- plyr::rename(crs2004_clean, c("Year"="year", 
                                               "ccodedonor" = "ccode1",
                                               "ccoderecipient" = "ccode2",
                                               "usd_disbursement" = "aidflow04"))





##### I need to collapse all the same dyads together #####
crs2005collapsed <- filter(crs2005_clean, !is.na(aidflow)) %>% 
  group_by(ccode1, ccode2) %>%
  dplyr::summarise(aidflow = sum(aidflow))
crs2004collapsed <- filter(crs2004_clean, !is.na(aidflow04)) %>% 
  group_by(ccode1, ccode2) %>%
  dplyr::summarise(aidflow04 = sum(aidflow04))


####let's also clean the dyad data a bit too
dyad2005clean <- dplyr::select(dyaddata2005, ccode1, ccode2, absidealdiff, s3un) %>%
  plyr::rename(c("absidealdiff"="id2005", "s3un" = "s3un2005"))

##I will also look at 2000-2007 data to try lags, first difference, and reverse causality
#2000 because Savun used 5th lags

dyad2000 <- filter(dyaddata, year == 2000) %>%
  dplyr::select(ccode1, ccode2, absidealdiff, s3un) %>%
  plyr::rename(c("absidealdiff"="id2000", "s3un" = "s3un2000"))

dyad2001 <- filter(dyaddata, year == 2001) %>%
  dplyr::select(ccode1, ccode2, absidealdiff, s3un) %>%
  plyr::rename(c("absidealdiff"="id2001", "s3un" = "s3un2001"))

dyad2002 <- filter(dyaddata, year == 2002) %>%
  dplyr::select(ccode1, ccode2, absidealdiff, s3un) %>%
  plyr::rename(c("absidealdiff"="id2002", "s3un" = "s3un2002"))

dyad2003 <- filter(dyaddata, year == 2003) %>%
  dplyr::select(ccode1, ccode2, absidealdiff, s3un) %>%
  plyr::rename(c("absidealdiff"="id2003", "s3un" = "s3un2003"))

dyad2004 <- filter(dyaddata, year == 2004) %>%
  dplyr::select(ccode1, ccode2, absidealdiff, s3un) %>%
  plyr::rename(c("absidealdiff"="id2004", "s3un" = "s3un2004"))

dyad2006 <- filter(dyaddata, year == 2006) %>%
  dplyr::select(ccode1, ccode2, absidealdiff, s3un) %>%
  plyr::rename(c("absidealdiff"="id2006", "s3un" = "s3un2006"))

dyad2007 <- filter(dyaddata, year == 2007) %>%
  dplyr::select(ccode1, ccode2, absidealdiff, s3un) %>%
  plyr::rename(c("absidealdiff"="id2007", "s3un" = "s3un2007"))


####now merge the two
mergedyad <- left_join(crs2005collapsed, dyad2005clean, by = c("ccode1"="ccode1", "ccode2"="ccode2"))


## merge the other years

mergedyad <- left_join(mergedyad, dyad2000, by = c("ccode1"="ccode1", "ccode2"="ccode2"))
mergedyad <- left_join(mergedyad, dyad2001, by = c("ccode1"="ccode1", "ccode2"="ccode2"))
mergedyad <- left_join(mergedyad, dyad2002, by = c("ccode1"="ccode1", "ccode2"="ccode2"))
mergedyad <- left_join(mergedyad, dyad2003, by = c("ccode1"="ccode1", "ccode2"="ccode2"))
mergedyad <- left_join(mergedyad, dyad2004, by = c("ccode1"="ccode1", "ccode2"="ccode2"))
mergedyad <- left_join(mergedyad, dyad2006, by = c("ccode1"="ccode1", "ccode2"="ccode2"))
mergedyad <- left_join(mergedyad, dyad2007, by = c("ccode1"="ccode1", "ccode2"="ccode2"))
mergedyad <- left_join(mergedyad, crs2004collapsed, by = c("ccode1"="ccode1", "ccode2"="ccode2"))


##### Adding Controls

controls <- read_dta("extended_data_0401.dta")

#already lagged
controls2005 <- filter(controls, year == 2005 )


####Adding control variables to the dataset

###Recipient

#controls2005$conflict_prioryear
mergedyad <- left_join(mergedyad, 
                       controls2005[, c("cowcode", "conflict_prioryear")],
                       by = c("ccode2" = "cowcode"))
table(is.na(controls2005$conflict_prioryear))


#controls2005$polity2_dem_ord #this is the change in democratization
mergedyad <- left_join(mergedyad, 
                       controls2005[, c("cowcode", "polity2_dem_ord")],
                       by = c("ccode2" = "cowcode"))
table(is.na(controls2005$polity2_dem_ord))


#controls2005$democracy #this is the democracy score
mergedyad <- left_join(mergedyad, 
                       controls2005[, c("cowcode", "democracy")],
                       by = c("ccode2" = "cowcode"))
table(is.na(controls2005$democracy))

#controls2005$pop #this is the population that has not been logged #I use it to scale the aid flow
mergedyad <- left_join(mergedyad, 
                       controls2005[, c("cowcode", "pop")],
                       by = c("ccode2" = "cowcode"))
mergedyad <- plyr::rename(mergedyad, c("pop"="r_pop_notlogged"))



#rename to mark these ones as the recipient variables
mergedyad <- plyr::rename(mergedyad, c("democracy"="r_democracy", 
                                               "conflict_prioryear" = "r_conflict_prioryear",
                                               "polity2_dem_ord" = "r_politychange"))




###Both
mergedyad$ccode1 <- as.numeric(mergedyad$ccode1)


#controls2005$growth_real_gdppc
  #recipient
mergedyad <- left_join(mergedyad, 
                       controls2005[, c("cowcode", "growth_real_gdppc")],
                       by = c("ccode2" = "cowcode"))
mergedyad <- plyr::rename(mergedyad, c("growth_real_gdppc"="r_gdp_growth"))

  #donor
mergedyad <- left_join(mergedyad, 
                       controls2005[, c("cowcode", "growth_real_gdppc")],
                       by = c("ccode1" = "cowcode"))
mergedyad <- plyr::rename(mergedyad, c("growth_real_gdppc"="d_gdp_growth"))


#controls2005$real_gdppc
  #recipient
mergedyad <- left_join(mergedyad, 
                       controls2005[, c("cowcode", "real_gdppc")],
                       by = c("ccode2" = "cowcode"))
mergedyad <- plyr::rename(mergedyad, c("real_gdppc"="r_gdp"))

  #donor
mergedyad <- left_join(mergedyad, 
                       controls2005[, c("cowcode", "real_gdppc")],
                       by = c("ccode1" = "cowcode"))
mergedyad <- plyr::rename(mergedyad, c("real_gdppc"="d_gdp"))


#controls2005$population
  #recipient
mergedyad <- left_join(mergedyad, 
                       controls2005[, c("cowcode", "population")],
                       by = c("ccode2" = "cowcode"))
mergedyad <- plyr::rename(mergedyad, c("population"="r_population"))

  #donor
mergedyad <- left_join(mergedyad, 
                       controls2005[, c("cowcode", "population")],
                       by = c("ccode1" = "cowcode"))
mergedyad <- plyr::rename(mergedyad, c("population"="d_population"))


###Donor

#controls2005$women_parliament
mergedyad <- left_join(mergedyad, 
                       controls2005[, c("cowcode", "women_parliament")],
                       by = c("ccode1" = "cowcode"))
mergedyad <- plyr::rename(mergedyad, c("women_parliament"="d_women_parliament"))



##Divide aidflow by population (1000) and log it
mergedyad$aidflow_corrected <- log(mergedyad$aidflow/mergedyad$r_pop_notlogged*1000)


####CEPII data
cepii <- read_dta("dist_cepii.dta")
cepii$ccode1 <- countrycode(cepii$iso_o, "iso3c", "cown")
cepii$ccode2 <- countrycode(cepii$iso_d, "iso3c", "cown")
cepii$distlog <- log(cepii$dist)
#only the small countries (islands) are missing

mergedyad <- left_join(mergedyad, 
                       cepii,
                       by = c("ccode1"="ccode1", "ccode2"="ccode2"))



####first differences
mergedyad$idfd0304 <- mergedyad$id2004 - mergedyad$id2003
merge_pdata$idfd0304 <- merge_pdata$id2004 - merge_pdata$id2003

mergedyad$idfd0607 <- mergedyad$id2007 - mergedyad$id2006
merge_pdata$idfd0607 <- merge_pdata$id2007 - merge_pdata$id2006 


####continents & regions
mergedyad$continent1 <- countrycode(mergedyad$ccode1, "cown", "continent")
mergedyad$continent2 <- countrycode(mergedyad$ccode2, "cown", "continent")
mergedyad$samecont <- as.numeric(mergedyad$continent1 == mergedyad$continent2)
  #973 zeros, 142 ones

mergedyad$region1 <- countrycode(mergedyad$ccode1, "cown", "region")
mergedyad$region2 <- countrycode(mergedyad$ccode2, "cown", "region")
mergedyad$samereg <- as.numeric(mergedyad$region1 == mergedyad$region2)
  #1105 zeros, 10 ones



#### DESTA data- treaties
#collapse on dyad for treaties before 2004
#create column for number of treaties to be filled with 0s too after merge
DESTA <- read.csv(file="DESTA_treaties_dyadic.csv", header=TRUE, sep=",")
desta1 <- select(DESTA, iso1, iso2, year)

desta1$ccode1 <- countrycode(desta1$iso1, "iso3n", "cown")
desta1$ccode2 <- countrycode(desta1$iso2, "iso3n", "cown")
desta1 <- filter(desta1, year < 2005)

#create column for number of treaties to be filled with 0s too after merge
  #count of treaties
desta2 <- group_by(desta1, ccode1, ccode2) %>% 
  dplyr::summarise(count = length(year))
#this worked


#### IMI data
#purpose- collapse on maximum
militaryIMI <- read.csv("KisanganiandPickeringIMI1989-2005.csv", header = T, sep=",")
military <- as.data.frame(cbind(militaryIMI$ï..VAR.001, militaryIMI$VAR.002, militaryIMI$VAR.003, militaryIMI$VAR.004,
                 militaryIMI$VAR.021, militaryIMI$VAR.022, militaryIMI$VAR.023, militaryIMI$VAR.024, 
                 militaryIMI$VAR025))

military <- plyr::rename(military, c("V1" = "ccode1", "V2" = "ccode2", "V3" = "start", "V4" = "end",
                                     "V5" = "strategic", "V6" = "humanitarian", "V7"= "territorial",
                                     "V8" = "protectown"))

military$year <- substr(military$end,1,nchar(military$end)-4)

military2 <- group_by(military, ccode1, ccode2, year) %>% 
  dplyr::summarise(strategic = max(strategic), humanitarian = max(humanitarian),
                   territorial = max(territorial),protectown = max(protectown), intervention = 1) 

military2002 <- filter(military2, year == 2002)
military2003 <- filter(military2, year == 2003)
military2004 <- filter(military2, year == 2004)
#each as very few observations



####merge military and desta data 
mergedyad <- left_join(mergedyad, 
                       desta2,
                       by = c("ccode1"="ccode1", "ccode2"="ccode2"))

##for military, let's just do 2003 for now
mergedyad <- left_join(mergedyad, 
                       military2003,
                       by = c("ccode1"="ccode1", "ccode2"="ccode2"))

mergedyad[, 56:62][is.na(mergedyad[, 56:62])] <- 0
  #both have very few non-zeros


#####need to create a new dataframe where the aidflow corrected has no missing values
#since we can't have missing y

mergedyad2 <- filter(mergedyad, 
                     !is.na(aidflow_corrected) & !is.nan(aidflow_corrected) & !is.infinite(aidflow_corrected))
#don't need to filter for 2004

#I think it is good to have this for both
mergedyad <- mergedyad2


###to do TWC with HC3 errors, turn data into pdata.frame and use plm
merge_pdata <- pdata.frame(mergedyad, index = c("ccode1", "ccode2"))
merge_pdata2 <- pdata.frame(mergedyad2, index = c("ccode1", "ccode2"))


###no iraq data
#remove the US and Iraq (or just Iraq)
merge_pdata2_noirq <- filter(merge_pdata2, ccode2 != 625 )
#countrycode("Iraq", "country.name", "cown")


#looks good


#### Table 1 - Descriptives #####
stargazer(as.data.frame(mergedyad)[,-3],
          digits.extra= 0, 
          median = T, 
          omit = c("ccode", "s3un", "2003", "2007", "aidflow04", "logged"), 
          covariate.labels = c("Ideal Point, 2005", 
                               "Ideal Point, 2004", 
                               "Ideal Point, 2006",
                               "Conflict, Prior Year, R",
                               "Democratization (ord), R",
                               "Democracy (ord), R",
                               "GDP growth, R",
                               "GDP growth, D",
                               "GDP, R",
                               "GDP, D",
                               "Population, logged, R",
                               "Population, logged, D",
                               "Women in Parliament, D",
                               "Aidflow (per 1000)",
                               "Ideal Point, FD 03-04",
                               "Ideal Point, FD 06-07"),
          title = "Descriptive Statistics of the Data",
          type= "latex",
          font.size = "tiny")



#~~~~~
#my first model
model1 <- lm(aidflow_corrected ~ id2005, 
             data = mergedyad)
summary(model1)

# this is clustered error but it is not C3
m1_hc3 <- cluster.vcov(model1, cbind(mergedyad$ccode1, mergedyad$ccode2))
coeftest(model1, m1_hc3)

#to do TWC with HC3 errors, turn data into pdata.frame and use plm
merge_pdata <- pdata.frame(mergedyad, index = c("ccode1", "ccode2"))

model1plm <- plm(aidflow_corrected ~ id2005, 
                 data = merge_pdata, 
                 model = "pooling",
                 index = c("ccode1", "ccode2"))
summary(model1plm)

##this is the TWC with HC3 inflated errors
t1m1ct <- coeftest(model1plm, vcov=function(x) vcovDC(x, type="HC3"))

#~~~~
#second model
#need to use lagged ideal points data (2004 data, 2003 data)

model2a <- lm(aidflow_corrected ~ id2003, 
              data = mergedyad)
summary(model2a)

model2aplm <- plm(aidflow_corrected ~ id2003, 
                  data = merge_pdata, 
                  model = "pooling",
                  index = c("ccode1", "ccode2"))
summary(model2aplm)
t1m2act <- coeftest(model2aplm, vcov=function(x) vcovDC(x, type="HC3"))

#~~

model2b <- lm(aidflow_corrected ~ id2004, 
              data = mergedyad)
summary(model2b)

model2bplm <- plm(aidflow_corrected ~ id2004, 
                  data = merge_pdata, 
                  model = "pooling",
                  index = c("ccode1", "ccode2"))
summary(model2bplm)
t1m2bct <- coeftest(model2bplm, vcov=function(x) vcovDC(x, type="HC3"))

#this (2004 data) has the strongest effect as expected (decaying independent variable? AR-1 needed?)
#decaying independent variable means that later data can predict results for 2005 (hard to see reverse causality)


#~~~~
#third model
#first differences, lagged (positive is less similar, negative is more similar)
#2004 - 2003


model3 <- lm(aidflow_corrected ~ idfd0304, 
             data = mergedyad)
summary(model3)

model3plm <- plm(aidflow_corrected ~ idfd0304, 
                 data = merge_pdata, 
                 model = "pooling",
                 index = c("ccode1", "ccode2"))
summary(model3plm)
t1m3ct <- coeftest(model3plm, vcov=function(x) vcovDC(x, type="HC3"))

#not significant (and, def not significant with twc errors)

#~~~~~
#fourth model
#test for reverse causality
#lagged
model4a <- lm(aidflow_corrected ~ id2006, 
              data = mergedyad)
summary(model4a)

model4aplm <- plm(aidflow_corrected ~ id2006, 
                  data = merge_pdata, 
                  model = "pooling",
                  index = c("ccode1", "ccode2"))
summary(model4aplm)
t1m4act <- coeftest(model4aplm, vcov=function(x) vcovDC(x, type="HC3"))

#~~

model4b <- lm(aidflow_corrected ~ id2007, 
              data = mergedyad)
summary(model4b)

model4bplm <- plm(aidflow_corrected ~ id2007, 
                  data = merge_pdata, 
                  model = "pooling",
                  index = c("ccode1", "ccode2"))
summary(model4bplm)
t1m4bct <- coeftest(model4bplm, vcov=function(x) vcovDC(x, type="HC3"))

#although these are significant, it could be cause of the decaying independent variable (which is not an fd)
#so we should try with an fd

#~~~
#first differences, leading
#2007 - 2006

model4c <- lm(aidflow_corrected ~ idfd0607, 
              data = mergedyad)
summary(model4c)

model4cplm <- plm(aidflow_corrected ~ idfd0607, 
                  data = merge_pdata, 
                  model = "pooling",
                  index = c("ccode1", "ccode2"))
summary(model4cplm)
t1m4cct <- coeftest(model4cplm, vcov=function(x) vcovDC(x, type="HC3"))

#this is significant, so it means that there could be reverse causality
## but what is weird is the the signs are opposite (remember that negative is more similar)

###There seems to be autocorrelation, but I am technically not using panel data so I don't know if I can
#use AR models or ECM


######## Regression Table 2 ########

stargazer(t1m1ct, 
          #t1m2act, #2003
          t1m2bct, 
          t1m3ct, 
          t1m4act, 
          #t1m4bct, #2007
          t1m4cct, 
          type = "latex", 
          covariate.labels = c("Ideal Point (2005)",
                               #"Ideal Point (2003)",
                               "Ideal Point (2004)",
                               "I.P First difference (03-04)",
                               "Ideal Point (2006)",
                               #"Ideal Point (2007)",
                               "I.P First difference (06-07)"),
          notes = "Observations: 1115 ",
          font.size = "tiny")





#####~~~~~~~~~~~~~~~~~~~~~~~~~~

##### Models with controls

#~~~~~
#my first model
tb2_model1 <- lm(aidflow_corrected ~ id2005 +
                   r_conflict_prioryear +
                   r_politychange +
                   r_democracy +
                   r_gdp_growth +
                   d_gdp_growth +
                   r_gdp +
                   d_gdp +
                   r_population +
                   d_population +
                   d_women_parliament, 
             data = mergedyad2)
summary(tb2_model1)

# this is clustered error but it is not C3
t2m1_hc3 <- cluster.vcov(tb2_model1, cbind(mergedyad2$ccode1, mergedyad2$ccode2))
coeftest(tb2_model1, t2m1_hc3)

#to do TWC with HC3 errors, turn data into pdata.frame and use plm
#using it with the new mergedyad2
merge_pdata2 <- pdata.frame(mergedyad2, index = c("ccode1", "ccode2"))



tb2_model1plm <- plm(aidflow_corrected ~ id2005 +
                       r_conflict_prioryear +
                       r_politychange +
                       r_democracy +
                       r_gdp_growth +
                       d_gdp_growth +
                       r_gdp +
                       d_gdp +
                       r_population +
                       d_population +
                       d_women_parliament, 
                 data = merge_pdata2, 
                 model = "pooling",
                 index = c("ccode1", "ccode2"))
summary(tb2_model1plm)

##this is the TWC with HC3 inflated errors
t2m1ct <- coeftest(tb2_model1plm, vcov=function(x) vcovDC(x, type="HC3"))



#~~~~
#second model
#need to use lagged ideal points data (2004 data, 2003 data)

tb2_model2a <- lm(aidflow_corrected ~ id2003 +
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdp +
                d_gdp +
                r_population +
                d_population +
                d_women_parliament, 
              data = mergedyad2)
summary(tb2_model2a)

tb2_model2aplm <- plm(aidflow_corrected ~ id2003 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament, 
                  data = merge_pdata2, 
                  model = "pooling",
                  index = c("ccode1", "ccode2"))
summary(tb2_model2aplm)
t2m2act <- coeftest(tb2_model2aplm, vcov=function(x) vcovDC(x, type="HC3"))

#~~

tb2_model2b <- lm(aidflow_corrected ~ id2004 +
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdp +
                d_gdp +
                r_population +
                d_population +
                d_women_parliament, 
              data = mergedyad2)
summary(tb2_model2b)

tb2_model2bplm <- plm(aidflow_corrected ~ id2004 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament, 
                  data = merge_pdata2, 
                  model = "pooling",
                  index = c("ccode1", "ccode2"))
summary(tb2_model2bplm)
t2m2bct <- coeftest(tb2_model2bplm, vcov=function(x) vcovDC(x, type="HC3"))

#with controls, TWC kills the significance


#~~~~
#third model
#first differences, lagged (positive is less similar, negative is more similar)
#2004 - 2003


tb2_model3 <- lm(aidflow_corrected ~ idfd0304 +
               r_conflict_prioryear +
               r_politychange +
               r_democracy +
               r_gdp_growth +
               d_gdp_growth +
               r_gdp +
               d_gdp +
               r_population +
               d_population +
               d_women_parliament,  
             data = mergedyad2)
summary(tb2_model3)

tb2_model3plm <- plm(aidflow_corrected ~ idfd0304 +
                       r_conflict_prioryear +
                       r_politychange +
                       r_democracy +
                       r_gdp_growth +
                       d_gdp_growth +
                       r_gdp +
                       d_gdp +
                       r_population +
                       d_population +
                       d_women_parliament, 
                 data = merge_pdata2, 
                 model = "pooling",
                 index = c("ccode1", "ccode2"))
summary(tb2_model3plm)
t2m3ct <- coeftest(tb2_model3plm, vcov=function(x) vcovDC(x, type="HC3"))

#not significant (and, def not significant with twc errors)

#~~~~~
#fourth model
#test for reverse causality
#lagged
tb2_model4a <- lm(aidflow_corrected ~ id2006 +
                    r_conflict_prioryear +
                    r_politychange +
                    r_democracy +
                    r_gdp_growth +
                    d_gdp_growth +
                    r_gdp +
                    d_gdp +
                    r_population +
                    d_population +
                    d_women_parliament, 
              data = mergedyad2)
summary(tb2_model4a)

tb2_model4aplm <- plm(aidflow_corrected ~ id2006 +
                    r_conflict_prioryear +
                    r_politychange +
                    r_democracy +
                    r_gdp_growth +
                    d_gdp_growth +
                    r_gdp +
                    d_gdp +
                    r_population +
                    d_population +
                    d_women_parliament, 
                  data = merge_pdata2, 
                  model = "pooling",
                  index = c("ccode1", "ccode2"))
summary(tb2_model4aplm)
t2m4act <- coeftest(tb2_model4aplm, vcov=function(x) vcovDC(x, type="HC3"))

#~~

tb2_model4b <- lm(aidflow_corrected ~ id2007 +
                    r_conflict_prioryear +
                    r_politychange +
                    r_democracy +
                    r_gdp_growth +
                    d_gdp_growth +
                    r_gdp +
                    d_gdp +
                    r_population +
                    d_population +
                    d_women_parliament, 
              data = mergedyad2)
summary(tb2_model4b)

tb2_model4bplm <- plm(aidflow_corrected ~ id2007 +
                    r_conflict_prioryear +
                    r_politychange +
                    r_democracy +
                    r_gdp_growth +
                    d_gdp_growth +
                    r_gdp +
                    d_gdp +
                    r_population +
                    d_population +
                    d_women_parliament, 
                  data = merge_pdata2, 
                  model = "pooling",
                  index = c("ccode1", "ccode2"))
summary(tb2_model4bplm)
t2m4bct <- coeftest(tb2_model4bplm, vcov=function(x) vcovDC(x, type="HC3"))


#not significant

#~~~
#first differences, leading
#2007 - 2006


tb2_model4c <- lm(aidflow_corrected ~ idfd0607+
                    r_conflict_prioryear +
                    r_politychange +
                    r_democracy +
                    r_gdp_growth +
                    d_gdp_growth +
                    r_gdp +
                    d_gdp +
                    r_population +
                    d_population +
                    d_women_parliament, 
              data = mergedyad2)
summary(tb2_model4c)

tb2_model4cplm <- plm(aidflow_corrected ~ idfd0607 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament, 
                  data = merge_pdata2, 
                  model = "pooling",
                  index = c("ccode1", "ccode2"))
summary(tb2_model4cplm)
t2m4cct <- coeftest(tb2_model4cplm, vcov=function(x) vcovDC(x, type="HC3"))


#with the controls, this model is actually significant (although weak)
  #with just the idfd0607, it is quite significant
  #hints at reverse causality

##let's do the other regression
tb2_model4dplm <- plm(idfd0607 ~ aidflow_corrected  +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament, 
                      data = merge_pdata2, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb2_model4dplm)
t2m4dct <- coeftest(tb2_model4dplm, vcov=function(x) vcovDC(x, type="HC3"))

#I should try with the other years to make sure
  #this will take me a while to make

## but what is weird is the the signs are opposite (remember that negative is more similar)


##### April 9
##### new controls##################################

##### Models with controls

#~~~~~
#my first model
tb3_model1plm <- plm(aidflow_corrected ~ id2005 +
                       r_conflict_prioryear +
                       r_politychange +
                       r_democracy +
                       r_gdp_growth +
                       d_gdp_growth +
                       r_gdp +
                       d_gdp +
                       r_population +
                       d_population +
                       d_women_parliament +
                       distlog + 
                       colony +
                       count +
                       intervention,
                     data = merge_pdata2, 
                     model = "pooling",
                     index = c("ccode1", "ccode2"))
summary(tb3_model1plm)

##this is the TWC with HC3 inflated errors
t3m1ct <- coeftest(tb3_model1plm, vcov=function(x) vcovDC(x, type="HC3"))



#~~~~
#second model
#need to use lagged ideal points data (2004 data, 2003 data)

#intervention dropped due to perfect collinearity


tb3_model2aplm <- plm(aidflow_corrected ~ id2003 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention,
                      data = merge_pdata2, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb3_model2aplm)
t3m2act <- coeftest(tb3_model2aplm, vcov=function(x) vcovDC(x, type="HC3"))

#~~


tb3_model2bplm <- plm(aidflow_corrected ~ id2004 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention,
                      data = merge_pdata2, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb3_model2bplm)
t3m2bct <- coeftest(tb3_model2bplm, vcov=function(x) vcovDC(x, type="HC3"))

#with controls, TWC kills the significance

tb3_model2cplm <- plm(aidflow_corrected ~ id2001 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention,
                      data = merge_pdata2, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb3_model2cplm)
t3m2cct <- coeftest(tb3_model2cplm, vcov=function(x) vcovDC(x, type="HC3"))


tb3_model2dplm <- plm(aidflow_corrected ~ id2002 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention,
                      data = merge_pdata2, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb3_model2dplm)
t3m2dct <- coeftest(tb3_model2dplm, vcov=function(x) vcovDC(x, type="HC3"))
#even with 2001 and 2002, the significance is not stable



#~~~~
#third model
#first differences, lagged (positive is less similar, negative is more similar)
#2004 - 2003


tb3_model3plm <- plm(aidflow_corrected ~ idfd0304 +
                       r_conflict_prioryear +
                       r_politychange +
                       r_democracy +
                       r_gdp_growth +
                       d_gdp_growth +
                       r_gdp +
                       d_gdp +
                       r_population +
                       d_population +
                       d_women_parliament+
                       distlog + 
                       colony +
                       count +
                       intervention,
                     data = merge_pdata2, 
                     model = "pooling",
                     index = c("ccode1", "ccode2"))
summary(tb3_model3plm)
t3m3ct <- coeftest(tb3_model3plm, vcov=function(x) vcovDC(x, type="HC3"))

#not significant (and, def not significant with twc errors)

#~~~~~
#fourth model
#test for reverse causality
#lagged

tb3_model4aplm <- plm(aidflow_corrected ~ id2006 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention, 
                      data = merge_pdata2, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb3_model4aplm)
t3m4act <- coeftest(tb3_model4aplm, vcov=function(x) vcovDC(x, type="HC3"))


#~~

tb3_model4bplm <- plm(aidflow_corrected ~ id2007 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention, 
                      data = merge_pdata2, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb3_model4bplm)
t3m4bct <- coeftest(tb3_model4bplm, vcov=function(x) vcovDC(x, type="HC3"))


#not significant

#~~~
#first differences, leading
#2007 - 2006


tb3_model4cplm <- plm(aidflow_corrected ~ idfd0607 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament +
                        distlog + 
                        colony +
                        count +
                        intervention, 
                      data = merge_pdata2, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb3_model4cplm)
t3m4cct <- coeftest(tb3_model4cplm, vcov=function(x) vcovDC(x, type="HC3"))



tb3_model4dplm <- plm(idfd0607 ~ aidflow_corrected  +
                              r_conflict_prioryear +
                              r_politychange +
                              r_democracy +
                              r_gdp_growth +
                              d_gdp_growth +
                              r_gdp +
                              d_gdp +
                              r_population +
                              d_population +
                              d_women_parliament +
                              distlog + 
                              colony +
                              count +
                              intervention, 
                            data = merge_pdata2, 
                            model = "pooling",
                            index = c("ccode1", "ccode2"))
summary(tb3_model4dplm)
t3m4dct <- coeftest(tb3_model4dplm, vcov=function(x) vcovDC(x, type="HC3"))




#####################remove Iraq##################33


#my first model
tb4_model1plm <- plm(aidflow_corrected ~ id2005 +
                       r_conflict_prioryear +
                       r_politychange +
                       r_democracy +
                       r_gdp_growth +
                       d_gdp_growth +
                       r_gdp +
                       d_gdp +
                       r_population +
                       d_population +
                       d_women_parliament +
                       distlog + 
                       colony +
                       count +
                       intervention,
                     data = merge_pdata2_noirq, 
                     model = "pooling",
                     index = c("ccode1", "ccode2"))
summary(tb4_model1plm)

##this is the TWC with HC3 inflated errors
t4m1ct <- coeftest(tb4_model1plm, vcov=function(x) vcovDC(x, type="HC3"))



#~~~~
#second model
#need to use lagged ideal points data (2004 data, 2003 data)

#intervention dropped due to perfect collinearity


tb4_model2aplm <- plm(aidflow_corrected ~ id2003 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention,
                      data = merge_pdata2_noirq, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb4_model2aplm)
t4m2act <- coeftest(tb4_model2aplm, vcov=function(x) vcovDC(x, type="HC3"))

#~~


tb4_model2bplm <- plm(aidflow_corrected ~ id2004 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention,
                      data = merge_pdata2_noirq, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb4_model2bplm)
t4m2bct <- coeftest(tb4_model2bplm, vcov=function(x) vcovDC(x, type="HC3"))

#with controls, TWC kills the significance

tb4_model2cplm <- plm(aidflow_corrected ~ id2001 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention,
                      data = merge_pdata2_noirq, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb4_model2cplm)
t3m2cct <- coeftest(tb4_model2cplm, vcov=function(x) vcovDC(x, type="HC3"))


tb4_model2dplm <- plm(aidflow_corrected ~ id2002 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention,
                      data = merge_pdata2_noirq, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb4_model2dplm)
t3m2dct <- coeftest(tb3_model2dplm, vcov=function(x) vcovDC(x, type="HC3"))
#no significance



#~~~~
#third model
#first differences, lagged (positive is less similar, negative is more similar)
#2004 - 2003


tb4_model3plm <- plm(aidflow_corrected ~ idfd0304 +
                       r_conflict_prioryear +
                       r_politychange +
                       r_democracy +
                       r_gdp_growth +
                       d_gdp_growth +
                       r_gdp +
                       d_gdp +
                       r_population +
                       d_population +
                       d_women_parliament+
                       distlog + 
                       colony +
                       count +
                       intervention,
                     data = merge_pdata2_noirq, 
                     model = "pooling",
                     index = c("ccode1", "ccode2"))
summary(tb4_model3plm)
t4m3ct <- coeftest(tb4_model3plm, vcov=function(x) vcovDC(x, type="HC3"))

#not significant (and, def not significant with twc errors)

#~~~~~
#fourth model
#test for reverse causality
#lagged

tb4_model4aplm <- plm(aidflow_corrected ~ id2006 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention, 
                      data = merge_pdata2_noirq, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb4_model4aplm)
t4m4act <- coeftest(tb4_model4aplm, vcov=function(x) vcovDC(x, type="HC3"))


#~~

tb4_model4bplm <- plm(aidflow_corrected ~ id2007 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention, 
                      data = merge_pdata2_noirq, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb4_model4bplm)
t3m4bct <- coeftest(tb4_model4bplm, vcov=function(x) vcovDC(x, type="HC3"))
#not significant

#~~~
#first differences, leading
#2007 - 2006


tb4_model4cplm <- plm(aidflow_corrected ~ idfd0607 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament +
                        distlog + 
                        colony +
                        count +
                        intervention, 
                      data = merge_pdata2_noirq, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb4_model4cplm)
t4m4cct <- coeftest(tb4_model4cplm, vcov=function(x) vcovDC(x, type="HC3"))


#need to work on lags
tb4_model4dplm_noirq <- plm(idfd0607 ~ aidflow_corrected  +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention, 
                      data = merge_pdata2_noirq, 
                      model = "pooling",
                      index = c("ccode1", "ccode2"))
summary(tb4_model4dplm_noirq)
t4m4dct_noirq <- coeftest(tb4_model4dplm_noirq, vcov=function(x) vcovDC(x, type="HC3"))





#################### fixed effects (include all)#############################

#my first model
tb5_model1plm <- plm(aidflow_corrected ~ id2005 +
                       r_conflict_prioryear +
                       r_politychange +
                       r_democracy +
                       r_gdp_growth +
                       d_gdp_growth +
                       r_gdp +
                       d_gdp +
                       r_population +
                       d_population +
                       d_women_parliament +
                       distlog + 
                       colony +
                       count +
                       intervention,
                     data = merge_pdata2, 
                     model = "within",
                     effect = "twoways",
                     index = c("ccode1", "ccode2"))
summary(tb5_model1plm)

##this is the TWC with HC3 inflated errors
t5m1ct <- coeftest(tb5_model1plm, vcov=function(x) vcovDC(x, type="HC3"))



#~~~~
#second model
#need to use lagged ideal points data (2004 data, 2003 data)

#intervention dropped due to perfect collinearity


tb5_model2aplm <- plm(aidflow_corrected ~ id2003 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament +
                        distlog + 
                        colony +
                        count +
                        intervention,
                      data = merge_pdata2, 
                      model = "within",
                      effect = "twoways",
                      index = c("ccode1", "ccode2"))
summary(tb5_model2aplm)
t5m2act <- coeftest(tb5_model2aplm, vcov=function(x) vcovDC(x, type="HC3"))

#~~


tb5_model2bplm <- plm(aidflow_corrected ~ id2004 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention,
                      data = merge_pdata2, 
                      model = "within",
                      effect = "twoways",
                      index = c("ccode1", "ccode2"))
summary(tb5_model2bplm)
t5m2bct <- coeftest(tb5_model2bplm, vcov=function(x) vcovDC(x, type="HC3"))



tb5_model2cplm <- plm(aidflow_corrected ~ id2001 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention,
                      data = merge_pdata2, 
                      model = "within",
                      effect = "twoways",
                      index = c("ccode1", "ccode2"))
summary(tb5_model2cplm)
t5m2cct <- coeftest(tb5_model2cplm, vcov=function(x) vcovDC(x, type="HC3"))


tb5_model2dplm <- plm(aidflow_corrected ~ id2002 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention,
                      data = merge_pdata2, 
                      model = "within",
                      effect = "twoways",
                      index = c("ccode1", "ccode2"))
summary(tb5_model2dplm)
t5m2dct <- coeftest(tb5_model2dplm, vcov=function(x) vcovDC(x, type="HC3"))

#with fixed effects, the effects are very strong
#TWC actually makes effects more significant



#~~~~
#third model
#first differences, lagged (positive is less similar, negative is more similar)
#2004 - 2003


tb5_model3plm <- plm(aidflow_corrected ~ idfd0304 +
                       r_conflict_prioryear +
                       r_politychange +
                       r_democracy +
                       r_gdp_growth +
                       d_gdp_growth +
                       r_gdp +
                       d_gdp +
                       r_population +
                       d_population +
                       d_women_parliament+
                       distlog + 
                       colony +
                       count +
                       intervention,
                     data = merge_pdata2, 
                     model = "within",
                     effect = "twoways",
                     index = c("ccode1", "ccode2"))
summary(tb5_model3plm)
t5m3ct <- coeftest(tb5_model3plm, vcov=function(x) vcovDC(x, type="HC3"))

#not significant 

#~~~~~
#fourth model
#test for reverse causality
#lagged

tb5_model4aplm <- plm(aidflow_corrected ~ id2006 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention, 
                      data = merge_pdata2, 
                      model = "within",
                      effect = "twoways", 
                      index = c("ccode1", "ccode2"))
summary(tb5_model4aplm)
t5m4act <- coeftest(tb5_model4aplm, vcov=function(x) vcovDC(x, type="HC3"))


#~~

tb5_model4bplm <- plm(aidflow_corrected ~ id2007 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament+
                        distlog + 
                        colony +
                        count +
                        intervention, 
                      data = merge_pdata2, 
                      model = "within",
                      effect = "twoways",
                      index = c("ccode1", "ccode2"))
summary(tb5_model4bplm)
t5m4bct <- coeftest(tb5_model4bplm, vcov=function(x) vcovDC(x, type="HC3"))

#significant for 2007

#~~~~~
#first differences, leading
#2007 - 2006


tb5_model4cplm <- plm(aidflow_corrected ~ idfd0607 +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament +
                        distlog + 
                        colony +
                        count +
                        intervention, 
                      data = merge_pdata2, 
                      model = "within",
                      effect = "twoways",
                      index = c("ccode1", "ccode2"))
summary(tb5_model4cplm)
t5m4cct <- coeftest(tb5_model4cplm, vcov=function(x) vcovDC(x, type="HC3"))



tb5_model4dplm <- plm(idfd0607 ~ aidflow_corrected  +
                        r_conflict_prioryear +
                        r_politychange +
                        r_democracy +
                        r_gdp_growth +
                        d_gdp_growth +
                        r_gdp +
                        d_gdp +
                        r_population +
                        d_population +
                        d_women_parliament +
                        distlog + 
                        colony +
                        count +
                        intervention, 
                      data = merge_pdata2, 
                      model = "within",
                      effect = "twoways",
                      index = c("ccode1", "ccode2"))
summary(tb5_model4dplm)
t5m4dct <- coeftest(tb5_model4dplm, vcov=function(x) vcovDC(x, type="HC3"))

#what happens if both sides of causality are significant?

#reverse causality - different controls?







#######Regression table 3

stargazer(t2m1ct, 
          #t2m2act, #2003
          t2m2bct, 
          t2m3ct, 
          t2m4act, 
          #t2m4bct, #2007
          t2m4cct, 
          type = "latex", 
          covariate.labels = c("Ideal Point (2005)",
                             # "Ideal Point (2003)",
                               "Ideal Point (2004)",
                               "I.P First difference (03-04)",
                               "Ideal Point (2006)",
                             # "Ideal Point (2007)",
                               "I.P First difference (06-07)", 
                               "Conflict, Prior Year, R", 
                             # "Democratization (ord), R", 
                               "Democracy (ord), R",
                             # "GDP Growth, R", 
                             # "GDP GRowth, D", 
                               "GDP, R", 
                               "GDP, D",
                               "Population, R", 
                               "Population, D", 
                               "Women In Parliament (%), D"),
          omit = c("polity", "growth"),
          notes = "Observations: 974, GDP growth and democratization not reported due to space",
          font.size = "tiny")



###Table 3
stargazer(t2m4dct, type = "text")


####Table 5
stargazer(t5m1ct, 
          t5m2act, #2003
          t5m2bct,
          t5m2cct,
          t5m2dct,
          t5m3ct, 
          t5m4act, 
          t5m4bct, #2007
          t5m4cct, 
          type = "text", 
          omit = c("polity", "growth"),
          notes = "Observations: 974, GDP growth and democratization not reported due to space",
          font.size = "tiny")





####
#graphs

## normal network graph from edgelist
aidedge <- select(mergedyad, ccode1, ccode2, aidflow)
aidedge$ccode1 <- as.character(aidedge$ccode1)
aidedge$ccode2 <- as.character(aidedge$ccode2)
aidedge$aidflow<- round(aidedge$aidflow, digits = 2)

#plot
aidnetwork <- graph.data.frame(aidedge)


png(filename="network_normal.png",
    units="in", 
    width=14, 
    height=9, 
    res=144)
set.seed(7)
plot.igraph(aidnetwork,
            vertex.color = "slateblue",
            vertex.label = ifelse(strength(aidnetwork) > quantile(strength(aidnetwork), .99), 
                                  countrycode(V(aidnetb)$name, "cown", "country.name"), 
                                  NA), 
            vertex.label.color = "orange",
            vertex.size = 3 + (strength(aidnetwork, mode = "out"))/6, 
            edge.width= sqrt(E(aidnetwork)$aidflow)/2,
            edge.arrow.size = 0.5, 
            edge.color = "gray30",
            layout = layout.fruchterman.reingold,
            margin = -0.1,
            asp = 0.6,
            main = "Aidflow Network", sub = "99th percentile of donors labelled")
#strength is the weighted degree
dev.off()


##bipartite
aidnetb <- graph.data.frame(aidedge, directed = F)
V(aidnetb)$type <- 0
V(aidnetb)[name %in% aidedge$ccode2]$type <- 1
E(aidnetb)$weight <- as.numeric(as.matrix(aidedge)[,3])

#plot
V(aidnetb)$color <- c("orange", "skyblue")[V(aidnetb)$type+1]

#png(filename="network_bipartite.png",
#    units="in", 
#    width=14, 
#    height=9, 
#    res=144)
plot(aidnetb, 
     edge.color="gray30",
     edge.width=sqrt(E(aidnetb)$weight)/2,
     vertex.label = ifelse(strength(aidnetb) > quantile(strength(aidnetb), .99), 
                           countrycode(V(aidnetb)$name, "cown", "country.name"), 
                           NA), 
     vertex.size = sqrt((strength(aidnetb))), 
     vertex.label.dist=0,
     layout=layout_as_bipartite,
     asp = 0.35,
     margin = -0.1,
     main = "Aidflow Network- Bipartite Graph")
#dev.off()

aidnetincidence <- get.incidence(aidnetb, attr = "weight")

#shows the scale of statebuilding efforts in Iraq
#I think we should look at military aid (lagged effect of it) in later models

#we have outliers here
#try samples without the outliers



