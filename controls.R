
####variables I need

#recipient
controls2005$conflict_prioryear
controls2005$polity2_dem_ord

#both
controls2005$growth_real_gdppc
controls2005$real_gdppc
controls2005$population

#donor
controls2005$women_parliament

#~~~

countries_cow




pacman::p_load(dplyr,
               foreign,
               tidyverse,
               stats,
               Zelig,
               zeligverse,
               haven,
               plyr,
               tidyr,
               boot,
               data.table,
               popbio,
               fmsb,
               splines,
               readxl,
               countrycode,
               DataCombine,
               DAMisc,
               mixAK)









#Savun_Tirone_2011_AJPS_Replication_Dataset <- read_dta("Savun-Tirone 2011 AJPS Replication Dataset.dta")
#savun_data_base <- Savun_Tirone_2011_AJPS_Replication_Dataset

#years I am interested in
years <- c(1989:2016)

#to remove empty vectors
remove <- c("")

#get a vector of the countries that are included
#countries<- unique(Savun_Tirone_2011_AJPS_Replication_Dataset$recipient_country)
#countries <- countries[!countries == ""]

countries <- countries_chr

#### april 1
cow_code <- countries_cow

#deleting NAs
#cow_code <- unique(Savun_Tirone_2011_AJPS_Replication_Dataset$recipient_cowcode)







#polityIV
#1989-2016

original_polity_4v2016 <- read_excel("polity_4v2016.xls")

polity_2016_base <- original_polity_4v2016
#making a copy


polity_2016 <- polity_2016_base[polity_2016_base$year %in% c(1988:2016),]
#polity_2016 <- polity_2016[polity_2016$country %in% countries,] (use cowcode instead)
polity_2016 <- polity_2016[polity_2016$ccode %in% cow_code,] 
#creating a new dataset with the years and countries we are interested in
#for polity I start with 1988 because the democratization variables is calculated using value from two years prior



#let's see how many countries are included here
length(unique(polity_2016$ccode))
#there are 142, which means that  37 countries are missing in the polity data

#let's check which countries are missing

cow_code %in% unique(polity_2016$ccode)
#this only gives me T/F, need to match names

missing_ccode_polity <- setdiff(cow_code, unique(polity_2016$ccode))
#this gives me the names of the countries that are missing from Polity
#some of these are very small countries but others are countries that may go by other names

missing_cname_polity <- savun_data_base[match(missing_ccode_polity, cow_code), "recipient_country"]
missing_cname_polity
#most of these are small countries
#can't do anything about these, data is missing
#but it is good to know what they are

class(polity_2016)

#now let's select just the variables we are interested in so that we can use it to build our dataset
polity_2016_final <- dplyr::select(polity_2016, cyear, ccode, scode, country, year, polity, polity2)

#the first polity values have values like -66, let's make them NAs
polity_2016_final$polity[polity_2016_final$polity < -10] = NA

#first create ordinal democratization values for polity1. current value - value two years prior
polity_2016_final$polity1_dem_ord <- ave(polity_2016_final$polity, polity_2016_final$ccode, FUN=function(x) c(0,0, diff(x,lag=2)))

#then make this a dummy variable (this is one was used in savun)
polity_2016_final$polity1_dem <- as.numeric(polity_2016_final$polity1_dem_ord >= 3)  

#do the same as above for polity2
polity_2016_final$polity2_dem_ord <- ave(polity_2016_final$polity2, polity_2016_final$ccode, FUN=function(x) c(0,0, diff(x,lag=2)))
polity_2016_final$polity2_dem <- as.numeric(polity_2016_final$polity2_dem_ord >= 3)  

#finally, delete the 1988 data and rename ccode to cowcode
polity_2016_final <- polity_2016_final[polity_2016_final$year %in% years,]
polity_2016_final <- rename(polity_2016_final, c("ccode" = "cowcode"))

#looks good










#Penn World Tables
#GDP, GDPgrowth, population

pwt90 <- read_excel("pwt90.xlsx", sheet = "Data")
View(pwt90)

pennwt_9 <- pwt90[pwt90$year %in% years,]
#dataset with the years we are interested in

#here countries are coded by ISO, while savun's original data uses COW
#we need to translate between the two

pwt_cowcode <- countrycode(pennwt_9$countrycode, "iso3c", "cown", warn = TRUE)
pennwt_9 <- cbind(pwt_cowcode, pennwt_9)
#creating cow codes for pwt and binding the cow codes to the pwt dataframe
#now we can choose just the countries we need

pennwt_9 <- pennwt_9[pennwt_9$pwt_cowcode %in% cow_code,] 
#creating a new dataset with the years and countries we are interested in

#let's see how many countries are included here
length(unique(pennwt_9$countrycode))
#there are 146, which means that  33 countries are missing in the PWT data

#let's check which countries are missing
missing_ccode_pennwt <- setdiff(cow_code, unique(pennwt_9$pwt_cowcode))
#this gives me the names of the countries that are missing from Polity


#now let's select just the variables we are interested in so that we can use it to build our dataset
pennwt_9_final <- dplyr::select(pennwt_9, pwt_cowcode, countrycode, country, year, rgdpe, pop)

#we need additional variables
#first, GDP per capita is needed
#both are in millions so I just divide
pennwt_9_final$rgdp_pc <- pennwt_9_final$rgdpe/pennwt_9_final$pop

#then we need real gdp growth rate
pennwt_9_final<- ddply(pennwt_9_final,"pwt_cowcode",transform, rgdppc_growth=c(NA,exp(diff(log(rgdp_pc)))-1))
#calculates growth by getting the differences of logs and exponentiating them. In the end, it is basically (a/b) - 1.
#the pwt data used by savun was in percentages, so we should change it to percentages as well
pennwt_9_final$rgdppc_growth <- pennwt_9_final$rgdppc_growth*100

#we also need another population column that is in thousands and logged
pennwt_9_final$population <- log(pennwt_9_final$pop*1000)

#lastly, donor gdp can also be calculated through pwt data
#we need the logged average of annual sweden,us,and france 1989-2014 (then repeat the data)
donors <- c("SWE", "USA", "FRA")
donor_gdp <- filter(pwt90, countrycode %in% donors)
donor_gdp <- filter(donor_gdp, year %in% years)
#making a dataframe so that we are looking at just the donors' gdp for our timeframe

donor_gdp <- dplyr::select(donor_gdp, countrycode, year, rgdpe)
donor_gdp <- spread(donor_gdp, countrycode, rgdpe)
donor_gdp$donor_gdp <- log(rowMeans(donor_gdp[,-1]))


#we use the resulting vector to attach it to the pwt dataset
donor_gdp_vector <- rep(donor_gdp$donor_gdp, 146)
#repeating the vector 146 (while keeping the order)
pennwt_9_final <- cbind(pennwt_9_final, donor_gdp_vector)

#lastly let's rename pwt_cowcode to cowcode for better merging
pennwt_9_final <- rename(pennwt_9_final, c("pwt_cowcode"="cowcode"))

#looks good












#OECD democracy aid

government_civil_aid <- read_excel("government_civil_aid.xlsx", skip = 10)
View(government_civil_aid)

length(unique(government_civil_aid$Recipient))
#there are supposed to be 114, but there are 116
#need to remove "NA" and the comment at the end

government_civil_aid <- government_civil_aid[-2395,]
#removing the row with the comment

oecd_country <- unique(government_civil_aid$Recipient)
oecd_country <- na.omit(oecd_country)
oecd_country <- as.vector(oecd_country)
#removng the na (this makes it atomic, so I make it a vector)

#this dataset does not have country-years, so we make it
#only one row with country per country section
oecd_country <- rep(oecd_country, each=21)
#creating country row by repeating countries 21 times

government_civil_aid <- cbind(oecd_country, government_civil_aid)
#adding the row

government_civil_aid_copy <- government_civil_aid
#make a copy just in case


#clean the data

government_civil_aid <- government_civil_aid[,-4]
#removing the empty column
government_civil_aid <- government_civil_aid[,-2]
#removing the former country column 

colnames(government_civil_aid)[colnames(government_civil_aid)=="X__2"] <- "democracy_aid"
#rename the column that was arbitrarily named

government_civil_aid[government_civil_aid == ".."] <- NA
#some missing data were coded as "..", so I changed them to NAs

#getting ready to merge
#government_civil_aid$Recipient %in% countries
#unique(government_civil_aid$Recipient %in% countries)

setdiff(countries, oecd_country)
#check which countries (the vector we made) are missing from oecd countries
length(setdiff(countries, oecd_country))
#there are 86 missing countries
#some of them are small, some have become richer and don't receive aid any more
#but some are in both but have different names

setdiff(oecd_country, countries)
#so we look at which OECD countries are missing from our countries vector
#and find duplicates (need to do this manually, since there are many variations of country names)

sort(setdiff(countries, oecd_country))
#compare against an alphabetically sorted vector

government_civil_aid$oecd_country <- as.character((government_civil_aid$oecd_country))
#to replace, this column needs to be a character

government_civil_aid[government_civil_aid=="Former Yugoslav Republic of Macedonia"] <- "Macedonia"
government_civil_aid[government_civil_aid=="States Ex-Yugoslavia"] <- "Yugoslavia"
government_civil_aid[government_civil_aid=="Cabo Verde"] <- "Cape Verde"
government_civil_aid[government_civil_aid=="Congo"] <- "Congo, Republic of"
government_civil_aid[government_civil_aid=="Côte d'Ivoire"] <- "Cote d`Ivoire"
government_civil_aid[government_civil_aid=="Democratic Republic of the Congo"] <- "Congo, Dem. Rep."
government_civil_aid[government_civil_aid=="Gambia"] <- "Gambia, The"
government_civil_aid[government_civil_aid=="Saint Helena"] <- "St. Helena"
government_civil_aid[government_civil_aid=="Antigua and Barbuda"] <- "Antigua & Barbuda"
government_civil_aid[government_civil_aid=="Saint Kitts and Nevis"] <- "St. Kitts & Nevis"
government_civil_aid[government_civil_aid=="Saint Lucia"] <- "St. Lucia"
government_civil_aid[government_civil_aid=="Saint Vincent and the Grenadines"] <- "St.Vincent & Grenadines"
government_civil_aid[government_civil_aid=="Trinidad and Tobago"] <- "Trinidad &Tobago"
government_civil_aid[government_civil_aid=="Turks and Caicos Islands"] <- "Turks & Caicos"

#remove countries that are actually not included (move this to later?)
government_civil_aid <- government_civil_aid[government_civil_aid$oecd_country %in% countries,]

#now the country names should be the same

#I just fix one of the column names
government_civil_aid <- rename(government_civil_aid, c("Year" = "year", "oecd_country" = "recipient_country"))

#and reorder the columns
government_civil_aid <- government_civil_aid[c("year", "recipient_country", "democracy_aid")]

#just getting 2002 and later data to merge with savun data
government_civil_aid <- filter(government_civil_aid, year >2001)

#change vector classes
government_civil_aid$year <- as.numeric(government_civil_aid$year)
government_civil_aid$recipient_country <- as.character(government_civil_aid$recipient_country)
government_civil_aid$democracy_aid <- as.numeric(government_civil_aid$democracy_aid)

#but I need to get the pop data so that I can compare
#dividing using the pop data


#for this, we need the cow code in both savun and oecd data
government_civil_aid2 <- government_civil_aid

#create a "key" between countries (as they are spelled in the original database) and the cowcodes)
savun_country <- unique(savun_data_base$recipient_country)
savun_cowcode <- unique(savun_data_base$recipient_cowcode)
countryandcow <- as.data.frame(cbind(savun_country, savun_cowcode))
names(countryandcow)[1] <- "country"
names(countryandcow)[2] <- "cowcode"

#now create an empty column for the cowcode and match with the key to fill it in
government_civil_aid[,"cowcode"] <- NA
government_civil_aid$cowcode <- countryandcow$cowcode[match(government_civil_aid2$recipient_country, countryandcow$country)]

#merge with penn world tables data using the cowcode
#need to rearrange code so that this part comes after pennworld tables

#prepare the pwt data for merging with oecd data
pwt_pop <- dplyr::select(pennwt_9_final, cowcode, year, pop)
pwt_pop <- filter(pwt_pop, year>2001)

#merge oecd data and pwt data
govt_aid_pop <- merge(pwt_pop,government_civil_aid, by=c("cowcode","year"))

#check number of countries
length(unique(govt_aid_pop$cowcode))
#only 92, but the final extension dataset should have many countries and many blanks

#savun data's democracy aid is per 1000 people. Since pop is in millions, pop*1000000/1000= pop*1000
govt_aid_pop$democracy_aid_final <- govt_aid_pop$democracy_aid/govt_aid_pop$pop*1000

#now we can remove the unconverted democracy_aid data and give that name to the new column
govt_aid_pop$democracy_aid <- NULL
govt_aid_pop <- rename(govt_aid_pop, c("democracy_aid_final" = "democracy_aid"))

#we can also remove pop
govt_aid_pop$pop <- NULL

#and reorder data so that it matches savun's data
govt_aid_pop <- govt_aid_pop[c("year", "recipient_country", "democracy_aid", "cowcode")]



#let's deflate original savun data (the 2005 dollars to 2015 dollars)
#first, get the data from savun
savun_democracy_aid <- Savun_Tirone_2011_AJPS_Replication_Dataset

savun_democracy_aid <- dplyr::select(savun_democracy_aid, year, recipient_country, democracy_aid, recipient_cowcode)
#we now have the same columns as the oecd data

savun_democracy_aid_copy <- savun_democracy_aid
savun_democracy_aid <- savun_democracy_aid_copy
#just creating a copy in case

#First we should deflate savun data (2005 usd) to 2015 usd (oecd data).
#deflation values were found in oecd guide on deflating data, to turn 2005 dollars to 2015 dollars, the factor is 1.06
savun_democracy_aid$democracy_aid <- savun_democracy_aid$democracy_aid*1.06

#~~ (she actually didn't lag all the data, she just lead the conflict_ini, so the rest were untouched)
#these are already lagged one year, so we should lead it by one year for the two dataframes to match
#savun_democracy_aid <- slide(savun_democracy_aid, Var = "democracy_aid", GroupVar = "recipient_country", slideBy = 1)
#this creates a new column with the led variables

#now we can removed the original lagged column for democracy aid
#savun_democracy_aid$democracy_aid <- NULL

#change democracy_aid1 to democracy_aid, and recipient_cowcode to cowcode
#savun_democracy_aid <- rename(savun_democracy_aid, c("democracy_aid1"="democracy_aid", "recipient_cowcode"="cowcode"))
#~~~

#change recipient_cowcode to cowcode
savun_democracy_aid <- rename(savun_democracy_aid, c("recipient_cowcode"="cowcode"))


#reorder variables
savun_democracy_aid <- savun_democracy_aid[,c("year", "recipient_country", "democracy_aid", "cowcode")]

#just leaving the data from 2001 or earlier to merge with the oecd data
savun_democracy_aid <- filter(savun_democracy_aid, year < 2002)

#change vector classes
savun_democracy_aid$year <- as.numeric(savun_democracy_aid$year)
savun_democracy_aid$recipient_country <- as.character(savun_democracy_aid$recipient_country)
savun_democracy_aid$democracy_aid <- as.numeric(savun_democracy_aid$democracy_aid)


#now we add the savun data and oecd data together
combined_democracy_aid <- bind_rows(savun_democracy_aid, govt_aid_pop)

#order by country and year
combined_democracy_aid <- arrange(combined_democracy_aid, recipient_country, year)

#there are blank countries, so they were removed
combined_democracy_aid <- filter(combined_democracy_aid, ! recipient_country %in% remove)

#there are countries are only on one list (savun data), I just keep it like that for now
#all the countries are still in the subet of countries in savun's original data

#check the number of countries
#length(unique(combined_democracy_aid$cowcode))
#there are 178 countries so only 1 is missing
#however some are only in the 1990-2001 data and some are only in the 2002-2014 data

#change name for merge
democracy_aid_final <- combined_democracy_aid









#affinity_scores
affinity_2015 <- read_dta("Dyadicdata.dta")

#the order of the country pairs don't matter, it results in the same affinity score (there are two countries, ccode2 and ccode1)
#let's choose all the ccode1=2 (US)
affinity_us2015 <- filter(affinity_2015, ccode1 == 2)



#let's choose the years and countries that we are interested in
#affinity_us2015 <- affinity_us2015[affinity_us2015$year %in% years,]
affinity_us2015 <- affinity_us2015[affinity_us2015$ccode2 %in% cow_code,]
affinity_us2015 <- arrange(affinity_us2015, ccode2, year)

#savun lags affinity score by 5 years, so I do the same
affinity_us2015 <- slide(affinity_us2015, Var = "s3un", GroupVar = "ccode2", slideBy = -5)

#now we can select the years we want
affinity_us2015 <- affinity_us2015[affinity_us2015$year %in% years,]

#check how many countries are included
length(unique(affinity_us2015$ccode2))
#there are 165 countries, so only 14 from savun's data are missing

#select only the variables that we need
#savun used 3 category data (with 3 outcomes) so s3un
#I also got the ideal scores and the absolute ideal differences just in case I have time
affinity_us2015 <- dplyr::select(affinity_us2015, ccode2, year, s3un, ideal2, absidealdiff)

#change the name ccode2 to cowcode for merging
affinity_us2015 <- rename(affinity_us2015, c("ccode2"="cowcode"))

#change df name for merge
affinity_final <-affinity_us2015

#looks good









#conflict initiation
#the cow codes are also used here (called gwno)

ucdp_2014 <- read_csv("ucdp-onset-conf-2014.csv")
View(ucdp_2014)

ucdp_onset_2014 <- ucdp_2014

#let's choose the countries that we are interested in
ucdp_onset_2014 <- ucdp_onset_2014[ucdp_onset_2014$gwno %in% cow_code,]
#keep the years for now so that we can create peace years

#check the number of countries
length(unique(ucdp_onset_2014$gwno))
#there are 148 countries so 31 countries are missing

#out of 3689 obs, there are only 69 cases of onset and 158 cases of conflict (or new conflict) after more than 2 years of peace
#so I will keep both variables just in case
table(ucdp_onset_2014$newconflictinyearv414)
table(ucdp_onset_2014$onset2v414)
table(ucdp_onset_2014$incidencev414)

#let's select the variables we need
#incidencev412 is whether there is a conflict, 
#onset2cv412 is whether a conflict happened after two years of no conflict (can be initiation or not)
#newconflictinyearv412 is whether there was a new conflict initiated (what we are looking for to create dependent)
ucdp_onset_2014 <- dplyr::select(ucdp_onset_2014, year, gwno, incidencev414, newconflictinyearv414, onset2v414)

#conflict initiation data used is tricky (the name is misleading too)
#it is a measure of conflict incidence that happen after at least two years since a initiation (as defined by UCDP)
#incidence = 1 & conflict_ini (for previous year)= 0 & conflict_ini (for two years prior) = 0
#I lag the conflict initiation data twice (one for 1 year lag, other for 2 year lag)
ucdp_onset_2014 <- slide(ucdp_onset_2014, Var = "newconflictinyearv414", GroupVar = "gwno", slideBy = -1)
ucdp_onset_2014 <- slide(ucdp_onset_2014, Var = "newconflictinyearv414", GroupVar = "gwno", slideBy = -2)

#create prior year conflict variable by lagging incidencev414
ucdp_onset_2014 <- slide(ucdp_onset_2014, Var = "incidencev414", GroupVar = "gwno", slideBy = -1)

#creating our conflict initiation (defined by savun) data
ucdp_onset_2014$conflict_ini <- 0
ucdp_onset_2014$conflict_ini[which(ucdp_onset_2014$newconflictinyearv414==1 & ucdp_onset_2014$`newconflictinyearv414-1`==0 &
                                     ucdp_onset_2014$`newconflictinyearv414-2`==0)] <- 1

table(ucdp_onset_2014$conflict_ini)
#remove the lagged vectors
ucdp_onset_2014$`newconflictinyearv414-1` <- NULL
ucdp_onset_2014$`newconflictinyearv414-2` <- NULL

#need to make peace years data, this counts the years of peace since the last initiation (not conflict)
#the btscs function creates a new df with the peace years in it as "spell"
ucdp_onset_2014_pyears <- btscs(ucdp_onset_2014, "incidencev414", "year", "gwno")

#I attached the spell column to our ucdp dataset
ucdp_onset_2014$peaceyears <- ucdp_onset_2014_pyears$spell

#now we can select our years of interest
ucdp_onset_2014 <- ucdp_onset_2014[ucdp_onset_2014$year %in% years,]


#splines and time cubed

#using the peace years as a duration term, we create splines to account for the autocorrelation due to time effects
#natural splines with b-spline basis with equally-spaced knots have been recommended by some
#others say that we should adjust the knots by looking at fit- but to do this we have to construct the whole dataset, so this will come later
#time cubed is another method suggested by carter

#natural splines, with three knots at quantiles (knots are df - 1, when df is provided)
basis_spline <- (ns(ucdp_onset_2014$peaceyears, df=4))

#as an aside, the below shows that ns() with degrees and no intercept does remove the constant as required by carter
#only basis_spline3 has an extra spline basis. basis_spline and basis_spline2 are the same (knots for basis_spline2 are from basis_spline)
basis_spline <- (ns(ucdp_onset_2014$peaceyears, df=4))
basis_spline2 <- (ns(ucdp_onset_2014$peaceyears, knots = c(2,15,32.75)))
basis_spline3 <- (ns(ucdp_onset_2014$peaceyears, knots = c(2,15,32.75), intercept = T))

#merge the ucdp dataset with the spline basis matrix
ucdp_onset_2014 <- cbind(ucdp_onset_2014,basis_spline)

#rename the spline base vectors and other columns for readability
ucdp_onset_2014 <- rename(ucdp_onset_2014, c("1"="pyspline_1", "2"="pyspline_2", "3"="pyspline_3", "4"="pyspline_4",
                                             "gwno"= "cowcode", "incidencev414"= "v_incidence_nl", "incidencev414-1"="v_incidence", "newconflictinyearv414"= "conflict_onset_ucdp",
                                             "onset2v414"="v_incidence2yr"))

#change name for merge
ucdp_final <- ucdp_onset_2014

#looks good






#WDI dataset: Inflation and women in parliament (%)
#merged with Paxton's dataset for WIP for the earlier years (WDI data has spotty observation before the early 2000s)


wdi_orginal <- read_csv("WDI_Data_1960-2017.csv")
wdi <- wdi_orginal
wdi[wdi==".."] <- NA #label NAs correctly
wdi$`Series Code` <- NULL #series code is redundant


#we need to remove the last 5 empty rows
nwdi<-dim(wdi)[1] #get the number of rows
wdi<-wdi[1:(n-5),]


#changing year column names to just contain the years in numeric form
names(wdi)[-(1:3)] = gsub( ".{8}$" , replacement = "", x = names(wdi)[-(1:3)])

#rearranging data to tidy format
wdi <- gather(wdi, year, value, 4:61)
wdi <- spread(wdi, `Series Name`, value)

#change year to numeric
wdi$year <- as.numeric(wdi$year)

#dataset with the years we are interested in
wdi<- wdi[wdi$year %in% years, ]


#here countries are coded by ISO, while savun's original data uses COW
#we need to translate between the two
wdi_cowcode <- countrycode(wdi$`Country Code`, "iso3c", "cown", warn = TRUE)
wdi <- cbind(wdi_cowcode, wdi)

#before we just look at the recipients, I also make a separate dataset for the donors
wdi_donors <- wdi[wdi$`Country Code` %in% donors,] 
wdi_donors <- wdi_donors[wdi_donors$year %in% c(2003:2016),]
wdi_donors <- rename(wdi_donors, c("wdi_cowcode"="cowcode",
                                   "Proportion of seats held by women in national parliaments (%)" = "women_parliament"))
wdi_donors <- dplyr::select(wdi_donors, cowcode, year, women_parliament)
#creating cow codes for WDI and binding the cow codes to the WDI dataframe
#now we can choose just the countries we need
wdi <- wdi[wdi$wdi_cowcode %in% cow_code,] 
#creating a new dataset with the years and countries we are interested in

#let's see how many countries are included here
length(unique(wdi$`Country Code`))
#there are 164, which means that  15 countries are missing in the WDI data

#let's check which countries are missing
missing_ccode_wdi <- setdiff(cow_code, unique(wdi$wdi_cowcode))
#this gives me the names of the countries that are missing from Polity

#rename columns for better merging and readability

#now let's select just the variables we are interested in so that we can use it to build our dataset
#I will use the consumer price inflation until I find a reason to use another measure of inflation

#rename columns for better merging and readability
wdi_final <- rename(wdi, c("wdi_cowcode"="cowcode", 
                           "Inflation, consumer prices (annual %)" = "consumer_inflation",
                           "Proportion of seats held by women in national parliaments (%)" = "women_parliament",
                           "Country Name" = "country",
                           "Country Code" = "countrycode"))

#change women_parliament to numeric data
wdi_final$women_parliament <- as.numeric(wdi_final$women_parliament)

#this has both inflactio and WIP. I will separate the 2 later
wdi_both <- dplyr::select(wdi_final, cowcode, countrycode, country, year, consumer_inflation, women_parliament)

#looks good for the WDI part

#~~~~~~


#now to paxton data 1945-2003
paxton_wipdata <- read_dta("C:/Users/Spark/Desktop/Winter_2018/poli_666/Research/analysis/paxton_wipdata.dta")
paxton_wip <- paxton_wipdata

#label NAs correctly (-88 and -99 are missing data)
paxton_wip[paxton_wip=="-88"] <- NA
paxton_wip[paxton_wip=="-99"] <- NA

#we don't need the last dozen-ish columns after the years
paxton_wip <- paxton_wip[,1:which(colnames(paxton_wip)=="P2003")]

#we also don't need some columns in the beginning
paxton_wip <- paxton_wip[ , !(colnames(paxton_wip) %in% c("CASEID", "REGIONFULL", "REGIONCONCISE"))]


##we need to make the years to a numeric value and rearrange to country-year format
#first, fix the year values (remove the character "p" in all the column names)
paxtonyrname <- (which(colnames(paxton_wip)=="P1945"): which(colnames(paxton_wip)=="P2003"))

names(paxton_wip)[-(1:(min(paxtonyrname)-1))] <- gsub( "P", replacement = "",
                                                       x = names(paxton_wip)[-(1:(min(paxtonyrname)-1))])

#then, rearrange the structure
paxton_wip <- gather(paxton_wip, year, women_parliament, paxtonyrname)

#name year a numeric value
paxton_wip$year <- as.numeric(paxton_wip$year)

#dataset with the years we are interested in
#since we will merge this with WDI data, we end at from 2002
paxtonyears <- c(1989:2002)

paxton_wip<- paxton_wip[paxton_wip$year %in% paxtonyears, ]


#here countries are coded by UNID, while savun's original data uses COW
#we need to translate between the two
paxton_cowcode <- countrycode(paxton_wip$UNID, "un", "cown", warn = TRUE)
paxton_wip <- cbind(paxton_cowcode, paxton_wip)
#creating cow codes for paxton and binding the cow codes to the WDI dataframe

#before we just extract the recipients, we get the donor data for the IV's
donors_cowcode <- countrycode(donors, "iso3c", "cown", warn = TRUE)
paxton_donors <- paxton_wip[paxton_wip$paxton_cowcode %in% donors_cowcode, ]
paxton_donors <- rename(paxton_donors, c("paxton_cowcode"="cowcode"))
paxton_donors <-  dplyr::select(paxton_donors, cowcode, year, women_parliament)

#now we can choose just the countries we need
paxton_wip <- paxton_wip[paxton_wip$paxton_cowcode %in% cow_code,] 
#creating a new dataset with the years and countries we are interested in

#let's see how many countries are included here
length(unique(paxton_wip$paxton_cowcode))
#there are 161, which means that  18 countries are missing in the WDI data

#let's check which countries are missing
missing_ccode_paxton <- setdiff(cow_code, unique(paxton_wip$paxton_cowcode))
#this gives me the names of the countries that are missing from Polity

#rename the cowcode column
paxton_final <- rename(paxton_wip, c("paxton_cowcode"="cowcode"))


#merge WDI and Paxton
#I think it will be best if I just make two datasets, one for inflation and one for WIP

wdi_wip <- dplyr::select(wdi_both, cowcode, year, women_parliament)

#for wdi, we need to select only years from 2003-2016, and only the countries in both datasets
wdiwipyears <- c(2003:2016)
wdi_wip<- wdi_wip[wdi_wip$year %in% wdiwipyears, ]

#paxton wip data
paxton_final <-  dplyr::select(paxton_final, cowcode, year, women_parliament)

#WDI inflation data
inflation_final <- dplyr::select(wdi_both, cowcode, year, consumer_inflation)

#now we can bind all the wip data
combined_wip <- bind_rows(wdi_wip, paxton_final)

#order by country and year
combined_wip <- arrange(combined_wip, cowcode, year)

#and also only select countries in both datasets (so that I can easily make donor wip column)
wip_cowcode <- intersect(unique(wdi_wip$cowcode), unique(paxton_final$cowcode))
combined_wip <- combined_wip[combined_wip$cowcode %in% wip_cowcode, ]

#check the number of countries
length(unique(combined_wip$cowcode))
#there are 161 countries so 18 is missing

#~~~~
#lastly, donor wip can also be calculated through wip data
#we need the logged average of annual sweden,us,and france 1989-2014 (then repeat the data)

wdi_donors$women_parliament <- as.numeric(wdi_donors$women_parliament)

#create a separate dataframe just for the donors
donor_wip <- bind_rows(paxton_donors, wdi_donors)

#rearrange and calculate averages
donor_wip <- spread(donor_wip, cowcode, women_parliament)
donor_wip$donor_wip <- log(rowMeans(donor_wip[,-1]))

#we use the resulting vector to attach it to the pwt dataset
donor_wip_avg <- rep(donor_wip$donor_wip, 161)
#repeating the vector 146 (while keeping the order)
combined_wip <- cbind(combined_wip, donor_wip_avg)
#~~

#change name for merge
wip_final <- combined_wip






###### merge everything and export
#merge all the data

extended_data <- merge(ucdp_final, democracy_aid_final, by=c("cowcode","year"), all=TRUE) 
extended_data <- merge(extended_data, polity_2016_final, by=c("cowcode","year"), all=TRUE) 
extended_data <- merge(extended_data, pennwt_9_final, by=c("cowcode","year"), all=TRUE) 
extended_data <- merge(extended_data, affinity_final, by=c("cowcode","year"), all=TRUE)
extended_data <- merge(extended_data, inflation_final, by=c("cowcode","year"), all=TRUE)
extended_data <- merge(extended_data, wip_final, by=c("cowcode","year"), all=TRUE)


#rename some variables
extended_data <- rename(extended_data, c("s3un"="affinity_US", 
                                         "donor_gdp_vector"="donor_gdp",
                                         "donor_wip_avg" = "donor_wip"))

#also add regions 
extended_data$region <- countrycode(extended_data$cowcode, "cown", "region", warn = TRUE)

#add country name based on cowcode
extended_data$country_name <- countrycode(extended_data$cowcode, "cown", "country.name", warn = TRUE)


#moving country_name to the first column for better viewing
extended_data <- extended_data[,c(ncol(extended_data),1:(ncol(extended_data)-1))]

#remove some extra columns (country names)
extra_columns <- c("scode", "country.x", "countrycode", "country.y", "cyear", "recipient_country")
extended_data <- extended_data[ , !(names(extended_data) %in% extra_columns)]


#lead conflict_ini by one year to have the effect of lagging all other variables
extended_data<- slide(extended_data, Var = "conflict_ini", GroupVar = "cowcode", slideBy = 1)

#do the same to other measures of conflict initiations
extended_data<- slide(extended_data, Var = "v_incidence2yr", GroupVar = "cowcode", slideBy = 1)
extended_data<- slide(extended_data, Var = "conflict_onset_ucdp", GroupVar = "cowcode", slideBy = 1)

#rearrange order of data
extended_data <- extended_data[,c(1:7, ncol(extended_data)-2,ncol(extended_data)-1, ncol(extended_data),8:(ncol(extended_data)-3))]

#add dummy variable for after 2002
extended_data$dummy2002 <- 0
extended_data$dummy2002[which(extended_data$year >= 2002)] <- 1

#forgot to add t,t^2,t^3, I divided the cubed term by 1000 to avoid numeric instability as recommended by carter
extended_data$py_squared <- extended_data$peaceyears^2
extended_data$py_cubed <- extended_data$peaceyears^3*0.001


#I rename variables so that they are similar to the replication formula (new df just in case)
extended_data1 <- rename(extended_data, c("conflict_ini1"="fconflict_initiation", "peaceyears"="peace_years", 
                                          "polity1_dem"="democratization1", "polity1_dem_ord" = "democratization1_ord",
                                          "polity"="democracy", "rgdppc_growth"="growth_real_gdppc", "rgdp_pc"="real_gdppc",
                                          "v_incidence" = "conflict_prioryear", "v_incidence_nl" ="conflict_prioryear_nl" ))

#and adjust the years to 1990-2015

years2 <- c(1990:2014)
extended_data1 <- extended_data1[extended_data1$year %in% years2,]


#looks good!


#let's export this to stata for use
write.dta(extended_data1, "C:/Users/Spark/Desktop/Winter_2018/ORGB_708/research/researchproj/extended_data_0401.dta")




