

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
               gdata,
               tidyr,
               lfe,
               sandwich,
               reshape,
               igraph)

####### TABLE 1 ######
### Descriptive statistics ###

mergepdata <- read.csv("UNGA_fulldata.csv", stringsAsFactors = F)


mergepdata_desc <- dplyr::select(mergepdata, 
                                 aidflow_corrected,
                                 s3un, 
                                 ipfd, 
                                 r_conflict_prioryear,
                                 r_politychange,
                                 democratization,
                                 r_democracy,
                                 r_gdp_growth,
                                 d_gdp_growth,
                                 r_gdplog, 
                                 d_gdplog,
                                 r_population,
                                 d_population,
                                 d_women_parliament,
                                 comlang_off,
                                 colony,
                                 distlog,
                                 ptacount_1,
                                 intervention_1,
                                 r_unemployment_1,
                                 d_unemployment_1,
                                 unempdiff_1,
                                 logimmigration_1,
                                 logrtod_export_1,
                                 logdtor_export_1)

stargazer(as.data.frame(mergepdata_desc),
          type = "text",
          covariate.labels = c("Aidflow (per 1000, logged)",
                               "Ideal Point Difference",
                               "Δ Ideal Point Difference",
                               "Conflict, Prior Year, R",
                               "Democratization (ord), R",
                               "Democratization (dummy), R",
                               "Democracy (ord), R",
                               "GDP growth, R",
                               "GDP growth, D",
                               "GDP (logged), R",
                               "GDP (logged), D",
                               "Population (logged), R",
                               "Population (logged), D",
                               "Women in Parliament (%), D",
                               "Common Official Lang.",
                               "Past Colonial Relationship",
                               "Distance (km, logged)",
                               "# of PTAs",
                               "Military Intervention (dummy)",
                               "Unemployment Rate, R",
                               "Unemployment Rate, D",
                               "Unemployment Difference",
                               "Immigration (logged), R to D",
                               "Export (logged), R to D",
                               "Export (logged), D to R"),
          title = "Descriptive Statistics, 1991 - 2014")



######### Models ########


####Model 1- no controls ####
### Model 1b 
mergepdata$ccode1 <- as.factor(mergedata$ccode1)
mergepdata$ccode2 <- as.factor(mergedata$ccode2)
mergepdata$year <- as.factor(mergedata$year)


#let's just use lm() for now
#model1 <- felm(aidflow_corrected ~ s3un_1 | ccode1 + ccode2 + year | 0 | ccode1 + ccode2, data = mergepdata)

model1a <- lm(aidflow_corrected ~ 
               s3un_5 + 
               ccode1 + 
               ccode2 + 
               as.factor(year), 
             data = mergepdata,
             na.action = na.omit)

m1ahc3 <- coeftest(model1a, cluster.vcov(model1a,
                                       cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                       leverage = 3, 
                                       df_correction = T)) #[-(3:nrow(summary(model1a)$coefficients)),]; m1ahc3
#leverage = 3 means HC3



### Model 1b

model1b <- lm(aidflow_corrected ~ 
               s3un_1 + 
               ccode1 + 
               ccode2 + 
               as.factor(year), 
             data = mergepdata,
             na.action = na.omit)

m1bhc3 <- coeftest(model1b, cluster.vcov(model1b,
                                       cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                       leverage = 3, 
                                       df_correction = T)) #[-(3:nrow(summary(model1b)$coefficients)),]; m1bhc3



### Model 1c

model1c <- lm(aidflow_corrected ~ 
                s3un_3 + 
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.omit)

m1chc3 <- coeftest(model1c, cluster.vcov(model1c,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(3:nrow(summary(model1c)$coefficients)),]; m1chc3


stargazer(m1ahc3,
          m1bhc3, 
          m1chc3, 
          type = "text", 
          omit = c("ccode", "19", "20"))
#good
#potential for reverse causality


######## Model 2 - with controls #######


#To build the model, let's look at the correlation of variables
corrvar <- select(mergepdata,
                  s3un, 
                  ipfd, 
                  r_conflict_prioryear,
                  r_politychange,
                  r_democracy,
                  democratization,
                  r_gdp_growth,
                  d_gdp_growth,
                  r_gdplog, 
                  d_gdplog,
                  r_population,
                  d_population,
                  d_women_parliament,
                  comlang_off,
                  colony,
                  distlog,
                  ptacount_1,
                  intervention_1,
                  r_unemployment_1,
                  d_unemployment_1,
                  unempdiff_1,
                  logimmigration_1,
                  logrtod_export_1,
                  logdtor_export_1) 

corrtbl <- cor(corrvar, use = "complete.obs")

png(filename="corrplot.png",
    units="in", 
    width=9, 
    height=9, 
    res=144)
corrplot(corrtbl)
dev.off()
#this shows that just unempdiff should be used and not in conjunction with donor/recipient unemployment rates



### Model2a
#according Menard, it seems that immigration and unemployment may have an interactive effect
  #not really, actually

model2a <- lm(aidflow_corrected ~ 
                s3un_5 + 
                ipfd_5+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
    #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.omit)

m2ahc3 <- coeftest(model2a, cluster.vcov(model2a,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model2a)$coefficients)),]; m2ahc3




### Model2b
model2b <- lm(aidflow_corrected ~ 
                s3un_1 + 
                ipfd_1+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.omit)

m2bhc3 <- coeftest(model2b, cluster.vcov(model2b,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model2b)$coefficients)),]; m2bhc3


### Model2c
model2c <- lm(aidflow_corrected ~ 
                s3un_3 + 
                ipfd_3+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.omit)

m2chc3 <- coeftest(model2c, cluster.vcov(model2c,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))# [-(24:nrow(summary(model2b)$coefficients)),]; m2chc3


######## Table 2 ############
stargazer(m2ahc3,
          m2bhc3, 
          m2chc3,
          covariate.labels = c("Ideal Point Diff. (5 year lag)",
                               "Ideal Point Diff. (1 year lag)",
                               "Ideal Point Diff. (3 year lag)",
                               "FD Ideal Point Diff. (5 year lag)",
                               "FD Ideal Point Diff. (1 year lag)",
                               "FD Ideal Point Diff. (3 year lag)",
                               "Conflict, Prior Year, R",
                               "Democratization (ord), R",
                               "Democracy (ord), R",
                               "GDP growth, R",
                               "GDP growth, D",
                               "GDP (logged), R",
                               "GDP (logged), D",
                               "Population (logged), R",
                               "Population (logged), D",
                               "Women in Parliament (%), D",
                               "Common Official Lang.",
                               "Past Colonial Relationship",
                               "Distance (km, logged)",
                               "# of PTAs",
                               "Military Intervention (dummy)",
                               "Unemployment Difference",
                               "Immigration (logged), R to D",
                               "Export (logged), R to D",
                               "Export (logged), D to R"),
          type = "latex", 
          omit = c("ccode", "19", "20", "unemployment"),
          title = "Linear Regression with Two-way Clustered Errors: Different Lags of I.P. Difference",
          notes = c("With country and year fixed effects", 
                    "Robust clustered standard errors (clusters: Recipient countries, Donor Countries)"))

#since only the first lag is significant and positive, there is somewhat suspicion of reverse causality



####### Model 3 - Reverse Causality #######

#### Model 3a
model3a <- lm(s3un ~ 
                aidflow_corrected_5 + 
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.exclude)

m3ahc3 <- coeftest(model3a, cluster.vcov(model3a,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))#[-(23:nrow(summary(model3a)$coefficients)),]; m3ahc3


#### Model 3b
model3b <- lm(s3un ~ 
                aidflow_corrected_1 + 
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.exclude)

m3bhc3 <- coeftest(model3b, cluster.vcov(model3b,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))#[-(23:nrow(summary(model3b)$coefficients)),]; m3bhc3



### Model 3c


model3c <- lm(s3un ~ 
                aidflow_corrected_3 + 
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.exclude)

m3chc3 <- coeftest(model3c, cluster.vcov(model3c,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))#[-(23:nrow(summary(model3c)$coefficients)),]; m3chc3



################# TABLE 3 ####################

stargazer(m3ahc3,
          m3bhc3, 
          m3chc3,
          covariate.labels = c("Democracy Aid (logged, 5 year lag)",
                               "Democracy Aid (logged, 1 year lag)",
                               "Democracy Aid (logged, 3 year lag)",
                               "Conflict, Prior Year, R",
                               "Democratization (ord), R",
                               "Democracy (ord), R",
                               "GDP growth, R",
                               "GDP growth, D",
                               "GDP (logged), R",
                               "GDP (logged), D",
                               "Population (logged), R",
                               "Population (logged), D",
                               "Women in Parliament (%), D",
                               "Common Official Lang.",
                               "Past Colonial Relationship",
                               "Distance (km, logged)",
                               "# of PTAs",
                               "Military Intervention (dummy)",
                               "Unemployment Difference",
                               "Immigration (logged), R to D",
                               "Export (logged), R to D",
                               "Export (logged), D to R"),
          type = "latex", 
          omit = c("ccode", "19", "20", "unemployment"),
          title = "Reverse Causality Check: Different Lags of Democracy Aid",
          notes = c("With country and year fixed effects", 
                    "Robust clustered standard errors (clusters: Recipient Countries, Donor Countries)"))


## Interesting that, immigration has no effect here



##############################################
### Misc models ###

#let's look at the graphs





#Just use 5th lag for s3un
#1st lag for aid

##### Model 4- like model 2 but by recipient/donor continent

#list of continents included
unique(mergedata$continent1)
unique(mergedata$continent2)

table(mergedata$continent1)
table(mergedata$continent2)

## Model 4a - rec: Africa (most obs)
model4a <- lm(aidflow_corrected ~ 
                s3un_5 + 
                ipfd_5+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                #logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = filter(mergepdata, continent2 == "Africa"),
              na.action = na.omit)

m4ahc3 <- coeftest(model4a, cluster.vcov(model4a,
                                         cluster = cbind(filter(mergepdata, continent2 == "Africa")$ccode1,
                                                         filter(mergepdata, continent2 == "Africa")$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model4a)$coefficients)),]; m4ahc3
# I have to remove immigration for singularity issues. (possibly due to high correlation, but I can't find any)


## Model 4b - rec: Asia (2nd most obs)
model4b <- lm(aidflow_corrected ~ 
                s3un_5 + 
                ipfd_5+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = filter(mergepdata, continent2 == "Asia"),
              na.action = na.omit)

m4bhc3 <- coeftest(model4b, cluster.vcov(model4b,
                                         cluster = cbind(filter(mergepdata, continent2 == "Asia")$ccode1,
                                                         filter(mergepdata, continent2 == "Asia")$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model4b)$coefficients)),]; m4bhc3

## Model 4c - rec: no Iraq
model4c <- lm(aidflow_corrected ~ 
                s3un_5 + 
                ipfd_5+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata_noirq,
              na.action = na.omit)

m4chc3 <- coeftest(model4c, cluster.vcov(model4c,
                                         cluster = cbind(mergepdata_noirq$ccode1,
                                                         mergepdata_noirq$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model4c)$coefficients)),]; m4chc3

#now s3un is significant and in the right direction
#interesting



## Model 5a - don: Europe (most obs)
model5a <- lm(aidflow_corrected ~ 
                s3un_5 + 
                ipfd_5+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = filter(mergepdata, continent1 == "Europe"),
              na.action = na.omit)

m5ahc3 <- coeftest(model5a, cluster.vcov(model5a,
                                         cluster = cbind(filter(mergepdata, continent1 == "Europe")$ccode1,
                                                         filter(mergepdata, continent1 == "Europe")$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model5a)$coefficients)),]; m5ahc3


## Model 5b - don: Americas (2nd most obs) 
  #has US (country that bases aid off of UNGA votes)
model5b <- lm(aidflow_corrected ~ 
                s3un_5 + 
                ipfd_5+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = filter(mergepdata, continent1 == "Americas"),
              na.action = na.omit)

m5bhc3 <- coeftest(model5b, cluster.vcov(model5b,
                                         cluster = cbind(filter(mergepdata, continent1 == "Americas")$ccode1,
                                                         filter(mergepdata, continent1 == "Americas")$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model5b)$coefficients)),]; m5bhc3


## Model 5c - don: US) 
#US (country that bases aid off of UNGA votes)
model5c <- lm(aidflow_corrected ~ 
                s3un_5 + 
                ipfd_5+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                #d_gdp_growth +
                r_gdplog +
                #d_gdplog +
                r_population +
                #d_population +
                #d_women_parliament +
                #comlang_off+
                #colony +
                #distlog+
                ptacount_1 +
                intervention_1+
                #r_unemployment_1 +
                #d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                #ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = filter(mergepdata, ccode1 == 2),
              na.action = na.omit)

m5chc3 <- coeftest(model5c, cluster.vcov(model5c,
                                         cluster = cbind(#filter(mergepdata, ccode1 == 2)$ccode1,
                                                         filter(mergepdata, ccode1 == 2)$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model5c)$coefficients)),]; m5chc3


##################### TABLE 4 #####################

fit.m2a <- summary(model2a, diagnostics = T)
fit.m4a <- summary(model4a, diagnostics = T)
fit.m4b <- summary(model4a, diagnostics = T)
fit.m4c <- summary(model4a, diagnostics = T)
fit.m5a <- summary(model5a, diagnostics = T)
fit.m5b <- summary(model5a, diagnostics = T)
fit.m5c <- summary(model5a, diagnostics = T)


add.lines = list(
  c(diagnostics[1], round(c(fit.m2a$r.squared,
                            fit.m4a$r.squared,
                            fit.m4b$r.squared,
                            fit.m4c$r.squared,
                            fit.m5a$r.squared,
                            fit.m5b$r.squared,
                            fit.m5c$r.squared),
                          2)),
  c(diagnostics[2], round(c(fit.m2a$fstatistic[1],
                            fit.m4a$fstatistic[1],
                            fit.m4b$fstatistic[1],
                            fit.m4c$fstatistic[1],
                            fit.m5a$fstatistic[1],
                            fit.m5b$fstatistic[1],
                            fit.m5c$fstatistic[1]),
                          2)),
  c(diagnostics[3], nrow(mergepdata) - nrow(as.matrix(fit.m2a$na.action)),
    nrow(filter(mergepdata, continent2 == "Africa")) - nrow(as.matrix(fit.m4a$na.action)),
    nrow(filter(mergepdata, continent2 == "Asia")) - nrow(as.matrix(fit.m4b$na.action)),
    nrow(mergepdata_noirq) - nrow(as.matrix(fit.m4c$na.action)),
    nrow(filter(mergepdata, continent1 == "Europe")) - nrow(as.matrix(fit.m5a$na.action)),
    nrow(filter(mergepdata, continent1 == "Americas")) - nrow(as.matrix(fit.m5b$na.action)),
    nrow(filter(mergepdata, ccode1 == 2)) - nrow(as.matrix(fit.m5c$na.action)))))


stargazer(m2ahc3,
          m4ahc3,
          m4bhc3, 
          m4chc3, 
          m5ahc3,
          m5bhc3, 
          m5chc3,
          type = "latex",
          covariate.labels = c("Ideal Point Diff. (5 year lag)",
                               "Δ Ideal Point Diff. (5 year lag)",
                               "Conflict, Prior Year, R",
                               "Democratization (ord), R",
                               "Democracy (ord), R",
                               "GDP growth, R",
                               "GDP growth, D",
                               "GDP (logged), R",
                               "GDP (logged), D",
                               "Population (logged), R",
                               "Population (logged), D",
                               "Women in Parliament (%), D",
                               "Common Official Lang.",
                               "Past Colonial Relationship",
                               "Distance (km, logged)",
                               "# of PTAs",
                               "Military Intervention (dummy)",
                               "Unemployment Rate, D",
                               "Unemployment Difference",
                               "Immigration (logged), R to D",
                               "Export (logged), R to D",
                               "Export (logged), D to R"),
          column.labels = c("All", "Africa", "Asia", "No Iraq", "Europe", "Americas", "USA"),
          title = "Linear Regression with Two-way Clustered Errors: By Region Subsamples",
          notes = c("All independent variables lagged 1 year unless otherwise specified",
                    "With country and year fixed effects", 
                    "Robust clustered standard errors -clusters: Recipient Countries, Donor Countries"),
          omit = c("ccode", "19", "20"))

#US or Canada (or one of them) are the ones that base their democracy aid on UNGA votes. But do to multicollinearity when 
  #looking at US and Canada separately, we can't run the same model on each

#HOWEVER, the sign is wrong. It seems that they give more aid to those who are different. This may be because they want to
  #persuade them. However, this goes against the US policy on aid.


##################################################################
##################################################################

#### Repeat without Iraq ####
## Plotting shows that Iraq is a special case 


######## Model 7 - with controls #######


#To build the model, let's look at the correlation of variables
corrvar_noirq <- select(mergepdata_noirq,
                        s3un, 
                        ipfd, 
                        r_conflict_prioryear,
                        r_politychange,
                        r_democracy,
                        r_gdp_growth,
                        d_gdp_growth,
                        r_gdplog, 
                        d_gdplog,
                        r_population,
                        d_population,
                        d_women_parliament,
                        comlang_off,
                        colony,
                        distlog,
                        ptacount_1,
                        intervention_1,
                        r_unemployment_1,
                        d_unemployment_1,
                        unempdiff_1,
                        logimmigration_1,
                        logrtod_export_1,
                        logdtor_export_1) 

corrtbl_noirq <- cor(corrvar_noirq, use = "complete.obs")
corrplot(corrtbl_noirq)
#this shows that just unempdiff should be used and not in conjunction with donor/recipient unemployment rates



### Model7a
#according Menard, it seems that immigration and unemployment may have an interactive effect
#not really, actually

model7a <- lm(aidflow_corrected ~ 
                s3un_5 + 
                ipfd_5+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata_noirq,
              na.action = na.omit)

m7ahc3 <- coeftest(model7a, cluster.vcov(model7a,
                                         cluster = cbind(mergepdata_noirq$ccode1, mergepdata_noirq$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model2a)$coefficients)),]; m2ahc3




### Model7b
model7b <- lm(aidflow_corrected ~ 
                s3un_1 + 
                ipfd_1+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata_noirq,
              na.action = na.omit)

m7bhc3 <- coeftest(model7b, cluster.vcov(model7b,
                                         cluster = cbind(mergepdata_noirq$ccode1, mergepdata_noirq$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model7b)$coefficients)),]; m7bhc3


### Model7c
model7c <- lm(aidflow_corrected ~ 
                s3un_3 + 
                ipfd_3+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata_noirq,
              na.action = na.omit)

m7chc3 <- coeftest(model7c, cluster.vcov(model7c,
                                         cluster = cbind(mergepdata_noirq$ccode1, mergepdata_noirq$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))# [-(74:nrow(summary(model7b)$coefficients)),]; m7chc3


######## Table 7 ############
stargazer(m2ahc3,
          m2bhc3, 
          m2chc3,
          m7ahc3,
          m7bhc3, 
          m7chc3, 
          type = "text", 
          omit = c("ccode", "19", "20"))

#since only the first lag is significant and positive, there is somewhat suspicion of reverse causality



####### Model 8 - Reverse Causality #######

#### Model 8a
model8a <- lm(s3un ~ 
                aidflow_corrected_5 + 
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata_noirq,
              na.action = na.exclude)

m8ahc3 <- coeftest(model8a, cluster.vcov(model8a,
                                         cluster = cbind(mergepdata_noirq$ccode1, mergepdata_noirq$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))#[-(23:nrow(summary(model8a)$coefficients)),]; m8ahc3


#### Model 8b
model8b <- lm(s3un ~ 
                aidflow_corrected_1 + 
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata_noirq,
              na.action = na.exclude)

m8bhc3 <- coeftest(model8b, cluster.vcov(model8b,
                                         cluster = cbind(mergepdata_noirq$ccode1, mergepdata_noirq$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))#[-(23:nrow(summary(model8b)$coefficients)),]; m8bhc3



### Model 8c


model8c <- lm(s3un ~ 
                aidflow_corrected_3 + 
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata_noirq,
              na.action = na.exclude)

m8chc3 <- coeftest(model8c, cluster.vcov(model8c,
                                         cluster = cbind(mergepdata_noirq$ccode1, mergepdata_noirq$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))#[-(23:nrow(summary(model8c)$coefficients)),]; m8chc3



################# TABLE 8 ####################

stargazer(m3ahc3,
          m3bhc3, 
          m3chc3,
          m8ahc3,
          m8bhc3, 
          m8chc3, 
          type = "text", 
          omit = c("ccode", "19", "20"))

############################################

#### Conclusion- the results with or without iraq are basically the same
  # let's just use the full data
  # Is this due to FE? I think so? But then why do people single regions out with FE- due to clustering?





############## ECM Models ##################

####### First, test for time dependence by adding lagged DV on RHS #####
  # We see if the lagged DV it NOT significant
  # Augmented Dickey-Fuller test

########## Model 9, like model 2 but with lagged DV on RHS #########

### Model9a
#according Menard, it seems that immigration and unemployment may have an interactive effect
#not really, actually

model9a <- lm(aidflow_corrected ~ 
                aidflow_corrected_1 +
                s3un_5 + 
                ipfd_5+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.omit)

m9ahc3 <- coeftest(model9a, cluster.vcov(model9a,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model9a)$coefficients)),]; m9ahc3


########## Real Dickey Fuller Test ##############

# Augmented Dickey-Fuller test

model9aa <- lm(d.aidflow_corrected ~ 
                 aidflow_corrected_1 +
                 d.aidflow_corrected_1 +
                 ccode1 + 
                 ccode2 + 
                 as.factor(year), 
               data = mergepdata,
               na.action = na.omit)

summary(model9aa)

m9aahc3 <- coeftest(model9aa, cluster.vcov(model9aa,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))[-(24:nrow(summary(model9aa)$coefficients)),]; m9aahc3

############ effect of lagged DV is very significant, so we do NOT have to run ECM ############



#####################################
##### Check for autocorrelation ######


model2aa <- lm(aidflow_corrected ~ 
                s3un_5 + 
                ipfd_5+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.exclude)

m2aahc3 <- coeftest(model2aa, cluster.vcov(model2aa,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model2a)$coefficients)),]; m2ahc3

#####
table(model2aa$residuals == resid(model2aa))


mergepdata$error <- resid(model2aa)
mergepdata$l.error <- ave(mergepdata$error,
                         mergepdata$ccode1,
                         mergepdata$ccode2,
                         FUN=function(x) lagpad(x, 1))


### Auxiliary regression

model2_aux <- lm(error ~ l.error,
                 data = mergepdata,
                 na.action = na.exclude)

m2auxhc3 <- coeftest(model2_aux, cluster.vcov(model2_aux,
                                           cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                           leverage = 3, 
                                           df_correction = T))

#the lagged error here is significant, so there IS autocorrelation
###########

#### Fixing autocorrelation
rho <- summary(model2_aux)$coefficients[2]


#get the lags of predicted variables and also rho transform all variables
mergepdata <- mergepdata %>%
  group_by(ccode1) %>%
  group_by(ccode2) %>%
  mutate(rho.aidflow_corrected = aidflow_corrected - rho*lagpad(aidflow_corrected,1)) %>%
  mutate(rho.s3un_5 = s3un_5 - rho*lagpad(s3un_5,1)) %>%
  mutate(rho.ipfd_5 = ipfd_5 - rho*lagpad(ipfd_5,1)) %>%
  mutate(rho.s3un_1 = s3un_1 - rho*lagpad(s3un_1,1)) %>%
  mutate(rho.ipfd_1 = ipfd_1 - rho*lagpad(ipfd_1,1)) %>%
  mutate(rho.s3un_3 = s3un_3 - rho*lagpad(s3un_3,1)) %>%
  mutate(rho.ipfd_3 = ipfd_3 - rho*lagpad(ipfd_3,1)) %>%
  mutate(rho.r_conflict_prioryear = r_conflict_prioryear - rho*lagpad(r_conflict_prioryear,1)) %>%
  mutate(rho.r_politychange = r_politychange - rho*lagpad(r_politychange,1)) %>%
  mutate(rho.r_democracy = r_democracy - rho*lagpad(r_democracy,1)) %>%
  mutate(rho.r_gdp_growth = r_gdp_growth - rho*lagpad(r_gdp_growth,1)) %>%
  mutate(rho.d_gdp_growth = d_gdp_growth - rho*lagpad(d_gdp_growth,1)) %>%
  mutate(rho.r_gdplog = r_gdplog - rho*lagpad(r_gdplog,1)) %>%
  mutate(rho.d_gdplog = d_gdplog - rho*lagpad(d_gdplog,1)) %>%
  mutate(rho.r_population = r_population - rho*lagpad(r_population,1)) %>%
  mutate(rho.d_population = d_population - rho*lagpad(d_population,1)) %>%
  mutate(rho.d_women_parliament = d_women_parliament - rho*lagpad(d_women_parliament,1)) %>%
  mutate(rho.comlang_off = comlang_off - rho*lagpad(comlang_off,1)) %>%
  mutate(rho.colony = colony - rho*lagpad(colony,1)) %>%
  mutate(rho.distlog = distlog - rho*lagpad(distlog,1)) %>%
  mutate(rho.ptacount_1 = ptacount_1 - rho*lagpad(ptacount_1,1)) %>%
  mutate(rho.intervention_1 = intervention_1 - rho*lagpad(intervention_1,1)) %>%
  mutate(rho.r_unemployment_1 = r_unemployment_1 - rho*lagpad(r_unemployment_1,1)) %>%
  mutate(rho.d_unemployment_1 = d_unemployment_1 - rho*lagpad(d_unemployment_1,1)) %>%
  mutate(rho.unempdiff_1 = unempdiff_1 - rho*lagpad(unempdiff_1,1)) %>%
  mutate(rho.logimmigration_1 = logimmigration_1 - rho*lagpad(logimmigration_1,1)) %>%
  mutate(rho.logrtod_export_1 = logrtod_export_1 - rho*lagpad(logrtod_export_1,1)) %>%
  mutate(rho.logdtor_export_1  = logdtor_export_1 - rho*lagpad(logdtor_export_1,1)) 

  
  
### Rho transformed model
modelrho <- lm(rho.aidflow_corrected ~ 
                 rho.s3un_5 + 
                 rho.ipfd_5 +
                 rho.r_conflict_prioryear +
                 rho.r_politychange +
                 rho.r_democracy +
                 rho.r_gdp_growth +
                 rho.d_gdp_growth +
                 rho.r_gdplog +
                 rho.d_gdplog +
                 rho.r_population +
                 rho.d_population +
                 rho.d_women_parliament +
                 rho.comlang_off+
                 rho.colony +
                 rho.distlog+
                 rho.ptacount_1 +
                 rho.intervention_1+
                 rho.d_unemployment_1 +
                 rho.unempdiff_1 +
                 rho.logimmigration_1 +
                 rho.logrtod_export_1 +
                 rho.logdtor_export_1 +
                 ccode1 + 
                 ccode2 + 
                 as.factor(year), 
               data = mergepdata,
               na.action = na.exclude)
summary(modelrho)

mrhohc3 <- coeftest(modelrho, cluster.vcov(modelrho,
                                              cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                              leverage = 3, 
                                              df_correction = T))


stargazer(mrhohc3, 
          type = "latex", 
          omit = c("ccode", "19", "20", "unemployment"),
          covariate.labels = c("Ideal Point Diff. (5 year lag)",
                               "Δ Ideal Point Diff. (5 year lag)",
                               "Conflict, Prior Year, R",
                               "Democratization (ord), R",
                               "Democracy (ord), R",
                               "GDP growth, R",
                               "GDP growth, D",
                               "GDP (logged), R",
                               "GDP (logged), D",
                               "Population (logged), R",
                               "Population (logged), D",
                               "Women in Parliament (%), D",
                               "Common Official Lang.",
                               "Past Colonial Relationship",
                               "Distance (km, logged)",
                               "# of PTAs",
                               "Military Intervention (dummy)",
                               #"Unemployment Rate, D",
                               "Unemployment Difference",
                               "Immigration (logged), R to D",
                               "Export (logged), R to D",
                               "Export (logged), D to R"),
          title = "Autoregression (AR(1)) Model with Two-way Clustered Errors",
          notes = c("All independent variables lagged 1 year unless otherwise specified",
                    "With country and year fixed effects", 
                    "Robust clustered standard errors (Clusters: Recipient Countries, Donor Countries)"),
          add.lines = list(
            c(diagnostics[1], round(c(fit.mrho$r.squared), 
                                    2)),
            c(diagnostics[2], round(c(fit.mrho$fstatistic[1]),
                                    2)),
            c(diagnostics[3], nrow(mergepdata) - nrow(as.matrix(fit.mrho$na.action)))))






############################################################
############################################################
## repeat with regions

mergepdata_noirq <- filter(mergepdata, ccode2 != 625 )
#do this to include rhos in the noirq dataset

##### Model 4- like model 2 but by recipient/donor continent

#list of continents included
unique(mergedata$continent1)
unique(mergedata$continent2)

table(mergedata$continent1)
table(mergedata$continent2)

## Model 4a - rec: Africa (most obs)
model4aa <- lm(aidflow_corrected ~ 
                 rho.s3un_5 + 
                 rho.ipfd_5+
                 rho.r_conflict_prioryear +
                 rho.r_politychange +
                 rho.r_democracy +
                 rho.r_gdp_growth +
                 rho.d_gdp_growth +
                 rho.r_gdplog +
                 rho.d_gdplog +
                 rho.r_population +
                 rho.d_population +
                 rho.d_women_parliament +
                 rho.comlang_off+
                 rho.colony +
                 rho.distlog+
                 rho.ptacount_1 +
                 rho.intervention_1+
                #r_unemployment_1 +
                 rho.d_unemployment_1 +
                 rho.unempdiff_1 +
                rho.logimmigration_1 +
                 rho.logrtod_export_1 +
                 rho.logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = filter(mergepdata, continent2 == "Africa"),
              na.action = na.omit)

m4aahc3 <- coeftest(model4aa, cluster.vcov(model4aa,
                                         cluster = cbind(filter(mergepdata, continent2 == "Africa")$ccode1,
                                                         filter(mergepdata, continent2 == "Africa")$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))#[-(24:nrow(summary(model4a)$coefficients)),]; m4aahc3
# I have to remove immigration for singularity issues. (possibly due to high correlation, but I can't find any)


## Model 4b - rec: Asia (2nd most obs)
model4ba <- lm(aidflow_corrected ~ 
                 rho.s3un_5 + 
                 rho.ipfd_5+
                 rho.r_conflict_prioryear +
                 rho.r_politychange +
                 rho.r_democracy +
                 rho.r_gdp_growth +
                 rho.d_gdp_growth +
                 rho.r_gdplog +
                 rho.d_gdplog +
                 rho.r_population +
                 rho.d_population +
                 rho.d_women_parliament +
                 rho.comlang_off+
                 rho.colony +
                 rho.distlog+
                 rho.ptacount_1 +
                 rho.intervention_1+
                #r_unemployment_1 +
                 rho.d_unemployment_1 +
                 rho.unempdiff_1 +
                 rho.logimmigration_1 +
                 rho.logrtod_export_1 +
                 rho.logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = filter(mergepdata, continent2 == "Asia"),
              na.action = na.omit)

m4bahc3 <- coeftest(model4ba, cluster.vcov(model4ba,
                                         cluster = cbind(filter(mergepdata, continent2 == "Asia")$ccode1,
                                                         filter(mergepdata, continent2 == "Asia")$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model4b)$coefficients)),]; m4bahc3

## Model 4c - rec: no Iraq
model4ca <- lm(aidflow_corrected ~ 
                 rho.s3un_5 + 
                 rho.ipfd_5+
                 rho.r_conflict_prioryear +
                 rho.r_politychange +
                 rho.r_democracy +
                 rho.r_gdp_growth +
                 rho.d_gdp_growth +
                 rho.r_gdplog +
                 rho.d_gdplog +
                 rho.r_population +
                 rho.d_population +
                 rho.d_women_parliament +
                 rho.comlang_off+
                 rho.colony +
                 rho.distlog+
                 rho.ptacount_1 +
                 rho.intervention_1+
                #r_unemployment_1 +
                 rho.d_unemployment_1 +
                 rho.unempdiff_1 +
                 rho.logimmigration_1 +
                 rho.logrtod_export_1 +
                 rho.logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata_noirq,
              na.action = na.omit)

m4cahc3 <- coeftest(model4ca, cluster.vcov(model4ca,
                                         cluster = cbind(mergepdata_noirq$ccode1,
                                                         mergepdata_noirq$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model4c)$coefficients)),]; m4chc3





## Model 5a - don: Europe (most obs)
model5aa <- lm(aidflow_corrected ~ 
                 rho.s3un_5 + 
                 rho.ipfd_5+
                 rho.r_conflict_prioryear +
                 rho.r_politychange +
                 rho.r_democracy +
                 rho.r_gdp_growth +
                 rho.d_gdp_growth +
                 rho.r_gdplog +
                 rho.d_gdplog +
                 rho.r_population +
                 rho.d_population +
                 rho.d_women_parliament +
                 rho.comlang_off+
                 rho.colony +
                 rho.distlog+
                 rho.ptacount_1 +
                 rho.intervention_1+
                #r_unemployment_1 +
                 rho.d_unemployment_1 +
                 rho.unempdiff_1 +
                 rho.logimmigration_1 +
                 rho.logrtod_export_1 +
                 rho.logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = filter(mergepdata, continent1 == "Europe"),
              na.action = na.omit)

m5aahc3 <- coeftest(model5aa, cluster.vcov(model5aa,
                                         cluster = cbind(filter(mergepdata, continent1 == "Europe")$ccode1,
                                                         filter(mergepdata, continent1 == "Europe")$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model5a)$coefficients)),]; m5aahc3


## Model 5b - don: Americas (2nd most obs) 
#has US (country that bases aid off of UNGA votes)
model5ba <- lm(aidflow_corrected ~ 
                 rho.s3un_5 + 
                 rho.ipfd_5+
                 rho.r_conflict_prioryear +
                 rho.r_politychange +
                 rho.r_democracy +
                 rho.r_gdp_growth +
                 rho.d_gdp_growth +
                 rho.r_gdplog +
                 rho.d_gdplog +
                 rho.r_population +
                 rho.d_population +
                 rho.d_women_parliament +
                 rho.comlang_off+
                 rho.colony +
                 rho.distlog+
                 rho.ptacount_1 +
                 rho.intervention_1+
                #r_unemployment_1 +
                 rho.d_unemployment_1 +
                 rho.unempdiff_1 +
                 rho.logimmigration_1 +
                 rho.logrtod_export_1 +
                 rho.logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = filter(mergepdata, continent1 == "Americas"),
              na.action = na.omit)

m5bahc3 <- coeftest(model5ba, cluster.vcov(model5ba,
                                         cluster = cbind(filter(mergepdata, continent1 == "Americas")$ccode1,
                                                         filter(mergepdata, continent1 == "Americas")$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model5b)$coefficients)),]; m5bahc3


## Model 5c - don: US) 
#US (country that bases aid off of UNGA votes)
model5ca <- lm(aidflow_corrected ~ 
                 rho.s3un_5 + 
                 rho.ipfd_5+
                 rho.r_conflict_prioryear +
                 rho.r_politychange +
                 rho.r_democracy +
                 rho.r_gdp_growth +
                #d_gdp_growth +
                 rho.r_gdplog +
                #d_gdplog +
                 rho.r_population +
                #d_population +
                #d_women_parliament +
                #comlang_off+
                #colony +
                #distlog+
                 rho.ptacount_1 +
                 rho.intervention_1+
                #r_unemployment_1 +
                #d_unemployment_1 +
                 rho.unempdiff_1 +
                 rho.logimmigration_1 +
                 rho.logrtod_export_1 +
                 rho.logdtor_export_1 +
                #ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = filter(mergepdata, ccode1 == 2),
              na.action = na.omit)

m5cahc3 <- coeftest(model5ca, cluster.vcov(model5ca,
                                         cluster = cbind(#filter(mergepdata, ccode1 == 2)$ccode1,
                                           filter(mergepdata, ccode1 == 2)$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))#[-(24:nrow(summary(model5c)$coefficients)),]; m5cahc3


########### Latex ##############


fit.mrho <- summary(modelrho, diagnostics = T)
fit.m4aa <- summary(model4aa, diagnostics = T)
fit.m4ba <- summary(model4ba, diagnostics = T)
fit.m4ca <- summary(model4ca, diagnostics = T)
fit.m5aa <- summary(model5aa, diagnostics = T)
fit.m5ba <- summary(model5ba, diagnostics = T)
fit.m5ca <- summary(model5ca, diagnostics = T)


add.lines = list(
  c(diagnostics[1], round(c(fit.mrho$r.squared,
                            fit.m4aa$r.squared,
                            fit.m4ba$r.squared,
                            fit.m4ca$r.squared,
                            fit.m5aa$r.squared,
                            fit.m5ba$r.squared,
                            fit.m5ca$r.squared),
                          2)),
  c(diagnostics[2], round(c(fit.mrho$fstatistic[1],
                            fit.m4aa$fstatistic[1],
                            fit.m4ba$fstatistic[1],
                            fit.m4ca$fstatistic[1],
                            fit.m4aa$fstatistic[1],
                            fit.m5ba$fstatistic[1],
                            fit.m5ca$fstatistic[1]),
                          2)),
  c(diagnostics[3], nrow(mergepdata) - nrow(as.matrix(fit.mrho$na.action)),
    nrow(filter(mergepdata, continent2 == "Africa")) - nrow(as.matrix(fit.m4aa$na.action)),
    nrow(filter(mergepdata, continent2 == "Asia")) - nrow(as.matrix(fit.m4ba$na.action)),
    nrow(mergepdata_noirq) - nrow(as.matrix(fit.m4ca$na.action)),
    nrow(filter(mergepdata, continent1 == "Europe")) - nrow(as.matrix(fit.m5aa$na.action)),
    nrow(filter(mergepdata, continent1 == "Americas")) - nrow(as.matrix(fit.m5ba$na.action)),
    nrow(filter(mergepdata, ccode1 == 2)) - nrow(as.matrix(fit.m5ca$na.action))))


stargazer(mrhohc3,
          m4aahc3,
          m4bahc3, 
          m4cahc3, 
          m5aahc3,
          m5bahc3, 
          m5cahc3,
          type = "latex",
          covariate.labels = c("Ideal Point Diff. (5 year lag)",
                               "Δ Ideal Point Diff. (5 year lag)",
                     "Conflict, Prior Year, R",
                     "Democratization (ord), R",
                     "Democracy (ord), R",
                     "GDP growth, R",
                     "GDP growth, D",
                     "GDP (logged), R",
                     "GDP (logged), D",
                     "Population (logged), R",
                     "Population (logged), D",
                     "Women in Parliament (%), D",
                     "Common Official Lang.",
                     "Past Colonial Relationship",
                     "Distance (km, logged)",
                     "# of PTAs",
                     "Military Intervention (dummy)",
                     "Unemployment Rate, D",
                     "Unemployment Difference",
                     "Immigration (logged), R to D",
                     "Export (logged), R to D",
                     "Export (logged), D to R"),
          column.labels = c("All", "Africa", "Asia", "No Iraq", "Europe", "Americas", "USA"),
          title = "Autoregression (AR(1)) Model with TWC: By Region Subsamples",
          notes = c("All independent variables lagged 1 year unless otherwise specified",
                    "With country and year fixed effects", 
                    "Robust clustered standard errors (clusters: Recipient Countries, Donor Countries)"),
          omit = c("ccode", "19", "20"),
          add.lines = list(
            c(diagnostics[1], round(c(fit.mrho$r.squared,
                                      fit.m4aa$r.squared,
                                      fit.m4ba$r.squared,
                                      fit.m4ca$r.squared,
                                      fit.m5aa$r.squared,
                                      fit.m5ba$r.squared,
                                      fit.m5ca$r.squared),
                                    2)),
            c(diagnostics[2], round(c(fit.mrho$fstatistic[1],
                                      fit.m4aa$fstatistic[1],
                                      fit.m4ba$fstatistic[1],
                                      fit.m4ca$fstatistic[1],
                                      fit.m4aa$fstatistic[1],
                                      fit.m5ba$fstatistic[1],
                                      fit.m5ca$fstatistic[1]),
                                    2)),
            c(diagnostics[3], nrow(mergepdata) - nrow(as.matrix(fit.mrho$na.action)),
              nrow(filter(mergepdata, continent2 == "Africa")) - nrow(as.matrix(fit.m4aa$na.action)),
              nrow(filter(mergepdata, continent2 == "Asia")) - nrow(as.matrix(fit.m4ba$na.action)),
              nrow(mergepdata_noirq) - nrow(as.matrix(fit.m4ca$na.action)),
              nrow(filter(mergepdata, continent1 == "Europe")) - nrow(as.matrix(fit.m5aa$na.action)),
              nrow(filter(mergepdata, continent1 == "Americas")) - nrow(as.matrix(fit.m5ba$na.action)),
              nrow(filter(mergepdata, ccode1 == 2)) - nrow(as.matrix(fit.m5ca$na.action)))))

#########################################################################
#########################################################################


#DEC 6, beamer presentation

stargazer(mrhohc3,
          m4aahc3,
          m4bahc3, 
          #m4cahc3, 
          m5aahc3,
          #m5bahc3, 
          #m5cahc3,
          type = "latex",
          covariate.labels = c("Ideal Point Diff. (5 year lag)",
                               "FD Ideal Point Diff. (5 year lag)",
                               "Democratization (ord), R",
                               "Democracy (ord), R",
                               "GDP (logged), R",
                               "GDP (logged), D",
                               "Past Colonial Relationship",
                               "Military Intervention (dummy)",
                               "Immigration (logged), R to D"),
          omit = c("ccode", "19", "20", "pta", "gdp_growth", "unemp", "confl", "popu", "women", "export", "distlog", "comlang"),
          column.labels = c("All", "Africa", "Asia", "Europe"),
          title = "Autoregression (AR(1)) Model with TWC: By Region Subsamples",
          notes = c("All independent variables lagged 1 year unless otherwise specified",
                    "With country and year fixed effects", 
                    "Robust clustered standard errors (clusters: Recipient Countries, Donor Countries)",
                    "Not shown: PTA count, GDP growth, Unemployment, conflict,", 
                    "population, women in parliament (D), trade, distance, common language"),
          add.lines = list(
            c("R-squared", round(c(fit.mrho$r.squared,
                                      fit.m4aa$r.squared,
                                      fit.m4ba$r.squared,
                                      #fit.m4ca$r.squared,
                                      fit.m5aa$r.squared),
                                      #fit.m5ba$r.squared,
                                      #fit.m5ca$r.squared),
                                    2)),
            c("F-stat", round(c(fit.mrho$fstatistic[1],
                                      fit.m4aa$fstatistic[1],
                                      fit.m4ba$fstatistic[1],
                                      #fit.m4ca$fstatistic[1],
                                      fit.m4aa$fstatistic[1]),
                                      #fit.m5ba$fstatistic[1],
                                      #fit.m5ca$fstatistic[1]),
                                    2)),
            c("Obs", nrow(mergepdata) - nrow(as.matrix(fit.mrho$na.action)),
              nrow(filter(mergepdata, continent2 == "Africa")) - nrow(as.matrix(fit.m4aa$na.action)),
              nrow(filter(mergepdata, continent2 == "Asia")) - nrow(as.matrix(fit.m4ba$na.action)),
             # nrow(mergepdata_noirq) - nrow(as.matrix(fit.m4ca$na.action)),
              nrow(filter(mergepdata, continent1 == "Europe")) - nrow(as.matrix(fit.m5aa$na.action))
              #nrow(filter(mergepdata, continent1 == "Americas")) - nrow(as.matrix(fit.m5ba$na.action)),
              #nrow(filter(mergepdata, ccode1 == 2)) - nrow(as.matrix(fit.m5ca$na.action))
             )))





############################################
### Model9b
model9b <- lm(aidflow_corrected ~ 
                aidflow_corrected_1 +
                s3un_1 + 
                ipfd_1+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.omit)

m9bhc3 <- coeftest(model9b, cluster.vcov(model9b,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model9b)$coefficients)),]; m9bhc3


### Model9c
model9c <- lm(aidflow_corrected ~
                aidflow_corrected_1 +
                s3un_3 + 
                ipfd_3+
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.omit)

m9chc3 <- coeftest(model9c, cluster.vcov(model9c,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))# [-(24:nrow(summary(model9b)$coefficients)),]; m9chc3


######## Table 9 ############
stargazer(m9ahc3,
          m9bhc3, 
          m9chc3, 
          type = "text", 
          omit = c("ccode", "19", "20"))

#### Yes, there is time dependency, so we need ECM ####
  #The effect is large (0.64)
  #We check for the same thing in model 3 (reverse causality)

# immigration is still a strong predictor



####### Model 10 - Reverse Causality with lagged DV #######

#### Model 10a
model10a <- lm(s3un ~ 
                s3un_1 +
                aidflow_corrected_5 + 
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.exclude)

m10ahc3 <- coeftest(model10a, cluster.vcov(model10a,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))#[-(23:nrow(summary(model10a)$coefficients)),]; m10ahc3


#### Model 10b
model10b <- lm(s3un ~ 
                s3un_1 +
                aidflow_corrected_1 + 
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.exclude)

m10bhc3 <- coeftest(model10b, cluster.vcov(model10b,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))#[-(23:nrow(summary(model10b)$coefficients)),]; m10bhc3



### Model 10c
model10c <- lm(s3un ~ 
                s3un_1 +
                aidflow_corrected_3 + 
                r_conflict_prioryear +
                r_politychange +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #           r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.exclude)

m10chc3 <- coeftest(model10c, cluster.vcov(model10c,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))#[-(23:nrow(summary(model10c)$coefficients)),]; m10chc3



################# TABLE 10 ####################

stargazer(m10ahc3,
          m10bhc3, 
          m10chc3, 
          type = "text", 
          omit = c("ccode", "19", "20"))

#### There is time dependence here too
  #effect is a bit smaller (0.3)




########## Model 11, like model 2 but ECM #########

  # I think time invariant variables would drop out

### Model11a

#mergepdata

model11a <- lm(d.aidflow_corrected ~ 
                aidflow_corrected_1 +
                l.s3un_5 +
                d.s3un_5 +
                l.r_conflict_prioryear +
                d.r_conflict_prioryear +
                l.r_politychange +
                d.r_politychange +
                l.r_democracy +
                d.r_democracy +
                l.r_gdp_growth +
                d.r_gdp_growth + 
                l.d_gdp_growth +
                d.d_gdp_growth +
                l.r_gdplog +
                d.r_gdplog +
                l.d_gdplog +
                d.d_gdplog +
                l.r_population +
                d.r_population +
                l.d_population +
                d.d_population +
                l.d_women_parliament +
                d.d_women_parliament +
                comlang_off+
                colony +
                distlog+
                l.ptacount_1 +
                d.ptacount_1 +
                l.intervention_1+
                d.intervention_1+
                #r_unemployment_1 +
                l.d_unemployment_1 +
                d.d_unemployment_1 +
                l.unempdiff_1 +
                d.unempdiff_1 +
                l.logimmigration_1 +
                d.logimmigration_1 +
                l.logrtod_export_1 +
                d.logrtod_export_1 + 
                l.logdtor_export_1 +
                d.logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.exclude)

m11ahc3 <- coeftest(model11a, cluster.vcov(model11a,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model11a)$coefficients)),]; m11ahc3




### Model11b
model11b <- lm(d.aidflow_corrected ~ 
                 aidflow_corrected_1 +
                 l.s3un_1 +
                 d.s3un_1 +
                 l.r_conflict_prioryear +
                 d.r_conflict_prioryear +
                 l.r_politychange +
                 d.r_politychange +
                 l.r_democracy +
                 d.r_democracy +
                 l.r_gdp_growth +
                 d.r_gdp_growth + 
                 l.d_gdp_growth +
                 d.d_gdp_growth +
                 l.r_gdplog +
                 d.r_gdplog +
                 l.d_gdplog +
                 d.d_gdplog +
                 l.r_population +
                 d.r_population +
                 l.d_population +
                 d.d_population +
                 l.d_women_parliament +
                 d.d_women_parliament +
                 comlang_off+
                 colony +
                 distlog+
                 l.ptacount_1 +
                 d.ptacount_1 +
                 l.intervention_1+
                 d.intervention_1+
                 #r_unemployment_1 +
                 l.d_unemployment_1 +
                 d.d_unemployment_1 +
                 l.unempdiff_1 +
                 d.unempdiff_1 +
                 l.logimmigration_1 +
                 d.logimmigration_1 +
                 l.logrtod_export_1 +
                 d.logrtod_export_1 + 
                 l.logdtor_export_1 +
                 d.logdtor_export_1 +
                 ccode1 + 
                 ccode2 + 
                 as.factor(year), 
               data = mergepdata,
               na.action = na.exclude)
                 
            

m11bhc3 <- coeftest(model11b, cluster.vcov(model11b,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model11b)$coefficients)),]; m11bhc3


### Model11c
model11c <- lm(d.aidflow_corrected ~
                 aidflow_corrected_1 +
                 l.s3un_3 +
                 d.s3un_3 +
                 l.r_conflict_prioryear +
                 d.r_conflict_prioryear +
                 l.r_politychange +
                 d.r_politychange +
                 l.r_democracy +
                 d.r_democracy +
                 l.r_gdp_growth +
                 d.r_gdp_growth + 
                 l.d_gdp_growth +
                 d.d_gdp_growth +
                 l.r_gdplog +
                 d.r_gdplog +
                 l.d_gdplog +
                 d.d_gdplog +
                 l.r_population +
                 d.r_population +
                 l.d_population +
                 d.d_population +
                 l.d_women_parliament +
                 d.d_women_parliament +
                 comlang_off+
                 colony +
                 distlog+
                 l.ptacount_1 +
                 d.ptacount_1 +
                 l.intervention_1+
                 d.intervention_1+
                 #r_unemployment_1 +
                 l.d_unemployment_1 +
                 d.d_unemployment_1 +
                 l.unempdiff_1 +
                 d.unempdiff_1 +
                 l.logimmigration_1 +
                 d.logimmigration_1 +
                 l.logrtod_export_1 +
                 d.logrtod_export_1 + 
                 l.logdtor_export_1 +
                 d.logdtor_export_1 +
                 ccode1 + 
                 ccode2 + 
                 as.factor(year), 
               data = mergepdata,
               na.action = na.exclude)  
                 
  
m11chc3 <- coeftest(model11c, cluster.vcov(model11c,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T))# [-(24:nrow(summary(model11b)$coefficients)),]; m11chc3


######## Table 11 ############
stargazer(m11ahc3,
          m11bhc3, 
          m11chc3, 
          type = "text", 
          omit = c("ccode", "19", "20"))


################### Model 12: Bewley Transformation for Model 11 #################


## Model 12 A ##

## Predict values
mergepdata$dyhat11a <- predict(model11a)

model12a <- lm(aidflow_corrected ~ 
                 dyhat11a +
                 l.s3un_5 +
                 d.s3un_5 +
                 l.r_conflict_prioryear +
                 d.r_conflict_prioryear +
                 l.r_politychange +
                 d.r_politychange +
                 l.r_democracy +
                 d.r_democracy +
                 l.r_gdp_growth +
                 d.r_gdp_growth + 
                 l.d_gdp_growth +
                 d.d_gdp_growth +
                 l.r_gdplog +
                 d.r_gdplog +
                 l.d_gdplog +
                 d.d_gdplog +
                 l.r_population +
                 d.r_population +
                 l.d_population +
                 d.d_population +
                 l.d_women_parliament +
                 d.d_women_parliament +
                 comlang_off+
                 colony +
                 distlog+
                 l.ptacount_1 +
                 d.ptacount_1 +
                 l.intervention_1+
                 d.intervention_1+
                 #r_unemployment_1 +
                 l.d_unemployment_1 +
                 d.d_unemployment_1 +
                 l.unempdiff_1 +
                 d.unempdiff_1 +
                 l.logimmigration_1 +
                 d.logimmigration_1 +
                 l.logrtod_export_1 +
                 d.logrtod_export_1 + 
                 l.logdtor_export_1 +
                 d.logdtor_export_1 +
                 ccode1 + 
                 ccode2 + 
                 as.factor(year), 
               data = mergepdata,
               na.action = na.exclude)

m12ahc3 <- coeftest(model12a, cluster.vcov(model12a,
                                           cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                           leverage = 3, 
                                           df_correction = T)) #[-(24:nrow(summary(model12a)$coefficients)),]; m12ahc3

#this shows strong long-term significance but not short-term significance
  #sign is correct



## Model 12 B ##

## Predict values
mergepdata$dyhat11b <- predict(model11b)

model12b <- lm(aidflow_corrected ~ 
                 dyhat11b +
                 l.s3un_1 +
                 d.s3un_1 +
                 l.r_conflict_prioryear +
                 d.r_conflict_prioryear +
                 l.r_politychange +
                 d.r_politychange +
                 l.r_democracy +
                 d.r_democracy +
                 l.r_gdp_growth +
                 d.r_gdp_growth + 
                 l.d_gdp_growth +
                 d.d_gdp_growth +
                 l.r_gdplog +
                 d.r_gdplog +
                 l.d_gdplog +
                 d.d_gdplog +
                 l.r_population +
                 d.r_population +
                 l.d_population +
                 d.d_population +
                 l.d_women_parliament +
                 d.d_women_parliament +
                 comlang_off+
                 colony +
                 distlog+
                 l.ptacount_1 +
                 d.ptacount_1 +
                 l.intervention_1+
                 d.intervention_1+
                 #r_unemployment_1 +
                 l.d_unemployment_1 +
                 d.d_unemployment_1 +
                 l.unempdiff_1 +
                 d.unempdiff_1 +
                 l.logimmigration_1 +
                 d.logimmigration_1 +
                 l.logrtod_export_1 +
                 d.logrtod_export_1 + 
                 l.logdtor_export_1 +
                 d.logdtor_export_1 +
                 ccode1 + 
                 ccode2 + 
                 as.factor(year), 
               data = mergepdata,
               na.action = na.exclude)

m12bhc3 <- coeftest(model12b, cluster.vcov(model12b,
                                           cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                           leverage = 3, 
                                           df_correction = T)) #[-(24:nrow(summary(model12b)$coefficients)),]; m12bhc3

#this shows NO significance 



## Model 12 C ##

## Predict values
mergepdata$dyhat11c <- predict(model11c)


model12c <- lm(aidflow_corrected ~ 
                 dyhat11c +
                 l.s3un_3 +
                 d.s3un_3 +
                 l.r_conflict_prioryear +
                 d.r_conflict_prioryear +
                 l.r_politychange +
                 d.r_politychange +
                 l.r_democracy +
                 d.r_democracy +
                 l.r_gdp_growth +
                 d.r_gdp_growth + 
                 l.d_gdp_growth +
                 d.d_gdp_growth +
                 l.r_gdplog +
                 d.r_gdplog +
                 l.d_gdplog +
                 d.d_gdplog +
                 l.r_population +
                 d.r_population +
                 l.d_population +
                 d.d_population +
                 l.d_women_parliament +
                 d.d_women_parliament +
                 comlang_off+
                 colony +
                 distlog+
                 l.ptacount_1 +
                 d.ptacount_1 +
                 l.intervention_1+
                 d.intervention_1+
                 #r_unemployment_1 +
                 l.d_unemployment_1 +
                 d.d_unemployment_1 +
                 l.unempdiff_1 +
                 d.unempdiff_1 +
                 l.logimmigration_1 +
                 d.logimmigration_1 +
                 l.logrtod_export_1 +
                 d.logrtod_export_1 + 
                 l.logdtor_export_1 +
                 d.logdtor_export_1 +
                 ccode1 + 
                 ccode2 + 
                 as.factor(year), 
               data = mergepdata,
               na.action = na.exclude)

m12chc3 <- coeftest(model12c, cluster.vcov(model12c,
                                           cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                           leverage = 3, 
                                           df_correction = T)) #[-(24:nrow(summary(model12c)$coefficients)),]; m12chc3

#There is NO long term significance, but there is short term significance
  #BUT the sign for the short term effect is WRONG

###############################

#diagnostics from the models without coeftest
diagnostics <- c("R-squared", "F-stat", "Obs.")
fit.m11a <- summary(model11a, diagnostics = T)
fit.m11a$r.squared
fit.m11a$fstatistic[1]
nrow(mergepdata) - nrow(as.matrix(fit.m11a$na.action))

rownames(paxton_donors)

fit.m12a <- summary(model12a, diagnostics = T)
fit.m12a$r.squared
fit.m12a$fstatistic[1]
nrow(mergepdata) - nrow(as.matrix(fit.m12a$na.action))

fit.m11b <- summary(model11b, diagnostics = T)
fit.m11b$r.squared
fit.m11b$fstatistic[1]
nrow(mergepdata) - nrow(as.matrix(fit.m11b$na.action))

fit.m12b <- summary(model12b, diagnostics = T)
fit.m12b$r.squared
fit.m12b$fstatistic[1]
nrow(mergepdata) - nrow(as.matrix(fit.m12b$na.action))

fit.m11c <- summary(model11c, diagnostics = T)
fit.m11c$r.squared
fit.m11c$fstatistic[1]
nrow(mergepdata) - nrow(as.matrix(fit.m11c$na.action))

fit.m12c <- summary(model12c, diagnostics = T)
fit.m12c$r.squared
fit.m12c$fstatistic[1]
nrow(mergepdata) - nrow(as.matrix(fit.m12c$na.action))



######## Table 12 - Model 11 (ECM) and Model 12 (Bewley) ############
#### Probably my main table

stargazer(m11ahc3,
          m12ahc3,
          m11bhc3,
          m12bhc3,
          m11chc3,
          m12chc3, 
          type = "text", 
          omit = c("ccode", "19", "20"),
          dep.var.labels = c("Democracy Aid Flow"),
          covariate.labels = c("Dem. Aid flow $_{t-1}$(ln)",
                               "Pred. Dem. Aid flow (ln, Model 1)",
                               "Ideal Point Diff. $_{t-6}$",
                               "Δ Ideal Point Diff. $_{t-5}$",
                               "Pred. Aid flow (ln, Model 3)",
                               "Ideal Point Diff. $_{t-2}$",
                               "Δ Ideal Point Diff. $_{t-1}$",
                               "Pred. Aid flow (ln, Model 5)",
                               "Ideal Point Diff. $_{t-4}$",
                               "Δ Ideal Point Diff. $_{t-3}$",
                               "Conflict, Prior Year, R $_{t-1}$",
                               "Δ Conflict, Prior Year, R", 
                               "Democratization (ord), R $_{t-1}$",
                               "Δ Democratization (ord), R$",
                               "Democracy (ord), R $_{t-1}$",
                               "Δ Democracy (ord), R",
                               "GDP Growth, R $_{t-1}$", 
                               "Δ GDP Growth, R",
                               "GDP GRowth, D $_{t-1}$", 
                               "Δ GDP GRowth, D", 
                               "GDP, R (ln) $_{t-1}$", 
                               "Δ GDP, R (ln)", 
                               "GDP, D (ln) $_{t-1}$", 
                               "Δ GDP, D (ln)",
                               "Population, R (ln) $_{t-1}$",
                               "Δ Population, R (ln)",
                               "Population, D (ln) $_{t-1}$",
                               "Δ Population, D (ln)",
                               "Women In Parliament (%), D $_{t-1}$",
                               "Δ Women In Parliament (%), D",
                               "Common Official Language (Dummy)",
                               "Past Colonial Relationship",
                               "Distance (ln)",
                               "# of PTA $_{t-1}$",
                               "Δ # of PTA",
                               "Military Intervention $_{t-1}$",
                               "Δ Military Intervention",
                               "Unemployment (%), D $_{t-1}$",
                               "Δ Unemployment (%), D",
                               "Diff. Unemployment (%) $_{t-1}$",
                               "Δ Diff. Unemployment (%)",
                               "Immigration (ln) $_{t-1}$",
                               "Δ Immigration (ln)",
                               "Exports (ln, R to D) $_{t-1}$",
                               "Δ Exports (ln, R to D)",
                               "Exports (ln, D to R) $_{t-1}$",
                               "Δ Exports (ln, D to R)",
                               "Constant"),
          add.lines = list(
            c(diagnostics[1], round(c(fit.m11a$r.squared,
                                      fit.m12a$r.squared,
                                      fit.m11b$r.squared,
                                      fit.m12b$r.squared,
                                      fit.m11c$r.squared,
                                      fit.m12c$r.squared), 
                                    2)),
            c(diagnostics[2], round(c(fit.m11a$fstatistic[1],
                                      fit.m12a$fstatistic[1],
                                      fit.m11b$fstatistic[1],
                                      fit.m12b$fstatistic[1],
                                      fit.m11c$fstatistic[1],
                                      fit.m12c$fstatistic[1]),
                                    2)),
            c(diagnostics[3], nrow(mergepdata) - nrow(as.matrix(fit.m11a$na.action)),
              nrow(mergepdata) - nrow(as.matrix(fit.m12a$na.action)),
              nrow(mergepdata) - nrow(as.matrix(fit.m11b$na.action)),
              nrow(mergepdata) - nrow(as.matrix(fit.m12b$na.action)),
              nrow(mergepdata) - nrow(as.matrix(fit.m11c$na.action)),
              nrow(mergepdata) - nrow(as.matrix(fit.m12c$na.action)))))


#After correction in April 24- there is always a long-term effect and it is in the correct direction










#####################################################




####### MISC checking stuff

#checking correlation within African recipients
mergepdata_afr <- filter(mergepdata, continent2 == "Africa")

corrvar2 <- select(mergepdata,
                  s3un, 
                  ipfd, 
                  r_conflict_prioryear,
                  r_politychange,
                  r_democracy,
                  r_gdp_growth,
                  d_gdp_growth,
                  r_gdplog, 
                  d_gdplog,
                  r_population,
                  d_population,
                  d_women_parliament,
                  comlang_off,
                  colony,
                  distlog,
                  ptacount_1,
                  intervention_1,
                  r_unemployment_1,
                  d_unemployment_1,
                  unempdiff_1,
                  logimmigration_1,
                  logrtod_export_1,
                  logdtor_export_1,
                  continent2) 
corrvar2 <- filter(corrvar2, continent2 == "Africa")
corrvar2$continent2 <- NULL

corrtbl2 <- cor(corrvar2, use = "complete.obs")
corrplot(corrtbl2)








## Looking for outliers
#the identify function is too slow
#creating standardized hat values

lwdData <- select(mergepdata,
                  ccode1,
                  ccode2,
                  year,
                  aidflow_corrected,
                  s3un_5, 
                  ipfd_5, 
                  r_conflict_prioryear,
                  r_politychange,
                  r_democracy,
                  r_gdp_growth,
                  d_gdp_growth,
                  r_gdplog, 
                  d_gdplog,
                  r_population,
                  d_population,
                  d_women_parliament,
                  comlang_off,
                  colony,
                  distlog,
                  ptacount_1,
                  intervention_1,
                  d_unemployment_1,
                  unempdiff_1,
                  logimmigration_1,
                  logrtod_export_1,
                  logdtor_export_1) %>% na.omit()
lwdData$id <- paste(lwdData$ccode1, lwdData$ccode2, lwdData$year, sep = "_")
regime_hatscore <- hatvalues(model2a)/mean(hatvalues(model2a))

#creating studentized residuals
regime_stu_res <- rstudent(model2a)

#finally, plot the standardized hat values and studentized residuals

plot(x = regime_hatscore, y = regime_stu_res)
identify(regime_hatscore, regime_stu_res, lwdData$id )

#the outliers seem meaningless






###### for the 666 project I'm also going to look at the dem- aid relationship here


model13a <- lm(democratization ~ 
                s3un_5 + 
                ipfd_5+
                r_conflict_prioryear +
                aidflow_corrected_3 +
                r_democracy +
                r_gdp_growth +
                d_gdp_growth +
                r_gdplog +
                d_gdplog +
                r_population +
                d_population +
                d_women_parliament +
                comlang_off+
                colony +
                distlog+
                ptacount_1 +
                intervention_1+
                #r_unemployment_1 +
                d_unemployment_1 +
                unempdiff_1 +
                logimmigration_1 +
                logrtod_export_1 +
                logdtor_export_1 +
                ccode1 + 
                ccode2 + 
                as.factor(year), 
              data = mergepdata,
              na.action = na.omit)

m13ahc3 <- coeftest(model13a, cluster.vcov(model13a,
                                         cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                         leverage = 3, 
                                         df_correction = T)) #[-(24:nrow(summary(model13a)$coefficients)),]; m13ahc3



model13b <- lm(aidflow_corrected ~ 
                 s3un_5 + 
                 ipfd_5+
                 r_conflict_prioryear +
                 democratization_3 +
                 r_democracy +
                 r_gdp_growth +
                 d_gdp_growth +
                 r_gdplog +
                 d_gdplog +
                 r_population +
                 d_population +
                 d_women_parliament +
                 comlang_off+
                 colony +
                 distlog+
                 ptacount_1 +
                 intervention_1+
                 #r_unemployment_1 +
                 d_unemployment_1 +
                 unempdiff_1 +
                 logimmigration_1 +
                 logrtod_export_1 +
                 logdtor_export_1 +
                 ccode1 + 
                 ccode2 + 
                 as.factor(year), 
               data = mergepdata,
               na.action = na.omit)

m13bhc3 <- coeftest(model13b, cluster.vcov(model13b,
                                          cluster = cbind(mergepdata$ccode1, mergepdata$ccode2), 
                                          leverage = 3, 
                                          df_correction = T)) #[-(24:nrow(summary(model13b)$coefficients)),]; m13bhc3

#here it shows that democracy aid and democratization has no effect.
  #when I look at the signs, they are opposite
  #even if I do logit, the signs would still be opposite
#democratization is not correlated with other variables when I look in the corrplot (so the effect couldn't have been killed
  #due to multicolinearity)

###### MODEL 13 - TABLE

#diagnostics from the models without coeftest
diagnostics <- c("R-squared", "F-stat", "Obs.")
fit.m13a <- summary(model11a, diagnostics = T)
fit.m13a$r.squared
fit.m13a$fstatistic[1]
nrow(mergepdata) - nrow(as.matrix(fit.m13a$na.action))

rownames(paxton_donors)

fit.m13b <- summary(model12a, diagnostics = T)
fit.m13b$r.squared
fit.m13b$fstatistic[1]
nrow(mergepdata) - nrow(as.matrix(fit.m13a$na.action))


stargazer(m13ahc3, 
          m13bhc3,
          column.labels = c("Democratization", "Democracy Aid Flow"),
          omit = c("ccode", "19", "20", "unemp", "distlog", "ptacount"),
          covariate.labels = c("Ideal Point Diff.",
                               "FD, Ideal Point Diff.",
                               "Conflict, Prior Year (recipient)",
                               "Democracy Aid Flow $_{t-3}$ (logged)",
                               "Democratization $_{t-1}$",
                               "Democracy (recipient)",
                               "GDP growth (recipient)",
                               "GDP growth (donor)",
                               "GDP pc, logged (recipient)",
                               "GDP pc, logged (donor)",
                               "Population, logged (recipient)",
                               "Population, logged (donor)",
                               "% of Women in Parliament (donor)",
                               "Common Official Language",
                               "Past Colonial Relationship",
                            #  "Distance (logged)",
                            #  "Number of PTAs",
                               "Military Intervention",
                            #  "Unemployment Rate (donor)",
                            #  "Unemployment Difference",
                               "Immigration (logged, recipient to donor)",
                               "Exports (logged, recipient to donor)",
                              "Exports (logged, donor to recipient)",
                               "Constant"),
          type = "text",
          notes = c("With country and year fixed effects", 
                    "Clustered standard errors (cluster: donor country, recipient country)", 
                    "All independent variables lagged one year",
                    "Effect of distance, PTAs, and unemployment not shown due to space"),
          add.lines = list(
            c(diagnostics[1], round(c(fit.m13a$r.squared,
                                      fit.m13b$r.squared),
                                    2)),
            c(diagnostics[2], round(c(fit.m13a$fstatistic[1],
                                      fit.m13b$fstatistic[1]),
                                    2)),
            c(diagnostics[3], nrow(mergepdata) - nrow(as.matrix(fit.m13a$na.action)),
              nrow(mergepdata) - nrow(as.matrix(fit.m13b$na.action)))))







