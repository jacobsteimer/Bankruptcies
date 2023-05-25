library(readxl)
IDBBankruptcies2022 <- read_excel("IDBBankruptcies2022.xlsx")
View(IDBBankruptcies2022)

IDBNoDup <- IDBBankruptcies2022 %>% distinct(CASEKEY, .keep_all = TRUE)
#Distinct CaseIDs based on what Bob said

zcta_data <- get_acs(
  +     geography = "zcta", year = 2021,
  +     variables = "B19013_001"
  + )
zcta_data2 <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B01003_001" 
) 
zcta_data3 <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B02001_003" 
)
#5-year estimates

ZIPcounts$ZIP <- substr(ZIPcounts$D1ZIP, 1, 5)
ZIPcounts$GEOID <- ZIPcounts$ZIP
ZIPcountsPOP <- left_join(ZIPcounts,zcta_data)
#Joining with `by = join_by(GEOID)`
View(ZIPcountsPOP)
ZIPcountsPOP$PerThou <- (ZIPcountsPOP$`n()`/ZIPcountsPOP$estimate)*1000
leaflet(ZIPcountPOPShape2) %>% addPolygons(fillColor = ~colorQuantile("YlOrRd", BankruptciesPerThousand)(BankruptciesPerThousand),
                                           fillOpacity = 0.7,
                                           color = "#BDBDC3",
                                           weight = 1)

zipcodes <- zctas(state = "TN", year = 2010)
ZIPcountPOPShape <- merge(zipcodes, ZIPcountsPOP, by.x = "ZCTA5CE10", by.y = "ZIP")
> leaflet(ZIPcountPOPShape) %>%
  +     addPolygons(fillColor = ~colorQuantile("YlOrRd", PerThou)(PerThou),
                    +                 fillOpacity = 0.7,
                    +                 color = "#BDBDC3",
                    +                 weight = 1)
#don't use this

IDBNoDup$ZIP <- substr(IDBNoDup$D1ZIP, 1, 5)
ZIPcount2 <- IDBNoDup %>% group_by(ZIP) %>% summarize(n())
ZIPcount2$GEOID <- ZIPcount2$ZIP
ZIPcountPOP2 <- left_join(ZIPcount2,zcta_data2)
ZIPcountPOP2$BankruptciesPerThousand <- (ZIPcountPOP2$`n()`/ZIPcountPOP2$estimate)*1000
ZIPcountPOP2 <- merge(ZillowZIP, ZIPcountPOP2, by.x = "ZillowZIP", by.y = "ZIP")
ZIPcountPOPShape2 <- merge(zipcodes, ZIPcountPOP2, by.x = "ZCTA5CE10", by.y = "ZillowZIP")
ZIPcountPOPShape2 <- ZIPcountPOPShape2 %>% filter(BankruptciesPerThousand < 100)
leaflet(ZIPcountPOPShape2) %>%
  addPolygons(fillColor = ~colorQuantile("YlOrRd", BankruptciesPerThousand)(BankruptciesPerThousand),
              fillOpacity = 0.7,
              color = "#BDBDC3",
              weight = 1)
#Use this

ZIPcountPOP3 <- left_join(ZIPcountPOP2, zcta_data3, by = "GEOID")
ZIPcountPOP3 <- ZIPcountPOP3 %>% rename(TotalPopulation = estimate.x) %>% rename(BlackCount = estimate.y)
ZIPcountPOP3 <- ZIPcountPOP3 %>% mutate(BlackPerc = BlackCount/TotalPopulation)
ZIPcountPOP3 %>% ggplot(aes(BlackPerc,BankruptciesPerThousand)) + geom_point()
#Clear correlation between Black ZIPs and bankruptcies
# Correlation = 0.9489693
# they're 95% percent statistically ... explains 95% of bankruptcy rate

ZIPcountPOP4 <- left_join(ZIPcountPOP3, zcta_data, by = "GEOID")
ZIPcountPOP4 <- ZIPcountPOP4 %>% filter(estimate.x > 0) 
ZIPcountPOP4 %>% summarize(cor(BankruptciesPerThousand,estimate.y),cor(BankruptciesPerThousand,BlackPerc))
cor(BankruptciesPerThousand, estimate.y)
-0.8412526
cor(BankruptciesPerThousand, BlackPerc)
0.9489693
lm(BankruptciesPerThousand ~ BlackPerc + estimate.y, data = ZIPcountPOP4)
library(broom)
fit <- lm(BankruptciesPerThousand ~ BlackPerc + estimate.y, data = ZIPcountPOP4)
tidy(fit)
term          estimate  std.error statistic  p.value
<chr>            <dbl>      <dbl>     <dbl>    <dbl>
  1 (Intercept)  3.05      0.813           3.75 7.22e- 4
2 BlackPerc    7.79      0.795           9.80 5.16e-11
3 estimate.y  -0.0000245 0.00000700     -3.50 1.44e- 3
# Race correlation is far higher than income correlation, according to LM
# try flipping positions # nothing changed
# both are highly significant even after adjusted for the other
# for 10% increase in race, expect a little less than one more bankruptcyper 1,000
# do in 1,000s or 10s of thousands
ZIPcountPOP4 <- ZIPcountPOP4 %>% rename(MedianHousehold = estimate) 
#Renaming

ZIPcountPOP4 <- ZIPcountPOP4 %>% mutate(HouseholdIncome10Thous = MedianHousehold/10000)
fit <- lm(BankruptciesPerThousand ~ BlackPerc + HouseholdIncome10Thous, data = ZIPcountPOP4)
tidy(fit)
# A tibble: 3 × 5
term                   estimate std.error statistic  p.value
<chr>                     <dbl>     <dbl>     <dbl>    <dbl>
  1 (Intercept)               3.05     0.813       3.75 7.22e- 4
2 BlackPerc                 7.79     0.795       9.80 5.16e-11
3 HouseholdIncome10Thous   -0.245    0.0700     -3.50 1.44e- 3
#better units ... for every 10,000 more in household income, bankruptcies drop .25 per 1,000

ZIPcount5 <- IDBNoDup %>% filter(ORGFLCHP == 13) %>% group_by(ZIP) %>% summarize(n())
ZIPcount5$GEOID <- ZIPcount5$ZIP
ZIPcountPOP5 <- left_join(ZIPcount5,zcta_data2)
ZIPcountPOP5$BankruptciesPerThousand <- (ZIPcountPOP5$`n()`/ZIPcountPOP5$estimate)*1000
ZIPcountPOP5 <- merge(ZillowZIP, ZIPcountPOP5, by.x = "ZillowZIP", by.y = "ZIP")
ZIPcountPOP5 <- ZIPcountPOP5 %>% rename(TotalPopulation = estimate)
ZIPcountPOP5 <- left_join(ZIPcountPOP5,zcta_data, by = "GEOID")
ZIPcountPOP5 <- left_join(ZIPcountPOP5,zcta_data3)
ZIPcountPOP5 <- ZIPcountPOP5 %>% rename(MedianHousehold = estimate.y)
ZIPcountPOP5 <- ZIPcountPOP5 %>% rename(ThirteensPerThou = BankruptciesPerThousand)
ZIPcountPOP5 <- ZIPcountPOP5 %>% filter(ThirteensPerThou < 100)
ZIPcountPOP5 <- ZIPcountPOP5 %>% mutate(BlackPerc = Black/TotalPopulation)
# This is just Chapter 13s per ZIP

cor(ZIPcountPOP5$ThirteensPerThou,ZIPcountPOP5$MedianHousehold)
[1] -0.8319284
cor(ZIPcountPOP5$ThirteensPerThou,ZIPcountPOP5$BlackPerc)
[1] 0.9385523
fit2 <- lm(ThirteensPerThou ~ BlackPerc + MedianHousehold, data = ZIPcountPOP5)
tidy(fit2)
term              estimate  std.error statistic       p.value
<chr>                <dbl>      <dbl>     <dbl>         <dbl>
  1 (Intercept)      2.40      0.779           3.09 0.00425      
2 BlackPerc        6.51      0.762           8.55 0.00000000116
3 MedianHousehold -0.0000205 0.00000671     -3.05 0.00466  
#Ch13 correlations

ZIPcountPOP5 <- ZIPcountPOP5 %>% mutate(HouseholdIncome10Thous = MedianHousehold/10000)
fit2 <- lm(ThirteensPerThou ~ BlackPerc + HouseholdIncome10Thous, data = ZIPcountPOP5)
tidy(fit2)
term                   estimate std.error statistic       p.value
<chr>                     <dbl>     <dbl>     <dbl>         <dbl>
  1 (Intercept)               2.40     0.779       3.09 0.00425      
2 BlackPerc                 6.51     0.762       8.55 0.00000000116
3 HouseholdIncome10Thous   -0.205    0.0671     -3.05 0.00466   
#better units

ZIPcountPOP5 %>% ggplot(aes(BlackPerc,ThirteensPerThou)) + geom_point()
mu_x <- mean(ZIPcountPOP5$BlackPerc)
mu_y <- mean(ZIPcountPOP5$ThirteensPerThou)
s_x <- sd(ZIPcountPOP5$BlackPerc)
s_y <- sd(ZIPcountPOP5$ThirteensPerThou)
r <- cor(ZIPcountPOP5$ThirteensPerThou,ZIPcountPOP5$BlackPerc)
ZIPcountPOP5 %>% ggplot(aes(BlackPerc,ThirteensPerThou)) + geom_point() + geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x)
#graphing

ZIPpercs <- IDBNoDup %>%
  group_by(ZIP) %>%
  summarize(Chapter13_percentage = sum(ORGFLCHP == "13") / n()) %>%
  mutate(Chapter13_percentage = Chapter13_percentage * 100)
ZIPpercs$GEOID <- ZIPpercs$ZIP
ZIPpercs <- left_join(ZIPpercs,zcta_data2)
ZIPpercs <- merge(ZillowZIP, ZIPpercs, by.x = "ZillowZIP", by.y = "ZIP")
ZIPpercs <- ZIPpercs %>% rename(TotalPopulation = estimate)
ZIPpercs <- left_join(ZIPpercs,zcta_data, by = "GEOID")
ZIPpercs <- left_join(ZIPpercs,zcta_data3)
ZIPpercs <- ZIPpercs %>% rename(MedianHousehold = estimate)
ZIPpercs <- ZIPpercs %>% filter(TotalPopulation > 0)
ZIPpercs <- ZIPpercs %>% mutate(BlackPerc = Black/TotalPopulation)
#Chapter 13 Percentages per ZIP

cor(ZIPpercs$Chapter13_percentage,ZIPpercs$MedianHousehold)
-0.01816902
cor(ZIPpercs$Chapter13_percentage,ZIPpercs$BlackPerc)
0.1821899
fit3 <- lm(Chapter13_percentage ~ BlackPerc + MedianHousehold, data = ZIPpercs)
term              estimate std.error statistic  p.value
<chr>                <dbl>     <dbl>     <dbl>    <dbl>
  1 (Intercept)     71.1       6.32          11.3  1.77e-12
2 BlackPerc        9.49      6.18           1.54 1.35e- 1
3 MedianHousehold  0.0000611 0.0000544      1.12 2.70e- 1
#Appears thrown off by 38139, which has only seven bankruptcies (rich ZIP) but all are 13s
#Ch 13 Percentage Correlations
#what a sad little p-value

ZIPpercsOutlier <- ZIPpercs %>% Chapter13
cor(ZIPpercsOutlier$Chapter13_percentage,ZIPpercsOutlier$MedianHousehold)
[1] -0.348093
cor(ZIPpercsOutlier$Chapter13_percentage,ZIPpercsOutlier$BlackPerc)
[1] 0.3749504
fit3 <- lm(Chapter13_percentage ~ BlackPerc + MedianHousehold, data = ZIPpercsOutlier)
tidy(fit3)
term              estimate std.error statistic  p.value
<chr>                <dbl>     <dbl>     <dbl>    <dbl>
  1 (Intercept)     78.2       5.69         13.8   1.72e-14
2 BlackPerc        5.34      5.35          0.999 3.26e- 1
3 MedianHousehold -0.0000292 0.0000523    -0.559 5.80e- 1
#remove outlier ... still not statistically significant

zcta_data3 <- get_acs(
  geography = "zcta", year = 2021,
  variables = c("B02001_003")
)
zcta_data3$Black <- zcta_data3$estimate
colnames(zcta_data3) <- c("GEOID", "oldName", "CenVar", "Black", "errormarg", "BlackRepeat")
ZIPcountPOP3 <- left_join(ZIPcountPOP2,zcta_data3)

IDBNoDup$ZIP <- substr(IDBNoDup$D1ZIP, 1, 5)
IDBNoDupShelb <- IDBNoDup %>% filter(ZIP %in% ZillowZIP$ZillowZIP)
IDBShelbChaps <- IDBNoDupShelb %>% group_by(ORGFLCHP) %>% summarize(n())
1035+4349
[1] 5384
IDBShelbChaps <- IDBShelbChaps %>% mutate(Percent = IDBShelbChaps$`n()`/5384)

ZIPcountPOP3$BlackPerc <- ZIPcountPOP3$Black/ZIPcountPOP3$estimate
BlackZIPs <- ZIPcountPOP3 %>% filter(BlackPerc > .50)
View(BlackZIPs)
IDBShelbBlack <- IDBNoDupShelb %>% filter(ZIP %in% BlackZIPs$ZillowZIP)
IDBShelbBlackChaps <- IDBShelbBlack %>% group_by(ORGFLCHP) %>% summarize(n())
View(IDBShelbBlackChaps)
721+3243
[1] 3964
IDBShelbBlackChaps <- IDBShelbBlackChaps %>% mutate(Percent = IDBShelbBlackChaps$`n()`/3964)