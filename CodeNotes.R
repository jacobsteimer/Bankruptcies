Bankruptcies12thruJune <- read_excel("bf_f5a_0630.2022.xlsx")
Bankruptcies12thruJune <- Bankruptcies12thruJune %>% mutate(CountyCode = substr(...2,1,5))
#Now with county codes without asterisks

library(ggrepel)
BrupwithPops <- inner_join(Bankruptcies12thruJune, MostRecentCtyPops2)
BrupwithPops <- BrupwithPops %>% mutate(BperThousand = (`Total All Chapters`/POPESTIMATE2021)*1000)
# Now combined and with bankruptcies per 1,000

BrupwithLargePops <- BrupwithPops %>% filter(POPESTIMATE2021 > 500000)
# Only large counties

BrupwithLargePops <- BrupwithLargePops %>% arrange(desc(BperThousand))
BrupwithPops <- BrupwithPops %>% arrange(desc(BperThousand))
# Descending by Bankruptcies per 1,000

X2020Bankruptcies <- X2020Bankruptcies %>% mutate(CountyCode = substr(`County + Code ¹`,1,5))
pop_2020 <- pop_2020 %>% mutate(CountyCode = GEOID)                                                                      
BrupwithPops2020 <- inner_join(X2020Bankruptcies, pop_2020)

#NEW DATA FINALLY
Bankruptcies12thruMarch <- read_excel("C:/Users/jacob/Downloads/bf_f5a_0331.2023.xlsx")
Bankruptcies12thruMarch <- Bankruptcies12thruMarch %>% mutate(CountyCode = substr(...2,1,5))
#Now with county codes without asterisks

library(ggrepel)
County_Pops2021 <- get_acs(
  geography = "county", year = 2021,
  variables = "B01003_001" 
) 
County_Pops2021 <- County_Pops2021 %>% mutate(CountyCode = GEOID) 

BrupMarchwithPops <- inner_join(Bankruptcies12thruMarch, County_Pops2021)
BrupMarchwithPops <- BrupMarchwithPops %>% mutate(BperThousand = (`Total All Chapters`/estimate)*1000)
# Now combined and with bankruptcies per 1,000

BrupMarchwithLargePops <- BrupMarchwithPops %>% filter(estimate > 500000)
# Only large counties

BrupMarchwithLargePops <- BrupMarchwithLargePops %>% arrange(desc(BperThousand))
# Descending by Bankruptcies per 1,000

names(BrupMarchwithLargePops)[16] <- "NonBizCh13"
names(BrupMarchwithLargePops)[13] <- "NonBizAllChap"
names(BrupMarchwithLargePops)[14] <- "NonBizCh7"
#clean up names
BrupMarchwithLargePops$NonBizCh13 <- as.numeric(BrupMarchwithLargePops$NonBizCh13)
BrupMarchwithLargePops$NonBizAllChap <- as.numeric(BrupMarchwithLargePops$NonBizAllChap)
BrupMarchwithLargePops$NonBizCh7 <- as.numeric(BrupMarchwithLargePops$NonBizCh7)

#clean up data types
BrupMarchwithLargePops <- BrupMarchwithLargePops %>% mutate(ThirteenPerThou = (NonBizCh13/BrupMarchwithLargePops$estimate)*1000)
BrupMarchwithLargePops <- BrupMarchwithLargePops %>% mutate(PersonalPerThou = (NonBizAllChap/estimate)*1000)
#more analysis

Top100 <- BrupMarchwithLargePops %>% filter(PersonalPerThou > .66788)
#Top100 in personal bankruptcies

Top25 <- Top100 %>% filter(PersonalPerThou > 1.59)
Top25 %>% ggplot(aes(NAME,PersonalPerThou)) + geom_point(size = 3, color = ifelse(Top25$NAME == "Shelby County, Tennessee", "red", "black")) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Top25 graphed

Top25 %>% ggplot(aes(NAME,PersonalPerThou)) + geom_col(size = 3, fill = ifelse(Top25$NAME == "Shelby County, Tennessee", "red", "black")) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Try Bar Chart

Top25 %>% ggplot(aes(NAME,PersonalPerThou)) + geom_col(size = 3, fill = ifelse(Top25$NAME == "Shelby County, Tennessee", "#FFCC22", "#817f75")) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Brand Colors

Top25 %>% ggplot(aes(reorder(NAME,PersonalPerThou),PersonalPerThou)) + geom_col(size = 3, fill = ifelse(Top25$NAME == "Shelby County, Tennessee", "#FFCC22", "#817f75")) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Reordered

Top25TwoCol <- Top25 %>% select(NAME,PersonalPerThou)
#Two Columns

Top25TwoCol <- Top25TwoCol %>% add_row(NAME = "U.S. Average",PersonalPerThou = 1.179181)
Top25TwoCol %>% ggplot(aes(reorder(NAME,PersonalPerThou),PersonalPerThou)) + geom_col(size = 3, fill = ifelse(Top25TwoCol$NAME %in% c("Shelby County, Tennessee","U.S. Average"), "#FFCC22", "#817f75")) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Add the average

Top25TwoCol %>% ggplot(aes(reorder(NAME,PersonalPerThou),PersonalPerThou)) + geom_col(size = 3, fill = ifelse(Top25TwoCol$NAME %in% c("Shelby County, Tennessee","U.S. Average"), "#FFCC22", "#817f75")) + labs(x = NULL, y = "Bankruptcies per thousand people", title = "The 25 counties with the highest personal bankruptcy rates", caption = "This analysis only included counties with more than 500,000 residents. Data was drawn from the Administrative Office of the U.S. Courts.") + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.caption = element_text(size = 12), plot.title = element_text(size = 25))
#Better titles

Top25TwoCol %>% ggplot(aes(reorder(NAME,PersonalPerThou),PersonalPerThou)) + geom_col(size = 3, fill = ifelse(Top25TwoCol$NAME %in% c("Shelby County, Tennessee","U.S. Average"), "#FFCC22", "#817f75")) + labs(x = NULL, y = "Bankruptcies per thousand people", title = "The 25 counties with the highest personal bankruptcy rates", caption = "This analysis only included counties with more than 500,000 residents. Data was drawn from the Administrative Office of the U.S. Courts.") + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.caption = element_text(size = 12), plot.title = element_text(size = 25)) + coord_flip()
#Coordinate flip

Top25TwoCol %>% ggplot(aes(reorder(NAME,-PersonalPerThou),PersonalPerThou)) + geom_col(size = 3, fill = ifelse(Top25TwoCol$NAME %in% c("Shelby County, Tennessee","U.S. Average"), "#FFCC22", "#817f75")) + labs(x = NULL, y = "Bankruptcies per thousand people", title = "The 25 counties with the highest personal bankruptcy rates", caption = "This analysis only included counties with more than 500,000 residents. Data was drawn from the Administrative Office of the U.S. Courts.") + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.caption = element_text(size = 12), plot.title = element_text(size = 25)) + coord_flip()
#Top to bottom
#Put shelby back on top, cut down number to 10, and move note away from x-axis label

County_Black2021 <- get_acs(
  geography = "county", year = 2021,
  variables = "B02001_003E" 
  )
BrupMarchwithLargePopsAndRace <- left_join(BrupMarchwithLargePops, zcta_data3, by = "GEOID")
#Add # of Black

BrupMarchwithLargePopsAndRace <- BrupMarchwithLargePopsAndRace %>% rename(TotalPopulation = estimate.x) %>% rename(BlackCount = estimate.y)
BrupMarchwithLargePopsAndRace <- BrupMarchwithLargePopsAndRace %>% mutate(BlackPerc = BlackCount/TotalPopulation)
#rename and percentages

cor(BrupMarchwithLargePopsAndRace$PersonalPerThou, BrupMarchwithLargePopsAndRace$BlackPerc)
[1] 0.1334374
#MUCH smaller correlation