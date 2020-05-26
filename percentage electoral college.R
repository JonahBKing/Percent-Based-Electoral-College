rm(list=ls())
library(tidyverse)
library(haven)

###load datasets
getwd()
setwd("~/Box Sync/R_work/proportional electoral college")
elections <- read_csv("electionfile.csv")
evotesperstate <- read_csv("evotesperstate.csv") %>% 
  select(state_po , year , evotes)

options(scipen = 999) 

###joining datasets to include number of electoral votes per state
evotesall <- left_join(elections, evotesperstate ) %>% 
  select(year , state, state_po , candidate , party, writein , candidatevotes , totalvotes , evotes) 

###thresholds should happen here to make things easier later on
threshold10 <- evotesall %>% 
  mutate(state_share = (candidatevotes/ totalvotes)*100) %>% 
  filter(state_share>= 10) %>% 
  group_by(year, state) %>% 
  mutate( totalvotes = sum(candidatevotes , na.rm = TRUE)) %>% 
  mutate(state_share = (candidatevotes / totalvotes)*100) %>% 
  mutate(evote_share = ((state_share/100) * evotes)) %>% 
  filter(evote_share >= .5) %>% 
  mutate(totalvotes = sum(candidatevotes , na.rm = TRUE)) %>% 
  mutate(state_share = (candidatevotes / totalvotes)*100) %>% 
  mutate(evote_share = ((state_share/100) * evotes)) %>% 
  mutate(round_share = round(evote_share, digits = 0)) %>%
  mutate(correct_round_share = case_when (year == 1992 & state_po == "NH" & candidate == "Perot, Ross" ~ 0 ,
                                          year == 1992 & state_po == "NE" & party == "republican" ~ 3 ,
                                          year == 1992 & state_po == "UT" & party == "republican" ~ 3 , 
                                          year == 1992 & state_po == "MN" & party == "democrat" ~ 5 , 
                                          year == 1992 & state_po == "WA" & party == "independent" ~ 2 ,
                                          year == 1992 & state_po == "IN" & party == "republican" ~ 6 ,
                                          year == 1992 & state_po == "MA" & party == "independent" ~ 2 ,
                                          year == 1992 & state_po == "GA" & party == "independent" ~ 1 ,
                                          year == 1992 & state_po == "NJ" & party == "democrat" ~ 7 ,
                                          year == 1992 & state_po == "OH" & party == "democrat" ~ 9 ,
                                          year == 1992 & state_po == "IL" & party == "independent" ~ 3 ,
                                          year == 1992 & state_po == "PA" & party == "democrat" ~ 11 ,
                                          year == 1996 & state_po == "NE" & party == "reform party" ~ 0 ,
                                          year == 1996 & state_po == "WV" & party == "reform party" ~ 0 ,
                                          year == 1996 & state_po == "WI" & party == "democrat" ~ 6 ,
                                          TRUE ~ as.numeric(round_share))) %>% 
  mutate(round_share = correct_round_share) %>% 
  mutate(correct_round_share = NULL) %>% 
  mutate(correct_candidate = case_when(year == 2012 & state_po == "WA" & party == "republican" ~ "Romney, Mitt" , TRUE ~ as.character(candidate))) %>% 
  mutate(candidate = correct_candidate) %>% 
  mutate(correct_candidate = NULL) %>% 
  filter(round_share > 0)

#state_allocation10nocand <- aggregate(x = threshold10$correct_round_share,                
 #         by = list(threshold10$year , threshold10$state , threshold10$evotes),              
  #        FUN = sum)
#allocation_mistakes10 <- subset(state_allocation10nocand, state_allocation10nocand$Group.3 != state_allocation10nocand$x)

#test <- subset(threshold10, correct_candidate != candidate)

###check results for each year 
result1976 <- threshold10[which(threshold10$year ==1976),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_10 = sum(round_share)) %>% 
  mutate(year=1976)
  
result1980 <- threshold10[which(threshold10$year ==1980),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_10 = sum(round_share)) %>% 
  mutate(year=1980) 
  
result1984 <- threshold10[which(threshold10$year ==1984),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_10 = sum(round_share)) %>% 
  mutate(year=1984) 

result1988 <- threshold10[which(threshold10$year ==1988),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_10 = sum(round_share)) %>% 
  mutate(year=1988) 
  
result1992 <- threshold10[which(threshold10$year ==1992),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_10 = sum(round_share)) %>% 
  mutate(year=1992) 
  
result1996 <- threshold10[which(threshold10$year ==1996),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_10 = sum(round_share)) %>% 
  mutate(year=1996)

result2000 <- threshold10[which(threshold10$year ==2000),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_10 = sum(round_share)) %>% 
  mutate(year=2000)
  
result2004 <- threshold10[which(threshold10$year ==2004),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_10 = sum(round_share)) %>% 
  mutate(year=2004) 

result2008 <- threshold10[which(threshold10$year ==2008),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_10 = sum(round_share)) %>% 
  mutate(year=2008)

result2012 <- threshold10[which(threshold10$year ==2012),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_10 = sum(round_share)) %>% 
  mutate(year=2012)
  
result2016 <- threshold10[which(threshold10$year ==2016),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_10 = sum(round_share)) %>% 
  mutate(year=2016)
  

results10 <- rbind(result1976, result1980, result1984, result1988, result1992 , result1996, 
                   result2000 , result2004, result2008 , result2012 , result2016)  
results10 <- results10[c(3,1,2)]

### subsets for years, will be necessary for map
pres1976 <- subset(threshold10 , year==1976)
pres1980 <- subset(threshold10 , year==1980)
pres1984 <- subset(threshold10 , year==1984)
pres1988 <- subset(threshold10 , year==1988)
pres1992 <- subset(threshold10 , year==1992)
pres1996 <- subset(threshold10 , year==1996)
pres2000 <- subset(threshold10, year==2000)
pres2004 <- subset(threshold10 , year==2008)
pres2012 <- subset(threshold10 , year==2012)
pres2016 <- subset(threshold10 , year==2016)


##############################
### trying with a 5% Threshold
threshold5 <- evotesall %>% 
  mutate(state_share = (candidatevotes/ totalvotes)*100) %>% 
  filter(state_share>= 5) %>% 
  group_by(year, state) %>% 
  mutate( totalvotes = sum(candidatevotes , na.rm = TRUE)) %>% 
  mutate(state_share = (candidatevotes / totalvotes)*100) %>% 
  mutate(evote_share = ((state_share/100) * evotes)) %>% 
  filter(evote_share >= .5) %>% 
  mutate(totalvotes = sum(candidatevotes , na.rm = TRUE)) %>% 
  mutate(state_share = (candidatevotes / totalvotes)*100) %>% 
  mutate(evote_share = ((state_share/100) * evotes)) %>% 
  mutate(round_share = round(evote_share, digits = 0)) %>%
  mutate(correct_round_share = case_when(year == 1980 & state_po == "AZ" & party == "independent" ~ 0 ,
                                         year == 1980 & state_po == "FL" & party == "independent" ~ 0 ,
                                         year == 1980 & state_po == "PA" & party == "independent" ~ 1 ,
                                         year == 1992 & state_po == "NH" & party == "independent" ~ 0 ,
                                         year == 1992 & state_po == "NE" & party == "republican" ~ 3 ,
                                         year == 1992 & state_po == "UT" & party == "republican" ~ 3 ,
                                         year == 1992 & state_po == "MS" & party == "independent" ~ 0 ,
                                         year == 1992 & state_po == "MN" & party == "democrat" ~ 5 ,
                                         year == 1992 & state_po == "WA" & party == "independent" ~ 2 ,
                                         year == 1992 & state_po == "IN" & party == "republican" ~ 6 ,
                                         year == 1992 & state_po == "MA" & party == "independent" ~ 2 ,
                                         year == 1992 & state_po == "GA" & party == "independent" ~ 1 ,
                                         year == 1992 & state_po == "NJ" & party == "democrat" ~ 7 ,
                                         year == 1992 & state_po == "OH" & party == "democrat" ~ 9 ,
                                         year == 1992 & state_po == "IL" & party == "independent" ~ 3 ,
                                         year == 1992 & state_po == "PA" & party == "democrat" ~ 11 ,
                                         year == 1996 & state_po == "NE" & party == "reform party" ~ 0 ,
                                         year == 1996 & state_po == "UT" & party == "reform party" ~ 0 ,
                                         year == 1996 & state_po == "WV" & party == "reform party" ~ 0 ,
                                         year == 1996 & state_po == "IA" & party == "reform party" ~ 0 ,
                                         year == 1996 & state_po == "AZ" & party == "reform party" ~ 0 ,
                                         year == 1996 & state_po == "CO" & party == "reform party" ~ 0 ,
                                         year == 1996 & state_po == "KY" & party == "reform party" ~ 0 ,
                                         year == 1996 & state_po == "AL" & party == "independent" ~ 0 ,
                                         year == 1996 & state_po == "LA" & party == "reform party" ~ 0 ,
                                         year == 1996 & state_po == "WI" & party == "democrat" ~ 6 ,
                                         year == 1996 & state_po == "MA" & party == "democrat" ~ 8 ,
                                         year == 1996 & state_po == "NJ" & party == "democrat" ~ 9 ,
                                         year == 1996 & state_po == "PA" & party == "democrat" ~ 12 ,
                                         year == 2000 & state_po == "MN" & party == "green" ~ 0 ,
                                          TRUE ~ as.numeric(round_share))) %>% 
  mutate(round_share = correct_round_share) %>% 
  mutate(correct_round_share = NULL) %>% 
  mutate(correct_candidate = case_when(year == 2012 & state_po == "WA" & party == "republican" ~ "Romney, Mitt" , TRUE ~ as.character(candidate))) %>% 
  mutate(candidate = correct_candidate) %>% 
  mutate(correct_candidate = NULL) %>% 
  filter(round_share > 0)

#state_allocation5nocand <- aggregate(x = threshold5$correct_round_share,                
         #by = list(threshold5$year , threshold5$state , threshold5$evotes),              
        #FUN = sum)
#allocation_mistakes5 <- subset(state_allocation5nocand, state_allocation5nocand$Group.3 != state_allocation5nocand$x)

#subset(threshold5 , year == 2000 & state_po == "MN")
#test <- subset(threshold5, correct_round_share != round_share )

#################
### results 
result1976.5 <- threshold5[which(threshold5$year ==1976),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_5 = sum(round_share)) %>% 
  mutate(year=1976)

result1980.5 <- threshold5[which(threshold5$year ==1980),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_5 = sum(round_share)) %>% 
  mutate(year=1980) 

result1984.5 <- threshold5[which(threshold5$year ==1984),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_5 = sum(round_share)) %>% 
  mutate(year=1984) 

result1988.5 <- threshold5[which(threshold5$year ==1988),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_5 = sum(round_share)) %>% 
  mutate(year=1988) 

result1992.5 <- threshold5[which(threshold5$year ==1992),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_5 = sum(round_share)) %>% 
  mutate(year=1992) 

result1996.5 <- threshold5[which(threshold5$year ==1996),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_5 = sum(round_share)) %>% 
  mutate(year=1996)

result2000.5 <- threshold5[which(threshold5$year ==2000),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_5 = sum(round_share)) %>% 
  mutate(year=2000)

result2004.5 <- threshold5[which(threshold5$year ==2004),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_5 = sum(round_share)) %>% 
  mutate(year=2004) 

result2008.5 <- threshold5[which(threshold5$year ==2008),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_5 = sum(round_share)) %>% 
  mutate(year=2008)

result2012.5 <- threshold5[which(threshold5$year ==2012),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_5 = sum(round_share)) %>% 
  mutate(year=2012)

result2016.5 <- threshold5[which(threshold5$year ==2016),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_5 = sum(round_share)) %>% 
  mutate(year=2016)

results5 <- rbind( result1976.5 , result1980.5 , result1984.5 , result1988.5 , result1992.5 , result1996.5 , 
                   result2000.5 , result2004.5 , result2008.5 , result2012.5 , result2016.5)  
results5 <- results5[c(3,1,2)]

##############################
###############################
### trying with a 25% Threshold
threshold25 <- evotesall %>% 
  mutate(state_share = (candidatevotes/ totalvotes)*100) %>% 
  filter(state_share>= 25) %>% 
  group_by(year, state) %>% 
  mutate( totalvotes = sum(candidatevotes , na.rm = TRUE)) %>% 
  mutate(state_share = (candidatevotes / totalvotes)*100) %>% 
  mutate(evote_share = ((state_share/100) * evotes)) %>% 
  filter(evote_share >= .5) %>% 
  mutate(totalvotes = sum(candidatevotes , na.rm = TRUE)) %>% 
  mutate(state_share = (candidatevotes / totalvotes)*100) %>% 
  mutate(evote_share = ((state_share/100) * evotes)) %>% 
  mutate(round_share = round(evote_share, digits = 0)) %>%
  mutate(correct_round_share = round_share) %>% 
  mutate(correct_candidate = case_when(year == 2012 & state_po == "WA" & party == "republican" ~ "Romney, Mitt" , TRUE ~ as.character(candidate))) %>% 
  mutate(candidate = correct_candidate) %>% 
  mutate(correct_candidate = NULL) %>% 
  filter(round_share > 0)

###check results for each year 
result1976.25 <- threshold25[which(threshold25$year ==1976),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_25 = sum(round_share)) %>% 
  mutate(year=1976)

result1980.25 <- threshold25[which(threshold25$year ==1980),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_25 = sum(round_share)) %>% 
  mutate(year=1980) 

result1984.25 <- threshold25[which(threshold25$year ==1984),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_25 = sum(round_share)) %>% 
  mutate(year=1984) 

result1988.25 <- threshold25[which(threshold25$year ==1988),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_25 = sum(round_share)) %>% 
  mutate(year=1988) 

result1992.25 <- threshold25[which(threshold25$year ==1992),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_25 = sum(round_share)) %>% 
  mutate(year=1992) 

result1996.25 <- threshold25[which(threshold25$year ==1996),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_25 = sum(round_share)) %>% 
  mutate(year=1996)

result2000.25 <- threshold25[which(threshold25$year ==2000),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_25 = sum(round_share)) %>% 
  mutate(year=2000)

result2004.25 <- threshold25[which(threshold25$year ==2004),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_25 = sum(round_share)) %>% 
  mutate(year=2004) 

result2008.25 <- threshold25[which(threshold25$year ==2008),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_25 = sum(round_share)) %>% 
  mutate(year=2008)

result2012.25 <- threshold25[which(threshold25$year ==2012),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_25 = sum(round_share)) %>% 
  mutate(year=2012)

result2016.25 <- threshold25[which(threshold25$year ==2016),]  %>% 
  group_by(candidate) %>% 
  summarise(Viable_25 = sum(round_share)) %>% 
  mutate(year=2016)

results25 <- rbind(result1976.25, result1980.25, result1984.25, result1988.25, result1992.25 , result1996.25, 
                   result2000.25 , result2004.25, result2008.25 , result2012.25 , result2016.25)  
results25 <- results25[c(3,1,2)]

place <- left_join(results5 , results10   )
result_all <- left_join(place , results25)

write_csv(result_all, "Results.csv",  na= "0")
write
