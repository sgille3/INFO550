library(googleVis)
library(shiny)
library(dplyr)
library(RSQLite)


projectBdplyr         <- dplyr::src_sqlite(path="www/finalProjectDB.db")
deficienciesPenalties <- as.data.frame(tbl(projectBdplyr, "deficienciesPenaltiesFinal"))
providers             <- as.data.frame(tbl(projectBdplyr, "providersQuery"))


deficiencesYear <- as.data.frame(deficienciesPenalties %>%
                                   group_by(year) %>%
                                   summarise(numDeficienciesYearState = sum(numDeficiencies)))

penaltiesYear   <- as.data.frame(deficienciesPenalties %>%
                                   group_by(year) %>%
                                   summarise(numPenaltiesYearState = sum(numPenalties)) %>%
                                   select(numPenaltiesYearState))

yearFrequencies <- cbind(deficiencesYear, penaltiesYear)


percentages     <- inner_join(deficienciesPenalties, yearFrequencies, c("year")) %>%
  group_by(state, year) %>%
  summarise(defPercentage = round(((numDeficiencies/numDeficienciesYearState)*100),2),
            penPercentage = round(((numPenalties/numPenaltiesYearState)*100),2)) %>%
  select(year,state,defPercentage, penPercentage)

datminmax    <- data.frame(state=rep(c("Min", "Max"),6), 
                           defPercentage=rep(c(0, 100),6),
                           penPercentage=rep(c(0, 100),6),
                           year=sort(rep(seq(2010,2015,1),2)))

shinyDefPenData <- as.data.frame(rbind(percentages, datminmax)) 


shinyDefData <- shinyDefPenData %>% 
select(year, state, defPercentage) %>%
  rename(percentage = defPercentage) %>%
  filter(percentage != "NaN")

shinyPenData    <- shinyDefPenData %>%
  select(year, state, penPercentage) %>%
  rename(percentage = penPercentage)  %>%
  filter(percentage != "NaN")

#Summary Datasets
medicareOnly <- providers %>%
  filter(CERTIFICATION=="Medicare") %>%
  select(BEDCERT, overall_rating, TOTHRD, WEIGHTED_ALL_CYCLES_SCORE, FINE_CNT, TOT_PENLTY_CNT) %>%
  rename("Number Beds" = BEDCERT, "Nursing Rating" = overall_rating, "Nursing Hours" = TOTHRD,
         "Weighted Ratings"= WEIGHTED_ALL_CYCLES_SCORE, "Fine Counts" = FINE_CNT, 
         "Penalty Counts" = TOT_PENLTY_CNT)

medicaidOnly <- providers %>%
  filter(CERTIFICATION=="Medicaid") %>%
  select(BEDCERT, overall_rating, TOTHRD, WEIGHTED_ALL_CYCLES_SCORE, FINE_CNT, TOT_PENLTY_CNT) %>%
  rename("Number Beds" = BEDCERT, "Nursing Rating" = overall_rating, "Nursing Hours" = TOTHRD,
         "Weighted Ratings"= WEIGHTED_ALL_CYCLES_SCORE, "Fine Counts" = FINE_CNT, 
         "Penalty Counts" = TOT_PENLTY_CNT)


medicareAndMedicaidOnly <- providers %>%
  filter(CERTIFICATION=="Medicare and Medicaid") %>%
  select(BEDCERT, overall_rating, TOTHRD, WEIGHTED_ALL_CYCLES_SCORE, FINE_CNT, TOT_PENLTY_CNT) %>%
  rename("Number Beds" = BEDCERT, "Nursing Rating" = overall_rating, "Nursing Hours" = TOTHRD,
         "Weighted Ratings"= WEIGHTED_ALL_CYCLES_SCORE, "Fine Counts" = FINE_CNT, 
         "Penalty Counts" = TOT_PENLTY_CNT)
