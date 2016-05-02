require(googleVis)
require(shiny)
library(dplyr)

setwd("C:/Users/sgille3/Projects/Course Work/INFO - Software Engineering/Project 1")
projectBdplyr         <- src_sqlite(path="finalProjectDB.db")
deficienciesPenalties <- as.data.frame(tbl(projectBdplyr, "deficienciesPenaltiesFinal"))
providers             <- as.data.frame(tbl(projectBdplyr, "providersQuery"))

##deficienciesPenalties
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

shinyDefData    <- shinyDefPenData %>%
                      select(year, state, defPercentage) %>%
                      rename(percentage = defPercentage)

shinyPenData    <- shinyDefPenData %>%
                      select(year, state, penPercentage) %>%
                      rename(percentage = penPercentage)

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

shinyServer(function(input, output) {
  myYear <- reactive({
    input$year
  })
  output$year <- renderText({
    paste("Proportion by state in", myYear())
  })
  
  percentageInput <- reactive({
    switch(input$dataset,
           "Deficiencies" = shinyDefData,
           "Penalties" = shinyPenData)
  })
  output$gvis <- renderGvis({
    percentageData <-percentageInput()
    myData <- subset(percentageData, 
                     (year > (myYear()-1)) & (year < (myYear()+1)))
    gvisGeoChart(myData,
                 locationvar="state", colorvar="percentage",
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces",
                              width=1000, height=800,
                              colorAxis="{values:[0,1,5,10,15,25,50,100],
                                        colors:[\'#fee0d2',\'#fcbba1',\'#fc9272',\'#fb6a4a',\'#ef3b2c',
                                        \'#cb181d', \'#a50f15',\'#67000d']}"
                              
                 ))     
  })

  datasetInput <- reactive({
    switch(input$dataset,
         "Medicare" = medicareOnly,
         "Medicaid" = medicaidOnly,
         "Medicare and Medicaid" = medicareAndMedicaidOnly)
  })

  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })

  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
})