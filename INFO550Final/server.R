shinyServer(function(input, output) {
  
  myYear <- reactive({
    input$year
  })
  
  
  percentageInput <- reactive({
    switch(input$myData,
           "Deficiencies" = shinyDefData,
           "Penalties" = shinyPenData)
  })
  
  
  output$text <- renderText({
    paste("Proportion by state in", myYear())
  })
  
  output$gvis <- renderGvis({
    year2<-myYear()
    percentageData<-percentageInput()
    
    if (input$myData=="Deficiencies"){
      percentageData<-shinyDefData
    }
    else {
      percentageData<- shinyPenData
    }
    
    # percentageData <-percentageInput()
    
    myData2 <- subset(percentageData,
                      ((year > year2-1) & (year < year2+1))
    )
    
    gvisGeoChart(myData2,
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