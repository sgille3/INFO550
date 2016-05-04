library(shiny)

shinyUI(tabsetPanel(tabPanel("Longitudinal Data",
                             pageWithSidebar(
                               headerPanel("US Nursing Home Proportions of Deficiencies and Penalties"),
                               sidebarPanel(
                                 selectInput('myData', selected="Deficiencies",
                                             'Choose a Variable:', c("Deficiencies", "Penalties")),
                                 
                                 sliderInput("year", "Year to be displayed:", 
                                             min=2010, max=2015, value=2010,  step=1,
                                             sep= "", animate=animationOptions(interval=4000,loop=FALSE))
                               ),
                               mainPanel(
                                 h3(textOutput("text"))
                                 ,
                                 htmlOutput("gvis")
                               )
                             )
)
,
tabPanel("Static Data",
         titlePanel("Nursing Home Summaries"),
         sidebarLayout(
           sidebarPanel(
             selectInput("dataset", "Choose a subset:",
                         choices = c("Medicare", "Medicaid", "Medicare and Medicaid")),
             
             numericInput("obs", "Number of observations to view:", 10)
           ),
           mainPanel(
             h4("Summary"),
             verbatimTextOutput("summary"),
             
             h4("Observations"),
             tableOutput("view")
           )
         )
)

))