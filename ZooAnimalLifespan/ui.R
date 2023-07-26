library(shiny)

fluidPage(
  
  titlePanel("Zoo Animal Lifespans"),
  
  tabsetPanel(
    tabPanel("About", 
             fluid = TRUE,
             mainPanel(textOutput("purposeOutput"),
                       br(),
                       textOutput("sourceOutput"),
                       br(),
                       uiOutput("link"),
                       br(),
                       uiOutput("link2"),
                       br(),
                       textOutput("tabOutput"),
                       br(),
                       imageOutput("pictureOutput"))
    ),
    
    tabPanel("Exploration",
             fluid = TRUE,
             mainPanel(textOutput("expOutput"))),
    
    tabPanel("Modeling",
             fluid = TRUE,
             tabsetPanel(
               tabPanel("Modeling Info",
                        fluid = TRUE,
                        mainPanel(textOutput("infoOutput"))),
               tabPanel("Model Fitting", 
                        fluid = TRUE,
                        mainPanel(textOutput("fitOutput"))),
               tabPanel("Prediction", 
                        fluid = TRUE,
                        mainPanel(textOutput("predOutput")))
             )),
    
    tabPanel("Data",
             fluid = TRUE,
             mainPanel(textOutput("dataOutput")))
  )
)
