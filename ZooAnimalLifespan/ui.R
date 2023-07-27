library(shiny)
library(DT)

fluidPage(
  
  titlePanel("Zoo Animal Lifespans"),
  
  tabsetPanel(
    #About Page
    tabPanel("About", 
             fluid = TRUE,
             mainPanel( "The purpose of this app to explore data relative to the median lifespans of different zoo animals. This app provides a platform to explore and visualize the data, fit three different supervised learning models to the data, and investigate and subset the actual data itself.",
                        br(),
                        br(),
                        "The data was found on the data world platform. The data was downloaded as a csv file. The variables included are: species common name, species scientific name, taxon class, overall sample size, overall median life expectancy, overall lower confidence interval endpoint, overall upper confidence interval endpoint, male sample size, male median life expectancy, male lower confidence interval endpoint, male upper confidence interval endpoint, female sample size, female median life expectancy, female lower confidence interval endpoint, female upper confidence interval endpoint, male data deficient, and female data deficient.",
                        br(),
                        br(),
                        uiOutput("link"),
                        br(),
                        uiOutput("link2"),
                        br(),
                        "The purpose of this page is to give the user a sense of what data will be used and the overall goal of the app. The second page will provide the user with an interactive platform for data exploration. The third page allows the user to fit three different supervised learning methods. Within this page, there are separate tabs for information on the models, model fits, and predicted values. The last and final page gives the user direct access to the data set. The user is able to subset and save the data here.",
                        br(),
                        br(),
                        imageOutput("pictureOutput"))
    ),
    #Explore Page
    tabPanel("Exploration",
             fluid = TRUE,
             sidebarPanel(
               "Let's explore some data!",
               br(),
               br(),
               selectInput("type", label = "Variable Selection", c("Taxon Class", "Median Life Expectancy")),
            
               conditionalPanel(condition = "input.type == 'Median Life Expectancy'",
                                selectInput("typePlot", label = "Type of Plot", c("Density", "Histogram")),
                                selectInput("median", label = "Specified MLE", c("Female", "Male", "Overall")),
              checkboxGroupInput("vals", label = "Additions", c("Mean and Confidence Interval Bounds")))
            ,
             conditionalPanel(condition = "input.type == 'Median Life Expectancy' & input.median == 'Overall'",
                              radioButtons("overlay", label = "Overlay", c("No Overlay","Female MLE", "Male MLE", "Both Female and Male MLE"))
             )),
             mainPanel(plotOutput("sumPlot"),
                       dataTableOutput("sumTable")))
          
    ,
    
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
             sidebarPanel(
               selectInput("choices", "Variables", c("All Data", "Species Categorical Data", "Overall Median Life Expectancy", "Male Median Life Expectancy", "Female Median Life Expectancy")),
               
               conditionalPanel(condition = "input.choices == 'Overall Median Life Expectancy' | input.choices == 'Female Median Life Expectancy' | input.choices =='Male Median Life Expectancy'",  radioButtons("order", label = "Lifespans", c("Order by longest median lifespan", "Order by shortest median lifespan"))),
               selectInput("download", "Do you want to save the data?", c( "No", "Yes")),
               conditionalPanel("input.download == 'Yes'",
                                textInput("path", "Please write the path to save this file:"),
                                actionButton("save", "Save"),
                                br(),
                                br(),
                                "An example path is: /Users/jessayers/Documents/ST 558/TOPIC 4/animal.csv",
                                br(),
                                br(),
                                "Quotations should not be included and animal.csv or a preferred name for the csv file should be attached to the end."
              
             )),
             mainPanel(dataTableOutput("dataOutput"))
)
)
)


#Multiple Linear Regression
#fit <- lm(Overall.MLE ~ TaxonClass + Female.MLE + Male.MLE, data = animal)
#Regression Tree
#fit <- tree(Overall.MLE ~ TaxonClass + Female.MLE + Male.MLE, data = animal)