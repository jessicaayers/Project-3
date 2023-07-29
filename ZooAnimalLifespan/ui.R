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
              checkboxGroupInput("vals", label = "Additions for Specified MLE", c("Mean and Confidence Interval Bounds")))
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
                        "There will be three models fit to the animal data.", br(), br(), "The first model fit will be a multiple linear regression model. The response variable that we are interested in predicting is the Overall Median Life Expectancy of the animals. The quantitative predictors will be the Male Median Life Expectancy and the Female Median Life Expectancy. Taxon Class will be used as a qualitative predictor. The benefits of this model are that the parameters can be easily interpreted. The downfalls of this model are that the predictive power may be low and there may be a relationship between Female and Male Median Life Expectancies that is not accounted for. Mathematically this can be shown: ", br(), br(), withMathJax(helpText('$$Overall.MLE = \\beta_0 + \\beta_1*TaxonClass +\\beta_2*Female.MLE +\\beta_3* Male.MLE$$ ')), br(), br(), 
                        "The second model that will be fit is a regression tree. Since the overall median life expectancy is a continuous variable, a regression tree will be fit instead of a classification tree. The benefits of a regression tree are that there is still a high level of predictability and interpretation. A tree-based method also provides an easily interpreted visual called a dendogram and any interaction between variables is already accounted for. The downfall of regression trees are that sometimes the data can be overfit, so pruning may be needed." ,br(), br(),
                        "The third model fit is a random forest model. This model averages many trees created from bootstrapped values to provide the mode efficient fit. Random subset of predictors are used to fit the trees. This is an advantage because by randomly selecting predictors, those with dominating influences will not overpower other predictors. Another advantage of random forests is that they tend to have a higher level of predictability than regression trees and models. downfall of random forests is that the trees are very hard to interpret since they are now being averaged."
                        
                        ),
               
               
               
               
               tabPanel("Model Fitting", 
                        fluid = TRUE,
                        sidebarPanel(
                          "Let's Build Our Models!",
                          br(),
                          br(),
                          numericInput("train", "Proportion of Data to Use in Training Set", value = 0.5, step = 0.1, min = 0, max = 1),
                          selectInput("vars", "Choose your combinations of variables for the multiple linear regression: ", c("Taxon Class", "Female MLE", "Male MLE", "Female MLE & Male MLE", "Taxon Class & Female MLE", "Taxon Class & Male MLE", "Taxon Class & Female MLE & Male MLE")
                          ),
                          selectInput("treevars", "Choose your combinations of variables for the regression tree: ", c("Taxon Class", "Female MLE", "Male MLE", "Female MLE & Male MLE", "Taxon Class & Female MLE", "Taxon Class & Male MLE", "Taxon Class & Female MLE & Male MLE")),
                          numericInput("m", "Choose the m value for the random forest model: ", value = 1, step = 1, min = 1, max = 3),
                          actionButton("fit", "Let's fit our models!"),
                          h3("Model Statistics!"),
                          dataTableOutput("mstats"),
                          dataTableOutput("tstats"),
                          dataTableOutput("rfstats"))
                        ,
                        mainPanel("The training data set is as follows: ",
                                  br(),
                                  br(),
                                  dataTableOutput("trainData"),
                                  "The test data set is as follows: ",
                                  br(),
                                  br(),
                                  dataTableOutput("testData"),
                                  br(),
                                  br(),
                                  "The Multiple Linear Regression Summary is: ",
                                  verbatimTextOutput("mlrfitOutput"),
                                  br(),
                                  br(),
                                  "The Regression Tree Output is: ",
                                  verbatimTextOutput("treeOutput"),
                                  br(),
                                  br(),
                                  "The Random Forest Output is: ",
                                  verbatimTextOutput("rfOutput"))
                      ),
             
               
               
               
               
               
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