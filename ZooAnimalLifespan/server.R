library(shiny)

function(input, output, session) {
  #About Page
  output$purposeOutput <- renderText({
    "The purpose of this app to explore data relative to the median lifespans of different zoo animals. This app provides a platform to explore and visualize the data, fit three different supervised learning models to the data, and investigate and subset the actual data itself."
  })
  
  output$sourceOutput <- renderText({
    "The data was found on the data world platform. The data was downloaded as a csv file. The variables included are: species common name, species scientific name, taxon class, overall sample size, overall median life expectancy, overall lower confidence interval endpoint, overall upper confidence interval endpoint, male sample size, male median life expectancy, male lower confidence interval endpoint, male upper confidence interval endpoint, female sample size, female median life expectancy, female lower confidence interval endpoint, female upper confidence interval endpoint, male data deficient, and female data deficient."
  })
  
  output$link <- renderUI({
    tagList("More information on the data can be found here: ", a("Zoo Animal Lifespans", href = "https://data.world/animals/zoo-animal-lifespans"))
  })
  
  output$link2 <- renderUI({
    tagList("The article this data was used for can be found here: ", a("Sex-specific median life expectancies from ex situ populations for 330 animal species", href = "https://www.nature.com/articles/sdata201919#Sec1"))
  })
  
  output$tabOutput <- renderText({
    "The purpose of this page is to give the user a sense of what data will be used and the overall goal of the app. The second page will provide the user with an interactive platform for data exploration. The third page allows the user to fit three different supervised learning methods. Within this page, there are separate tabs for information on the models, model fits, and predicted values. The last and final page gives the user direct access to the data set. The user is able to subset and save the data here."
  })
  
  output$pictureOutput <- renderImage({
    list(src = "/Users/jessayers/Documents/ST 558/TOPIC 4/zooanimal.jpeg",
         width = "1000", height = "400", deleteFile = FALSE)
  })
  
  output$expOutput <- renderText({
    "Exploration Page"
  })
  
  #Modeling Tab
  output$infoOutput <- renderText({
    "Model Info"
  })
  output$fitOutput <- renderText({
    "Fit Info"
  })
  output$predOutput <- renderText({
    "Pred Info"
  })
  
  
  
  output$dataOutput <- renderText({
    "Data Page"
  })
  
}
