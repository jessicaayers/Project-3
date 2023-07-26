library(shiny)
library(tidyverse)
library(DT)

animal <- read.csv("/Users/jessayers/Documents/ST 558/TOPIC 4/AZA_MLE_Jul2018.csv")
animal <- animal %>% select(-c(Male.Data.Deficient, Female.Data.Deficient))

function(input, output, session) {
  #About Page
  
  output$link <- renderUI({
    tagList("More information on the data can be found here: ", a("Zoo Animal Lifespans", href = "https://data.world/animals/zoo-animal-lifespans"))
  })
  
  output$link2 <- renderUI({
    tagList("The article this data was used for can be found here: ", a("Sex-specific median life expectancies from ex situ populations for 330 animal species", href = "https://www.nature.com/articles/sdata201919#Sec1"))
  })
  
  output$pictureOutput <- renderImage({
    list(src = "/Users/jessayers/Documents/ST 558/TOPIC 4/zooanimal.jpeg",
         width = "1000", height = "400")
  })
  
  #Explore Page
  output$sumPlot <- renderPlot({
    if(input$type == "Taxon Class"){
      barplot(table(animal$TaxonClass), col = "blue")
    }
    else if(input$type == "Median Life Expectancy"){
      if(input$median == "Overall"){
        hist(animal$Overall.MLE)
      }
      else if(input$median == "Female"){
        hist(animal$Female.MLE)
      }
      else if(input$median == "Male"){
        hist(animal$Male.MLE)
      }
    }
  })
  
  output$sumTable <- renderDataTable({
    if(input$type == "Taxon Class"){
      tab <- as.data.frame(table(animal$TaxonClass))
      rename(tab, "Taxon Class" = Var1, "Count" = Freq)
    }
    else if(input$type == "Median Life Expectancy"){
      if(input$median == "Overall"){
        print(animal %>% select(animal$Species.Common.Name, animal$Overall.MLE))
      }
      else if(input$median == "Female"){
        animal %>% select(animal$Species.Common.Name, animal$Female.MLE)
      }
      else if(input$median == "Male"){
        animal %>% select(animal$Species.Common.Name, animal$Male.MLE)
      }
    }
  })
  
  
  
  #hist(animal$Overall.MLE)
  #hist(animal$Male.MLE)
  #hist(animal$Female.MLE)
  
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
