library(shiny)
library(tidyverse)
library(DT)
library(tree)
library(randomForest)
library(caret)

animal <- read.csv("/Users/jessayers/Documents/ST 558/TOPIC 4/AZA_MLE_Jul2018.csv")
animal <- animal %>% select(-c(Male.Data.Deficient, Female.Data.Deficient)) 
animal[,15] <- as.numeric(animal[,15], na.rm = TRUE)
animal$Overall.MLE <- as.factor(animal$Overall.MLE)

set.seed(555)

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
      barplot(table(animal$TaxonClass), col = "brown")
    }
    else if(input$type == "Median Life Expectancy" & input$typePlot == "Histogram"){
      if(input$median == "Overall"){
        if(is.null(input$vals)){
       if(input$overlay == "No Overlay"){
        ggplot(animal, aes(x = Overall.MLE)) + 
          geom_histogram(alpha = 0.2, fill = "red")
        }
        else if(input$overlay == "Female MLE"){
          ggplot(animal, aes(x = Overall.MLE)) + 
            geom_histogram(alpha = 0.2, fill = "red") + 
            geom_histogram(aes(x = Female.MLE),alpha = 0.2, fill = "green")
        }
        else if(input$overlay == "Male MLE"){
          ggplot(animal, aes(x = Overall.MLE)) + 
            geom_histogram(alpha = 0.2, fill = "red") + 
            geom_histogram(aes(x = Male.MLE),alpha = 0.2, fill = "blue")
        }
        else if(input$overlay == "Both Female and Male MLE"){
          ggplot(animal, aes(x = Overall.MLE)) + 
            geom_histogram(alpha = 0.2, fill = "red") + 
            geom_histogram(aes(x = Female.MLE),alpha = 0.2, fill = "green") + 
            geom_histogram(aes(x = Male.MLE), alpha = 0.2, fill = "blue")
        }
        }
        else if(!is.null(input$vals)){
          if(input$overlay == "No Overlay"){
            ggplot(animal, aes(x = Overall.MLE)) + 
              geom_histogram(alpha = 0.2, fill = "red") + 
              geom_vline(xintercept = animal %>% select(Overall.MLE) %>% colMeans(na.rm = TRUE)) + 
              geom_vline(xintercept = animal %>% select(Overall.CI...lower) %>% colMeans(na.rm = TRUE), linetype = "dashed") + 
              geom_vline(xintercept = animal %>% select(Overall.CI...upper) %>% colMeans(na.rm = TRUE), linetype = "dashed")
          }
          else if(input$overlay == "Female MLE"){
            ggplot(animal, aes(x = Overall.MLE)) + 
              geom_histogram(alpha = 0.2, fill = "red") + 
              geom_histogram(aes(x = Female.MLE),alpha = 0.2, fill = "green") + 
              geom_vline(xintercept = animal %>% select(Overall.MLE) %>% colMeans(na.rm = TRUE)) + 
              geom_vline(xintercept = animal %>% select(Overall.CI...lower) %>% colMeans(na.rm = TRUE), linetype = "dashed") + 
              geom_vline(xintercept = animal %>% select(Overall.CI...upper) %>% colMeans(na.rm = TRUE), linetype = "dashed")
          }
          else if(input$overlay == "Male MLE"){
            ggplot(animal, aes(x = Overall.MLE)) + 
              geom_histogram(alpha = 0.2, fill = "red") + 
              geom_histogram(aes(x = Male.MLE),alpha = 0.2, fill = "blue") + 
              geom_vline(xintercept = animal %>% select(Overall.MLE) %>% colMeans(na.rm = TRUE)) + 
              geom_vline(xintercept = animal %>% select(Overall.CI...lower) %>% colMeans(na.rm = TRUE), linetype = "dashed") + 
              geom_vline(xintercept = animal %>% select(Overall.CI...upper) %>% colMeans(na.rm = TRUE), linetype = "dashed")
          }
          else if(input$overlay == "Both Female and Male MLE"){
            ggplot(animal, aes(x = Overall.MLE)) + 
              geom_histogram(alpha = 0.2, fill = "red") + 
              geom_histogram(aes(x = Female.MLE),alpha = 0.2, fill = "green") + 
              geom_histogram(aes(x = Male.MLE), alpha = 0.2, fill = "blue") + 
              geom_vline(xintercept = animal %>% select(Overall.MLE) %>% colMeans(na.rm = TRUE)) + 
              geom_vline(xintercept = animal %>% select(Overall.CI...lower) %>% colMeans(na.rm = TRUE), linetype = "dashed") + 
              geom_vline(xintercept = animal %>% select(Overall.CI...upper) %>% colMeans(na.rm = TRUE), linetype = "dashed")
          }
        }
      }
      else if(input$median == "Female"){
        if(is.null(input$vals)){
        ggplot(animal, aes(x = Female.MLE)) + 
          geom_histogram(alpha = 0.2, fill = "green")
        }
        else if(input$vals == "Mean and Confidence Interval Bounds"){
          ggplot(animal, aes(x = Female.MLE)) + 
            geom_histogram(alpha = 0.2, fill = "green") + 
            geom_vline(xintercept = animal %>% select(Female.MLE) %>% colMeans(na.rm = TRUE)) + 
            geom_vline(xintercept = animal %>% select(Female.CI...lower) %>% colMeans(na.rm = TRUE), linetype = "dashed") + 
            geom_vline(xintercept = animal %>% select(Female.CI...upper) %>% colMeans(na.rm = TRUE), linetype = "dashed")
        }
      }
      else if(input$median == "Male"){
        if(is.null(input$vals)){
          ggplot(animal, aes(x = Male.MLE)) + 
            geom_histogram(alpha = 0.2, fill = "blue")
        }
        else if(input$vals == "Mean and Confidence Interval Bounds"){
          ggplot(animal, aes(x = Male.MLE)) + 
            geom_histogram(alpha = 0.2, fill = "blue") + 
            geom_vline(xintercept = animal %>% select(Male.MLE) %>% colMeans(na.rm = TRUE)) + 
            geom_vline(xintercept = animal %>% select(Male.CI...lower) %>% colMeans(na.rm = TRUE), linetype = "dashed") + 
            geom_vline(xintercept = animal %>% select(Male.CI...upper) %>% colMeans(na.rm = TRUE), linetype = "dashed")
        }
      }
    }
    else if (input$type == "Median Life Expectancy" & input$typePlot == "Density"){
      if(input$median == "Overall"){
        if(is.null(input$vals)){
          if(input$overlay == "No Overlay"){
            ggplot(animal, aes(x = Overall.MLE)) + 
              geom_density(alpha = 0.2, fill = "red")
          }
          else if(input$overlay == "Female MLE"){
            ggplot(animal, aes(x = Overall.MLE)) + 
              geom_density(alpha = 0.2, fill = "red") + 
              geom_density(aes(x = Female.MLE),alpha = 0.2, fill = "green")
          }
          else if(input$overlay == "Male MLE"){
            ggplot(animal, aes(x = Overall.MLE)) + 
              geom_density(alpha = 0.2, fill = "red") + 
              geom_density(aes(x = Male.MLE),alpha = 0.2, fill = "blue")
          }
          else if(input$overlay == "Both Female and Male MLE"){
            ggplot(animal, aes(x = Overall.MLE)) + 
              geom_density(alpha = 0.2, fill = "red") + 
              geom_density(aes(x = Female.MLE),alpha = 0.2, fill = "green") + 
              geom_density(aes(x = Male.MLE), alpha = 0.2, fill = "blue")
          }
        }
        else if(!is.null(input$vals)){
          if(input$overlay == "No Overlay"){
            ggplot(animal, aes(x = Overall.MLE)) + 
              geom_density(alpha = 0.2, fill = "red") + 
              geom_vline(xintercept = animal %>% select(Overall.MLE) %>% colMeans(na.rm = TRUE)) + 
              geom_vline(xintercept = animal %>% select(Overall.CI...lower) %>% colMeans(na.rm = TRUE), linetype = "dashed") + 
              geom_vline(xintercept = animal %>% select(Overall.CI...upper) %>% colMeans(na.rm = TRUE), linetype = "dashed")
          }
          else if(input$overlay == "Female MLE"){
            ggplot(animal, aes(x = Overall.MLE)) + 
              geom_density(alpha = 0.2, fill = "red") + 
              geom_density(aes(x = Female.MLE),alpha = 0.2, fill = "green") + 
              geom_vline(xintercept = animal %>% select(Overall.MLE) %>% colMeans(na.rm = TRUE)) + 
              geom_vline(xintercept = animal %>% select(Overall.CI...lower) %>% colMeans(na.rm = TRUE), linetype = "dashed") + 
              geom_vline(xintercept = animal %>% select(Overall.CI...upper) %>% colMeans(na.rm = TRUE), linetype = "dashed")
          }
          else if(input$overlay == "Male MLE"){
            ggplot(animal, aes(x = Overall.MLE)) + 
              geom_density(alpha = 0.2, fill = "red") + 
              geom_density(aes(x = Male.MLE),alpha = 0.2, fill = "blue") + 
              geom_vline(xintercept = animal %>% select(Overall.MLE) %>% colMeans(na.rm = TRUE)) + 
              geom_vline(xintercept = animal %>% select(Overall.CI...lower) %>% colMeans(na.rm = TRUE), linetype = "dashed") + 
              geom_vline(xintercept = animal %>% select(Overall.CI...upper) %>% colMeans(na.rm = TRUE), linetype = "dashed")
          }
          else if(input$overlay == "Both Female and Male MLE"){
            ggplot(animal, aes(x = Overall.MLE)) + 
              geom_density(alpha = 0.2, fill = "red") + 
              geom_density(aes(x = Female.MLE),alpha = 0.2, fill = "green") + 
              geom_density(aes(x = Male.MLE), alpha = 0.2, fill = "blue") + 
              geom_vline(xintercept = animal %>% select(Overall.MLE) %>% colMeans(na.rm = TRUE)) + 
              geom_vline(xintercept = animal %>% select(Overall.CI...lower) %>% colMeans(na.rm = TRUE), linetype = "dashed") + 
              geom_vline(xintercept = animal %>% select(Overall.CI...upper) %>% colMeans(na.rm = TRUE), linetype = "dashed")
          }
        }
      }
      else if(input$median == "Female"){
        if(is.null(input$vals)){
          ggplot(animal, aes(x = Female.MLE)) + 
            geom_density(alpha = 0.2, fill = "green")
        }
        else if(input$vals == "Mean and Confidence Interval Bounds"){
          ggplot(animal, aes(x = Female.MLE)) + 
            geom_density(alpha = 0.2, fill = "green") + 
            geom_vline(xintercept = animal %>% select(Female.MLE) %>% colMeans(na.rm = TRUE)) + 
            geom_vline(xintercept = animal %>% select(Female.CI...lower) %>% colMeans(na.rm = TRUE), linetype = "dashed") + 
            geom_vline(xintercept = animal %>% select(Female.CI...upper) %>% colMeans(na.rm = TRUE), linetype = "dashed")
        }
      }
      else if(input$median == "Male"){
        if(is.null(input$vals)){
          ggplot(animal, aes(x = Male.MLE)) + 
            geom_density(alpha = 0.2, fill = "blue")
        }
        else if(input$vals == "Mean and Confidence Interval Bounds"){
          ggplot(animal, aes(x = Male.MLE)) + 
            geom_density(alpha = 0.2, fill = "blue") + 
            geom_vline(xintercept = animal %>% select(Male.MLE) %>% colMeans(na.rm = TRUE)) + 
            geom_vline(xintercept = animal %>% select(Male.CI...lower) %>% colMeans(na.rm = TRUE), linetype = "dashed") + 
            geom_vline(xintercept = animal %>% select(Male.CI...upper) %>% colMeans(na.rm = TRUE), linetype = "dashed")
        }
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
        if(!is.null(input$vals)){
          Means<- animal %>% select(Overall.MLE, Overall.CI...lower, Overall.CI...upper) %>% colMeans(na.rm = TRUE)
          Means <-  as.data.frame(Means)
        }
        else if(is.null(input$vals)){
          animal %>% select(Species.Common.Name, Scientific.Name, TaxonClass, Overall.MLE)
        }
      }
      else if(input$median == "Female"){
        if(!is.null(input$vals)){
          MeansFemale <- animal %>% select(Female.MLE, Female.CI...lower, Female.CI...upper) %>% colMeans(na.rm = TRUE)
         MeansFemale <-  as.data.frame(MeansFemale)
        }
        else if(is.null(input$vals)){
          animal %>% select(Species.Common.Name, Scientific.Name, TaxonClass, Female.MLE)
        }
      }
      else if(input$median == "Male"){
        if(!is.null(input$vals)){
          MeansMale <- animal %>% select(Male.MLE, Male.CI...lower, Male.CI...upper) %>% colMeans(na.rm = TRUE)
          MeansMale <-  as.data.frame(MeansMale)
        }
        else if(is.null(input$vals)){
          animal %>% select(Species.Common.Name,Scientific.Name, TaxonClass, Male.MLE)
        }
      }
    }
  })
  
  
  #Modeling Tab
  output$predOutput <- renderText({
    "Pred Info"
  })
  
  
  
  output$dataOutput <- renderDataTable({
    if(input$choices == "All Data"){
    animal 
    }
    else if (input$choices == "Species Categorical Data"){
      animal %>% select(Species.Common.Name, Scientific.Name, TaxonClass)
    }
    else if(input$choices == "Overall Median Life Expectancy"){
      if(input$order == "Order by longest median lifespan"){
        animal %>% select(Scientific.Name, Overall.Sample.Size, Overall.MLE, Overall.CI...lower, Overall.CI...upper) %>% arrange(desc(Overall.MLE))
      }
      else {
        animal %>% select(Scientific.Name, Overall.Sample.Size, Overall.MLE, Overall.CI...lower, Overall.CI...upper) %>% arrange(Overall.MLE) 
      }
      }
    else if(input$choices == "Female Median Life Expectancy"){
      if(input$order == "Order by longest median lifespan"){
      animal %>% select(Scientific.Name, Female.Sample.Size, Female.MLE, Female.CI...lower, Female.CI...upper) %>% arrange(desc(Female.MLE))
      }
      else{
        animal %>% select(Scientific.Name, Female.Sample.Size, Female.MLE, Female.CI...lower, Female.CI...upper) %>% arrange(Female.MLE)
      }
    }
    else if(input$choices == "Male Median Life Expectancy"){
      if(input$order == "Order by longest median lifespan"){
      animal %>% select(Scientific.Name, Male.Sample.Size, Male.MLE, Male.CI...lower, Male.CI...upper) %>% arrange(desc(Male.MLE))
      }
      else{
        animal %>% select(Scientific.Name, Male.Sample.Size, Male.MLE, Male.CI...lower, Male.CI...upper) %>% arrange(Male.MLE)
      }
    }
  })
  
  observeEvent(input$save, {
    write_csv(animal, input$path)
})


  trainIndex <- eventReactive(input$train, trainIndex <- createDataPartition(animal$Overall.MLE, p = input$train, list = FALSE))

      datasplit <- reactiveValues(animalTrain = animal[trainIndex(), ], animalTest = animal[-trainIndex(), ])
    
  
  mlrvarfit <- reactive({
    if(input$varvals == "Taxon Class"){
      mlrfit <- caret::train(Overall.MLE ~ TaxonClass,
                      data = as.data.frame(datasplit[[2]]),
                      na.remove = TRUE,
                      method = "lm",
                      preProcess = c("method", "scale"),
                      trControl = trainControl(method = "cv",number = 5))
    }
    summary(mlrfit)
  })
  
  output$fitOutput <- renderUI({
    mlrvarfit()
  })
}

  