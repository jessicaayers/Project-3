library(shiny)
library(tidyverse)
library(DT)
library(tree)
library(randomForest)
library(caret)

animal <- read.csv("/Users/jessayers/Documents/ST 558/TOPIC 4/AZA_MLE_Jul2018.csv", fileEncoding="latin1")
animal <- animal %>% select(-c(Male.Data.Deficient, Female.Data.Deficient)) 
animal[,15] <- as.numeric(animal[,15], na.rm = TRUE)
animal <- subset(animal, complete.cases(animal$Overall.MLE, Female.MLE, Male.MLE)) 

set.seed(555)

function(input, output, session) {
  #About Page
  
  output$link <- renderUI({
    tagList("More information on the data can be found here (a data world account may need to be created): ", a("Zoo Animal Lifespans", href = "https://data.world/animals/zoo-animal-lifespans"))
  })
  
  output$link2 <- renderUI({
    tagList("The article this data was used for can be found here: ", a("Sex-specific median life expectancies from ex situ populations for 330 animal species", href = "https://www.nature.com/articles/sdata201919#Sec1"))
  })
  
  output$pictureOutput <- renderImage({
    list(src = "../zooanimal.jpeg",
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
          Overall <- animal %>% select(Species.Common.Name, Scientific.Name, TaxonClass, Overall.MLE)
          Overall <- as.data.frame(Overall)
        }
      }
      else if(input$median == "Female"){
        if(!is.null(input$vals)){
          MeansFemale <- animal %>% select(Female.MLE, Female.CI...lower, Female.CI...upper) %>% colMeans(na.rm = TRUE)
         MeansFemale <-  as.data.frame(MeansFemale)
        }
        else if(is.null(input$vals)){
         Overall <-  animal %>% select(Species.Common.Name, Scientific.Name, TaxonClass, Female.MLE)
         Overall <- as.data.frame(Overall)
        }
      }
      else if(input$median == "Male"){
        if(!is.null(input$vals)){
          MeansMale <- animal %>% select(Male.MLE, Male.CI...lower, Male.CI...upper) %>% colMeans(na.rm = TRUE)
          MeansMale <-  as.data.frame(MeansMale)
        }
        else if(is.null(input$vals)){
         Overall <- animal %>% select(Species.Common.Name,Scientific.Name, TaxonClass, Male.MLE)
         Overall <-as.data.frame(Overall)
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
  

animalTrain <- eventReactive(input$fit, {
    trainIndex <- createDataPartition(animal$Overall.MLE, p = input$train, list = FALSE)
    animalTrain <- animal[trainIndex, ]
    animalTrain
})


animalTest <- eventReactive(input$fit, {
  trainIndex <- createDataPartition(animal$Overall.MLE, p = input$train, list = FALSE)
  animalTest <- animal[-trainIndex, ]
  animalTest
})

 output$trainData <- renderDataTable({
    animalTrain()
  })
 
  output$testData <- renderDataTable({
   animalTest()
  })
  
  mlrfit <- eventReactive(input$fit, {
    if(input$vars == "Taxon Class"){
      mlrfit <- train(Overall.MLE ~ TaxonClass,
                       data = animalTrain(),
                       method = "lm",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "cv", number = 5))
    }
    else if(input$vars == "Female MLE"){
      mlrfit <- train(Overall.MLE ~ Female.MLE,
                      data = animalTrain(),
                      method = "lm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = 5))
    }
    else if(input$vars == "Male MLE"){
      mlrfit <- train(Overall.MLE ~ Male.MLE,
                      data = animalTrain(),
                      method = "lm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = 5))
    }
    else if (input$vars == "Female MLE & Male MLE"){
      mlrfit <- train(Overall.MLE ~ Female.MLE + Male.MLE,
                      data = animalTrain(),
                      method = "lm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = 5))
    }
    else if(input$vars == "Taxon Class & Female MLE"){
      mlrfit <- train(Overall.MLE ~ TaxonClass + Female.MLE,
                      data = animalTrain(),
                      method = "lm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = 5))
    }
    else if(input$vars == "Taxon Class & Male MLE"){
      mlrfit <- train(Overall.MLE ~ TaxonClass + Male.MLE,
                      data = animalTrain(),
                      method = "lm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = 5))
    }
    else if(input$vars == "Taxon Class & Female MLE & Male MLE"){
      mlrfit <- train(Overall.MLE ~ TaxonClass + Male.MLE + Female.MLE,
                      data = animalTrain(),
                      method = "lm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = 5))
    }
  })
  
  output$mlrfitOutput <- renderPrint({
    summary(mlrfit())
  })
  
  treefit <- eventReactive(input$fit, {
    if(input$treevars == "Taxon Class"){
      treefit <- train(Overall.MLE ~ TaxonClass, 
                       data = animalTrain(),
                       method = "rpart",
                       trControl = trainControl(method = "cv", number = 5))
      treefit
    }
    else if(input$treevars == "Female MLE"){
      treefit <- train(Overall.MLE ~ Female.MLE,
                      data = animalTrain(),
                      method = "rpart",
                      trControl = trainControl(method = "cv", number = 5))
      treefit
    }
    else if(input$treevars == "Male MLE"){
      treefit <- train(Overall.MLE ~ Male.MLE,
                      data = animalTrain(),
                      method = "rpart",
                      trControl = trainControl(method = "cv", number = 5))
      treefit
    }
    else if (input$treevars == "Female MLE & Male MLE"){
      treefit <- train(Overall.MLE ~ Female.MLE + Male.MLE,
                      data = animalTrain(),
                      method = "rpart",
                      trControl = trainControl(method = "cv", number = 5))
      treefit
    }
    else if(input$treevars == "Taxon Class & Female MLE"){
      treefit <- train(Overall.MLE ~ TaxonClass + Female.MLE,
                      data = animalTrain(),
                      method = "rpart",
                      trControl = trainControl(method = "cv", number = 5))
      treefit
    }
    else if(input$treevars == "Taxon Class & Male MLE"){
      treefit <- train(Overall.MLE ~ TaxonClass + Male.MLE,
                      data = animalTrain(),
                      method = "rpart",
                      trControl = trainControl(method = "cv", number = 5))
      treefit
    }
    else if(input$treevars == "Taxon Class & Female MLE & Male MLE"){
      treefit <- train(Overall.MLE ~ TaxonClass + Male.MLE + Female.MLE,
                      data = animalTrain(),
                      method = "rpart",
                      trControl = trainControl(method = "cv", number = 5))
      treefit
    }
  })
  
  output$treeOutput <- renderPrint({
    treefit()
  })
  
  rfit <- eventReactive(input$fit, {
    rfit <- train(Overall.MLE ~ TaxonClass + Female.MLE + Male.MLE ,
                     data = animalTrain(),
                     method = "rf",
                     trControl = trainControl(method = "cv", number = 5),
                    tuneGrid = data.frame(mtry = 1:input$m))
    rfit
  })
  
  output$rfOutput <- renderPrint({
    rfit()
  })
  
  
  mlrpred <- eventReactive(input$fit, {
    pred <- predict(mlrfit(), newdata = animalTest())
  })
  
  mlrstats <- eventReactive(input$fit, {
    mlrstats <- as.data.frame(postResample(mlrpred(), obs = animalTest()$Overall.MLE))
    colnames(mlrstats) <- "Multiple Lin Reg"
    mlrstats
  })
  
  treepred <- eventReactive(input$fit, {
    treepred <- predict(treefit(), newdata = animalTest())
  })
  
  treestats <- eventReactive(input$fit, {
    treestats <- as.data.frame(postResample(treepred(), obs = animalTest()$Overall.MLE))
    colnames(treestats) <- "Regression Tree"
    treestats
  })
  
  rfpred <- eventReactive(input$fit, {
    rfpred <- predict(rfit(), newdata = animalTest())
  })
  
  rfstats <- eventReactive(input$fit, {
    rfstats <- as.data.frame(postResample(rfpred(), obs = animalTest()$Overall.MLE))
    colnames(rfstats) <- "Random Forest"
    rfstats
  })
  
  output$mstats <- renderDataTable({
   mlrstats()
  })
  
  output$tstats <- renderDataTable({
    treestats()
  })

  output$rfstats <- renderDataTable({
    rfstats()
  })

  new <- reactive({
    new <- data.frame(TaxonClass = input$tc, Female.MLE = input$FMLE, Male.MLE = input$MMLE)
  })
  
predictions <- eventReactive(input$pred, {
    if(input$model == "Multiple Linear Regression"){
    mlrfitp <- train(Overall.MLE ~ TaxonClass + Male.MLE + Female.MLE,
                    data = animal,
                    method = "lm",
                    preProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv", number = 5))
    p <- as.data.frame(predict(mlrfitp, newdata = new()))
    colnames(p) <- "Prediction"
    p
    }
    else if(input$model == "Regression Tree"){
      treefitp <- train(Overall.MLE ~ TaxonClass + Male.MLE + Female.MLE,
                       data = animal,
                       method = "rpart",
                       trControl = trainControl(method = "cv", number = 5))
     p <- as.data.frame( predict(treefitp, newdata = new()))
     colnames(p) <- "Prediction"
     p
    }
    else if(input$model == "Random Forest Model"){
      rfitp <- train(Overall.MLE ~ TaxonClass + Male.MLE + Female.MLE,
                    data = animal,
                    method = "rf",
                    trControl = trainControl(method = "cv", number = 5))
      p <- as.data.frame(predict(rfitp, newdata = new()))
      colnames(p) <- "Prediction"
      p
    }
  })
    
    output$predOutput <- renderDataTable({
      predictions()
    })
}

  