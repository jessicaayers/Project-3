# Project-3

## Zoo Animal Lifespans

This app was created using data on the median life expectancies of different zoo animals. The app allows the user to explore both categorical and numerical data. For the numerical data, histograms and density plots can be created along with the ability to plot confidence bounds and mean values. The user can see these mean and boundary values in table format at the same time as plotting. The user is able to create a multiple linear regression, regression tree, and random forest model of their choosing with the following variables: Taxon Class, Female Median Life Expectancy (Female MLE), and Male Life Expectancy (Male MLE). In addition, predicted values can be found with different combinations of new values. The response variable of interest is the Overall Median Life Expectancy (Overall MLE). Lastly, the user can see the full data set, as well as subset it into different categories such as: Cateogrical Data, Female-Specific Data, Male-Specific Data, etc. The user can download the entire data set as well as a csv file. 

The packages needed to run this app are:

  - shiny
  - tidyverse
  - DT
  - tree
  - randomForest
  - caret

Install the packages with: `install.packages(c("shiny", "tidyverse", "DT", "tree", "randomForest", "caret")`

Call the packages with:

library(shiny)

library(tidyverse)

library(DT)

library(tree)

library(randomForest)

library(caret)

Run the app with: `r shiny::runGitHub("Project-3", "jessicaayers", ref = "master")`
