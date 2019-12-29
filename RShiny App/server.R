
library(shiny)
library(plotly)
library(ggplot2)
library(rlang)
library(readr)
library(MLmetrics)
library(caret)

shinyServer(function(input, output) {
  
  
   
    
   datatable <- reactive({
     
     ############## LINEAR
     
     
     if(input$var == "linear")
    {
      games <- read_csv("games.csv")
      
      # remove any tows without user reviews
      games <- games[games$users_rated>0,]
      
      # remove any rows with missing values
      games <- games[complete.cases(games),]
      
      # unnecessary columns to drop
      drops <-  c("bayes_average_rating","type", "name", "id")
      
      games <- games[, !names(games) %in% drops]
      set.seed(123)
      trainingRowIndex <- sample(1:nrow(games),(input$slider/100)*nrow(games))
      
      trainData <- games[trainingRowIndex,]
      testData <- games[-trainingRowIndex,]
      
      model <- lm(average_rating ~ ., data = trainData)
      
      predictions <- predict(model, testData)
      
      info <- postResample(predictions, testData$average_rating)
      final <- list("model" = model, "info" = info)
      
      return(final)
      
     }
     
     ############### RIDGE
  

     else if(input$var == "ridge")
     {
       games <- read_csv("games.csv")
       
       # remove any tows without user reviews
       games <- games[games$users_rated>0,]
       
       # remove any rows with missing values
       games <- games[complete.cases(games),]
       
       # unnecessary columns to drop
       drops <-  c("bayes_average_rating","type", "name", "id")
       
       games <- games[, !names(games) %in% drops]
       set.seed(123)
       trainingRowIndex <- sample(1:nrow(games),(input$slider/100)*nrow(games))
       
       trainData <- games[trainingRowIndex,]
       testData <- games[-trainingRowIndex,]
       
       ridge_fit <- train(x = trainData, y = trainData$average_rating,
                          method = 'glmnet', 
                          trControl = trainControl(method = 'cv', number = 10),
                          tuneGrid = expand.grid(alpha = 0,
                                                 lambda = seq(0, 10e2, length.out = 20)))
       
       ridge_info <- postResample(predict(ridge_fit, testData), testData$average_rating)
       
       final <- list("model" = ridge_fit, "info" = ridge_info)
       
       return(final)

     }
     
     
     ########### LASSO 
     
     
     else if(input$var == "lasso")
     {
       games <- read_csv("games.csv")
       
       # remove any tows without user reviews
       games <- games[games$users_rated>0,]
       
       # remove any rows with missing values
       games <- games[complete.cases(games),]
       
       # unnecessary columns to drop
       drops <-  c("bayes_average_rating","type", "name", "id")
       
       games <- games[, !names(games) %in% drops]
       set.seed(123)
       trainingRowIndex <- sample(1:nrow(games),(input$slider/100)*nrow(games))
       
       trainData <- games[trainingRowIndex,]
       testData <- games[-trainingRowIndex,]
       
       lasso_fit <- train(x = trainData, y = trainData$average_rating, 
                          method = 'glmnet',
                          trControl = trainControl(method = 'cv', number = 10),
                          tuneGrid = expand.grid(alpha = 1,
                                                 lambda = seq(0.0001, 1, length.out = 50)))
       
       lasso_info <- postResample(predict(lasso_fit, testData), testData$average_rating)
       
       final <- list("model" = lasso_fit, "info" = lasso_info)
       
       return(final)

     }
     
     ############# PCR
     
     else if(input$var == 'pcr')
     {
       games <- read_csv("games.csv")
       
       # remove any tows without user reviews
       games <- games[games$users_rated>0,]
       
       # remove any rows with missing values
       games <- games[complete.cases(games),]
       
       # unnecessary columns to drop
       drops <-  c("bayes_average_rating","type", "name", "id")
       
       games <- games[, !names(games) %in% drops]
       set.seed(123)
       trainingRowIndex <- sample(1:nrow(games),(input$slider/100)*nrow(games))
       
       trainData <- games[trainingRowIndex,]
       testData <- games[-trainingRowIndex,]
       
       pcr_model <- train(x = trainData, y = trainData$average_rating,
                          method = 'pcr',
                          trControl = trainControl(method = 'cv', number = 10),
                          tuneGrid = expand.grid(ncomp = 1:10))

       pcr_info <- postResample(predict(pcr_model, testData), testData$average_rating)
       
       final <- list("model" = pcr_model, "info" = pcr_info)
       
       return(final)
       
     }
     
     ############## PLS
     
     else{
       
       games <- read_csv("games.csv")
       
       # remove any tows without user reviews
       games <- games[games$users_rated>0,]
       
       # remove any rows with missing values
       games <- games[complete.cases(games),]
       
       # unnecessary columns to drop
       drops <-  c("bayes_average_rating","type", "name", "id")
       
       games <- games[, !names(games) %in% drops]
       set.seed(123)
       trainingRowIndex <- sample(1:nrow(games),(input$slider/100)*nrow(games))
       
       trainData <- games[trainingRowIndex,]
       testData <- games[-trainingRowIndex,]
       
       pls_model <- train(x = trainData, y = trainData$average_rating,
                          method = 'pls',
                          trControl = trainControl(method = 'cv', number = 10),
                          tuneGrid = expand.grid(ncomp = 1:10))
       
       pls_info <- postResample(predict(pls_model, testData), testData$average_rating)
       
       final <- list("model" = pls_model, "info" = pls_info)
       
       return(final)
     }
   })
     
    
      output$plot <- renderPlot({
        
      data <- datatable()
      plot(data$model,which=2)
      
      
      
      })
      
     
      
      output$rmse <- renderText({
        d <- datatable()
        rmse <- round(d$info[3][1],2)
        paste(as.character(rmse))
        
      })
      
      output$mae <- renderText({
        d <- datatable()
        mae <- round(d$info[1][1],2)
        paste(as.character(mae))
        
      })
  
})


