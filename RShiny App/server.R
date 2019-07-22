
library(shiny)
library(plotly)
library(ggplot2)
library(rlang)


shinyServer(function(input, output) {
  
  
  
    
   datatable <- reactive({
     if(input$var == "disp")
    {
      
      set.seed(123)
      trainingRowIndex <- sample(1:nrow(mtcars),(input$slider/100)*nrow(mtcars))
      
      trainData <- mtcars[trainingRowIndex,]
      testData <- mtcars[-trainingRowIndex,]
      
      model <- lm(hp ~ disp, trainData)
      
      testData$predicted_hp <- predict(model,testData)
      final <- testData[,c(3,4,12)]
      return(final)
      
     }
     else if(input$var == "wt")
     {
       
       set.seed(123)
       trainingRowIndex <- sample(1:nrow(mtcars),(input$slider/100)*nrow(mtcars))
       
       trainData <- mtcars[trainingRowIndex,]
       testData <- mtcars[-trainingRowIndex,]
       
       model <- lm(hp ~ wt, trainData)
       
       testData$predicted_hp <- predict(model,testData)
       final <- testData[,c(6,4,12)]
       return(final)
       
     }
     else if(input$var == "qsec")
     {
       
       set.seed(123)
       trainingRowIndex <- sample(1:nrow(mtcars),(input$slider/100)*nrow(mtcars))
       
       trainData <- mtcars[trainingRowIndex,]
       testData <- mtcars[-trainingRowIndex,]
       
       model <- lm(hp ~ qsec, trainData)
       
       testData$predicted_hp <- predict(model,testData)
       final <- testData[,c(7,4,12)]
       return(final)
       
     } 
     else {
       
       set.seed(123)
       trainingRowIndex <- sample(1:nrow(mtcars),(input$slider/100)*nrow(mtcars))
       
       trainData <- mtcars[trainingRowIndex,]
       testData <- mtcars[-trainingRowIndex,]
       
       model <- lm(hp ~ mpg, trainData)
       
       testData$predicted_hp <- predict(model,testData)
       final <- testData[,c(1,4,12)]
       return(final)
       
     }
   })
     
    
      output$plot <- renderPlotly({
      data <- datatable()
        ggplotly(
        ggplot(data,aes(x=data[,1],y=predicted_hp))+geom_point(aes(size=5))+geom_line()+
          geom_segment(aes(xend=data[,1],yend=hp))+
          geom_point(aes(y=hp,fill="#8B0000",size=5))+
          xlab(colnames(data[1]))+ylab("Actual vs Predicted HP")+
          theme(
            panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                            size = 2, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "white"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "white"),
            legend.position = "none",
            plot.title = element_text(color = "#411753", size = 15,hjust = 0.48),
            axis.title.x = element_text(color = "#993333", size = 14),
            axis.title.y = element_text(color = "#993333", size = 14),
            axis.text.x = element_text(face="bold", color="black",
                                       size=13),
            axis.text.y = element_text(face="bold", color="black",
                                       size=13)
          )+
          labs(title="Regressed HP Vs Actual HP ",
                  subtitle = "Red-Actual | Black-Predicted")
        
      )
      })
      
      output$err <- renderText({
        d <- datatable()
        mape <- round(mean(abs((d$predicted_hp-d$hp))/d$hp)*100,2)
        paste(as.character(mape),"%")
        
      })
      
      output$acc <- renderText({
        d <- datatable()
        mape <- round(100-mean(abs((d$predicted_hp-d$hp))/d$hp)*100,2)
        paste(as.character(mape),"%")
        
      })
  
})


