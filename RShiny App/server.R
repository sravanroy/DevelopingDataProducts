
library(shiny)
library(plotly)

shinyServer(function(input, output) {
  
  # splitting into train and test sets
  
  output$plot <- renderPlotly({
    
    d <- mtcars
    trainingRowIndex <- sample(1:nrow(d),(input
                                          $slider/100)*nrow(d))
    
    trainData <- d[trainingRowIndex,]
    testData <- d[-trainingRowIndex,]
    
    # Execute linear regression model
    
    m_hp <- lm(mpg ~ hp, trainData)
    pred_mpg <- predict(m_hp,testData)
    
    actuals_preds <- data.frame(cbind(actuals=testData$mpg,predicteds=pred_mpg))
    testData$predicted_mpg <- actuals_preds$predicteds
    final <- testData
    
    ggplotly(
      (
        ggplot(final,aes(x=hp,y=predicted_mpg))+geom_point()+
      geom_point(aes(y=mpg,colour="#3C0414"))+xlab("Horse Power")+ylab("Actual vs Predicted MPG")+
        theme(
          panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "white"),
          legend.position = "none",
          plot.title = element_text(color = "blue", size = 16, face = "bold",hjust = 0.5),
          axis.title.x = element_text(color = "#993333", size = 14, face = "bold"),
          axis.title.y = element_text(color = "#993333", size = 14, face = "bold"),
          axis.text.x = element_text(face="bold", color="black", 
                                     size=13),
          axis.text.y = element_text(face="bold", color="black", 
                                     size=13)
        )+
        labs(title="Predicted Regression Line Vs Actuals")
      )
    )
    
  })
  
  
  
})
  
