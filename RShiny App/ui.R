
library(shiny)
library(plotly)
library(ggplot2)

sliderInput2 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
  x <- sliderInput(inputId, label, min, max, value, step,width = '600px')
  x$children[[2]]$attribs <- c(x$children[[2]]$attribs,
                               "data-from-min" = from_min,
                               "data-from-max" = from_max,
                               "data-from-shadow" = TRUE)
  x
}

shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("
                    #err {
                    color: white;
                    background: #834138;
                    font-family: 'Times New Roman', Times, serif;
                    font-size: 25px;
                    font-style: italic;
                    text-align: center;
                    }
                    #acc {
                    color: white;
                    background: #0C3437;
                    font-family: 'Times New Roman', Times, serif;
                    font-size: 25px;
                    font-style: italic;
                    text-align: center;
                    }
                    "))
    ),

  titlePanel(h2("Linear Regression",align="center",face="bold")),
  fluidRow(class = "row1",
    
    column(4,div(style = "height:500px;background-color: #F3F2F7;color:#1E0B26", 
                 br(),
                 br(),
           h3("This plot depicts the predicted values of horse power(in black) when regressed with one of it's variables(in red) - Displacement/Weight/Qsec/MPG in the dataset 'mtcars'."),
           h3("The black line represents the regression line of HP values predicted by the above mentioned four variables."),
          h3 ("The predictor varibles that are more accurate in predicting the HP are those that are highly correlated with HP!
              "))),
    column(8,
           plotlyOutput("plot",height = '500px')
            )
  ),
 
  fluidRow(style='border:1px solid',
    column(3,style='width:40px,border-right:1px solid',
      h3("Split Train and Test sets :"),
      tags$br(),
     
      sliderInput2("slider", h4("Train/Test Split (in %) :"),
                   min = 0, max = 100, value = 80, step = 5,from_min = 50, from_max = 90),
      tags$br()
    ),
    column(4,offset = 1,
           h3("Choose the predictor variable :"),
           br(),
           radioButtons("var",h4("Predictor variable"),
                        c("Displacement" = "disp",
                          "Weight" = "wt",
                          "Qsec" = "qsec",
                          "Miles/gallon" = "mpg"))
           
           ),
    column(4,
           h3("Mean Absolute Percentage Error (MAPE):"),
           br(),
           textOutput("err"),
           hr(),
           
           h3("Accuracy of the model :"),
           br(),
           textOutput("acc")
           )
  ),
  fluidRow(
    column(3,
           tags$ul(
             tags$li(
               tags$i(h3("A slider to choose the split of train and test size as a % of number of rows")
               )),
              tags$li(
                tags$i(h3("The slider range has been restricted to a mininum of 50 and a maximum of 90 for practical purposes")
                )))),
    column(4,offset = 1,
           tags$ul(
             tags$li(
               tags$i(h3("Choose a predictor variable which is used in the linear regression model to predict the HP")
             )),
           tags$li(
             tags$i(h3("The variables displayed in the menu reflect wide range of correlations with HP starting from high to low")
             )))),
    column(4,
           tags$ul(
             tags$li(
               tags$i(h3("MAPE measures the mean absolute percentage error of predicted values from that of actual values")
               )),
             tags$li(
               tags$i(h3("Accuracy is calulated as 1-MAPE to display the accuracy of model")
               ))))
    )
  
))


