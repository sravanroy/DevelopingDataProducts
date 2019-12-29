
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
                    #rmse {
                    color: white;
                    background: #834138;
                    font-family: 'Times New Roman', Times, serif;
                    font-size: 25px;
                    font-style: italic;
                    text-align: center;
                    }
                    #mae {
                    color: white;
                    background: #0C3437;
                    font-family: 'Times New Roman', Times, serif;
                    font-size: 25px;
                    font-style: italic;
                    text-align: center;
                    }
                    "))
    ),

  titlePanel(h2("Regression Analysis to predict average rating of a board game",align="center",face="bold")),
  fluidRow(class = "row1",
    column(4,div(style = "height:500px;",
                 br(),
                 br(),
                 tags$ul(
                   tags$li(
                     tags$i(h4("The purpose of this R shiny dashboard is to perform various regression techniques
                               to predict average rating of a board game. The board games data is scraped",
                               tags$a(href="https://www.boardgamegeek.com/", "online")," and can be 
                               found in my ",tags$a(href="www.rstudio.com", "git repo"),"")
                     )),
                   tags$li(
                     tags$i(h4("First, select the training and test size percentage split in the below 
                               slider to train a particular regression model")
                     )),
                   tags$li(
                     tags$i(h4("Next, choose a regression technique to perform regression analysis for predicting 
                               average rating of board games")
                     )),
                   tags$li(
                     tags$i(h4("The plots to the right side give us important information related to the choosen 
                               regression model")
                     )),
                   tags$li(
                     tags$i(h4("By dynamically changing the regression techniques and interpreting the individual plots,
                               we can make better decisions while finalizing the model!")
                     ))
                   )
          
          )),
    column(8,
           plotOutput("plot",height = '500px')
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
           tags$b(tags$i(h2("Choose the regression technique :"))),
           br(),
           radioButtons("var",h4("Type of fit :"),
                        c("Linear fit" = "linear",
                          "Ridge fit" = "ridge",
                          "Lasso fit" = "lasso",
                          "Principal Component Regression" = "pcr",
                          "Partial Least Squares" = "pls"))
           
           ),
    column(4,
           h3("Root Mean Square Error (RMSE) :"),
           br(),
           textOutput("rmse"),
           hr(),
           
           h3("Mean Absolute Error (MAE) :"),
           br(),
           textOutput("mae")
           )
  ),
  fluidRow(
    column(3,
           tags$ul(
             tags$li(
               tags$i(h4("A slider to choose the split of train and test size as a % of number of rows")
               )),
              tags$li(
                tags$i(h4("The slider range has been restricted to a mininum of 50 and a maximum of 90 for practical purposes")
                )))),
    column(4,offset = 1,
           tags$ul(
             tags$li(
               tags$i(h4("Choose the type of regression analysis to be performed on the board games data")
             )),
           tags$li(
             tags$i(h4("The plots generated for each fit represents the tuned hyper parameters while training the model")
             )))),
    column(4,
           tags$ul(
             tags$li(
               tags$i(h4("RMSE is the standard deviation of the residuals(prediction errors). Residuals are a measure of
                         how far from the regression line data points are; and tells how concentrated the data is around
                         the line of best fit")
               )),
             tags$li(
               tags$i(h4("MAE is a measure of difference between two continuous variables. It is the average of all
                         absolute errors")
               ))))
    )
  
))


