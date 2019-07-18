
library(shiny)
library(plotly)

sliderInput2 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
  x <- sliderInput(inputId, label, min, max, value, step,width = '600px')
  x$children[[2]]$attribs <- c(x$children[[2]]$attribs,
                               "data-from-min" = from_min,
                               "data-from-max" = from_max,
                               "data-from-shadow" = TRUE)
  x
}

shinyUI(fluidPage(
  titlePanel("Multiple Linear Regression"),
  
  plotlyOutput("plot",height = "600px"),
  hr(),
  fluidRow(style='border:1px solid',
    column(3,style='width:40px,border-right:1px solid',
      h3("Splitting Train and Test sets :"),
      tags$br(),
     
      sliderInput2("slider", h4("Train/Test Split (in %) :"),
                   min = 0, max = 100, value = 80, step = 5,from_min = 50, from_max = 90),
      tags$br()
    ),
    column(4,offset = 1,style='border-left:1px solid',
           h3("Choosing the predictor variable(s) :"),
           br(),
           checkboxInput("disp","Include Displacement",value = TRUE),
           checkboxInput("wt","Include Weight",value = FALSE),
           checkboxInput("carb","Include Carburetors",value = FALSE),
           checkboxInput("cyl","Include Cylinders",value = FALSE)
           
           ),
    column(4,
           tags$blockquote(h3("Mean Absolute Percentage Error (MAPE):")),
           textOutput("err"),
           hr(),
           
           tags$blockquote(h3("Accuracy of the model :")),
           textOutput("acc")
           )
  ),
  br()
  
))


