library(shiny)
library(tidyverse)
library(glue)

f <- function(x) g(x)
g <- function(x) h(x)
h <- function(x) x * 2

ui1 <- fluidPage(
  selectInput("n", "N", 1:10),
  plotOutput("plot")
)

server1 <- function(input, output, session) {
  output$plot <- renderPlot({
    browser()
    n <- f(input$n)
    plot(head(cars, n))
  }, res = 96)
}

ui2 <- fluidPage(
  sliderInput("x", "x", value = 1, min = 0, max = 10),
  sliderInput("y", "y", value = 2, min = 0, max = 10),
  sliderInput("z", "z", value = 3, min = 0, max = 10),
  textOutput("total")
)
server2 <- function(input, output, session) {
  observeEvent(input$x, {
    message(glue("Updating y from {input$y} to {input$x * 2}"))
    updateSliderInput(session, "y", value = input$x * 2)
  })

  total <- reactive({
    total <- input$x + input$y + input$z
    message(glue("New total is {total}"))
    total
  })

  output$total <- renderText({
    total()
  })
}

shinyApp(ui2, server2)
