library(shiny)

animals <- list(
  "typical" = c("dog", "cat"),
  "atypical" = c("mouse", "bird"),
  "other" = c("other 1", "other 2"),
  "weird" = c("I hate animals", "I like all animals")
)

ui <- fluidPage(
  textInput("name", label = NULL, placeholder = "Your name"),
  sliderInput("delivery_date", label = "When should we deliver",
              min = as.Date("2020-09-16"), max = as.Date("2020-09-23"),
              value = as.Date("2020-09-17")),

  sliderInput("increment5", "Select a number",
              min = 0, max = 100, value = 0, step = 5, animate = TRUE),

  selectInput("animal", "The best animal", animals),

  textOutput("text"),
  verbatimTextOutput("code")
)

server <- function(input, output, session) {
  output$text <- renderText("Hello!")

  output$code <- renderPrint(summary(1:10))
}

shinyApp(ui, server)
