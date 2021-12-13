library(shiny)

# 15.2.3 Exercises

ui <- fluidPage(
  checkboxInput("error", "error?"),
  textOutput("result")
)
server <- function(input, output, session) {
  a <- reactive({
    if (input$error) {
      stop("Error!")
    } else {
      1
    }
  })
  b <- reactive(a() + 1)
  c <- reactive(b() + 1)
  output$result <- renderText(c())
}

server <- function(input, output, session) {
  a <- reactive({
    req(input$error, cancelOutput = TRUE)
    1
  })
  b <- reactive(a() + 1)
  c <- reactive(b() + 1)
  output$result <- renderText(c())
}


# 15.4.3 Exercises

ui <- fluidPage(
  numericInput("x", "x", value = 50, min = 0, max = 100),
  actionButton("capture", "capture"),
  textOutput("out")
)

server <- function(input, output, session) {
  out_x <- reactiveVal(input$x)

  observeEvent(input$capture, {out_x(input$x)})

  output$out <- renderText(out_x())
}

shinyApp(ui, server)
