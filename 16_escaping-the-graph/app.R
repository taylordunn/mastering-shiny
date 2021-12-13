library(shiny)


# 16.2 What doesn't the reactive graph capture? ---------------------------

ui <- fluidPage(
  textInput("nm", "name"),
  actionButton("clr", "Clear"),
  textOutput("hi")
)
server <- function(input, output, session) {
  hi <- reactive(paste0("Hi ", input$nm))
  output$hi <- renderText(hi())
  observeEvent(input$clr, {
    updateTextInput(session, "nm", value = "")
  })
}

# 16.3 Case studies -------------------------------------------------------

ui <- fluidPage(
  actionButton("drink", "drink me"),
  actionButton("eat", "eat me"),
  textOutput("notice")
)
server <- function(input, output, session) {
  r <- reactiveValues(notice = "")
  observeEvent(input$drink, {
    r$notice <- "You are no longer thirsty"
  })
  observeEvent(input$eat, {
    r$notice <- "You are no longer hungry"
  })
  output$notice <- renderText(r$notice)
}

ui <- fluidPage(
  actionButton("up", "up"),
  actionButton("down", "down"),
  textOutput("n")
)
server <- function(input, output, session) {
  r <- reactiveValues(n = 0)
  observeEvent(input$up, {
    r$n <- r$n + 1
  })
  observeEvent(input$down, {
    r$n <- r$n - 1
  })

  output$n <- renderText(r$n)
}

ui <- fluidPage(
  textInput("name", "name"),
  actionButton("add", "add"),
  textOutput("names")
)
server <- function(input, output, session) {
  r <- reactiveValues(names = character())
  observeEvent(input$add, {
    r$names <- c(input$name, r$names)
    updateTextInput(session, "name", value = "")
  })

  output$names <- renderText(r$names)
}

ui <- fluidPage(
  textInput("name", "name"),
  actionButton("add", "add"),
  actionButton("del", "delete"),
  textOutput("names")
)
server <- function(input, output, session) {
  r <- reactiveValues(names = character())
  observeEvent(input$add, {
    r$names <- union(r$names, input$name)
    updateTextInput(session, "name", value = "")
  })
  observeEvent(input$del, {
    r$names <- setdiff(r$names, input$name)
    updateTextInput(session, "name", value = "")
  })

  output$names <- renderText(r$names)
}

ui <- fluidPage(
  actionButton("start", "start"),
  actionButton("stop", "stop"),
  textOutput("n")
)
server <- function(input, output, session) {
  r <- reactiveValues(running = FALSE, n = 0)

  observeEvent(input$start, {
    r$running <- TRUE
  })
  observeEvent(input$stop, {
    r$running <- FALSE
  })

  observe({
    if (r$running) {
      r$n <- isolate(r$n) + 1
      invalidateLater(250)
    }
  })
  output$n <- renderText(r$n)
}

# 16.3.4 Exercises --------------------------------------------------------

# Exercise 1

ui <- fluidPage(
  actionButton("rnorm", "Normal"),
  actionButton("runif", "Uniform"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  d <- reactiveVal()

  observeEvent(input$rnorm, {
    d(rnorm(100))
  })
  observeEvent(input$runif, {
    d(runif(100))
  })

  output$plot <- renderPlot({
    if (!is.null(d())) hist(d())
  })
}

# Exercise 2

ui <- fluidPage(
  selectInput("type", "type", c("Normal", "Uniform")),
  actionButton("go", "go"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  d <- reactiveVal()

  observeEvent(input$go, {
    if (input$type == "Normal") d(rnorm(100))
    else d(runif(100))
  })

  output$plot <- renderPlot({
    if (!is.null(d())) hist(d())
  })
}

# Exercise 3

ui <- fluidPage(
  actionButton("rnorm", "Normal"),
  actionButton("runif", "Uniform"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  d <- reactive({
    # Doesn't work because actionButton returns a count of button presses
    if (input$rnorm) rnorm(100)
    if (input$runif) runif(100)
  })

  output$plot <- renderPlot({
    if (!is.null(d())) hist(d())
  })
}

ui <- fluidPage(
  selectInput("type", "type", c("Normal", "Uniform")),
  actionButton("go", "go"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  d <- reactive({
    req(input$go)
    if (isolate(input$type) == "Normal") rnorm(100)
    else runif(100)
  })

  output$plot <- renderPlot({
    if (!is.null(d())) hist(d())
  })
}

shinyApp(ui, server)

