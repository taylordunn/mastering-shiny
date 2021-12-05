library(shiny)
library(tidyverse, warn.conflicts = FALSE)


# 10.1 Updating inputs ------------------------------------------------------


ui <- fluidPage(
  numericInput("min", "Minimum", 0),
  numericInput("max", "Maximum", 3),
  sliderInput("n", "n", min = 0, max = 3, value = 1)
)
server <- function(input, output, session) {
  observeEvent(input$min, {
    updateSliderInput(inputId = "n", min = input$min)
  })
  observeEvent(input$max, {
    updateSliderInput(inputId = "n", max = input$max)
  })
}

ui <- fluidPage(
  sliderInput("x1", "x1", 0, min = -10, max = 10),
  sliderInput("x2", "x2", 0, min = -10, max = 10),
  sliderInput("x3", "x3", 0, min = -10, max = 10),
  actionButton("reset", "Reset")
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateSliderInput(inputId = "x1", value = 0)
    updateSliderInput(inputId = "x2", value = 0)
    updateSliderInput(inputId = "x3", value = 0)
  })
}

ui <- fluidPage(
  numericInput("n", "Simulations", 10),
  actionButton("simulate", "Simulate")
)

server <- function(input, output, session) {
  observeEvent(input$n, {
    label <- paste0("Simulate ", input$n, " times")
    updateActionButton(inputId = "simulate", label = label)
  })
}

sales <- vroom::vroom("sales_data_sample.csv",
                      col_types = list(), na = "")
ui <- fluidPage(
  selectInput("territory", "Territory", choices = unique(sales$TERRITORY)),
  selectInput("customername", "Customer", choices = NULL),
  selectInput("ordernumber", "Order number", choices = NULL),
  tableOutput("data")
)
server <- function(input, output, session) {
  territory <- reactive({
    filter(sales, TERRITORY == input$territory)
  })
  observeEvent(territory(), {
    choices <- unique(territory()$CUSTOMERNAME)
    updateSelectInput(inputId = "customername", choices = choices)
  })

  customer <- reactive({
    req(input$customername)
    filter(territory(), CUSTOMERNAME == input$customername)
  })
  observeEvent(customer(), {
    choices <- unique(customer()$ORDERNUMBER)
    updateSelectInput(inputId = "ordernumber", choices = choices)
  })

  output$data <- renderTable({
    req(input$ordernumber)
    customer() %>%
      filter(ORDERNUMBER == input$ordernumber) %>%
      select(QUANTITYORDERED, PRICEEACH, PRODUCTCODE)
  })
}

ui <- fluidPage(
  selectInput("dataset", "Choose a dataset", c("pressure", "cars")),
  selectInput("column", "Choose column", character(0)),
  verbatimTextOutput("summary")
)

server <- function(input, output, session) {
  dataset <- reactive(get(input$dataset, "package:datasets"))

  observeEvent(input$dataset, {
    freezeReactiveValue(input, "column")
    updateSelectInput(inputId = "column", choices = names(dataset()))
  })

  output$summary <- renderPrint({
    summary(dataset()[[input$column]])
  })
}

## 10.1.6 Exercises --------------------------------------------------------

# Exercise 1

ui <- fluidPage(
  numericInput("year", "year", value = 2020),
  dateInput("date", "date")
)
server <- function(input, output, session) {
  observeEvent(input$year, {
    updateDateInput(inputId = "date",
                    min = paste0(input$year, "-01-01"),
                    max = paste0(input$year, "-12-31"))
  })
}

# Exercise 2

library(openintro, warn.conflicts = FALSE)
states <- unique(county$state)

ui <- fluidPage(
  selectInput("state", "State", choices = states),
  selectInput("county", "County", choices = NULL)
)

server <- function(input, output, session) {
  observeEvent(input$state, {
    updateSelectInput(
      inputId = "county",
      choices = filter(county, state == input$state)$name,
      label = case_when(input$state == "Louisiana" ~ "Parish",
                        input$state == "Alaska" ~ "Borough",
                        TRUE ~ "County")
    )
  })
}

# Exercise 3

library(gapminder)
continents <- unique(gapminder$continent)

ui <- fluidPage(
  selectInput("continent", "Continent", choices = continents),
  selectInput("country", "Country", choices = NULL),
  tableOutput("data")
)

server <- function(input, output, session) {
  observeEvent(input$continent, {
    updateSelectInput(
      inputId = "country",
      choices = filter(gapminder, continent == input$continent)$country
    )
  })

  output$data <- renderTable({
    req(input$country)
    gapminder %>%
      filter(continent == input$continent, country == input$country)
  })
}

# Exercise 4

continents <- c("All", as.character(continents))

ui <- fluidPage(
  selectInput("continent", "Continent", choices = continents),
  selectInput("country", "Country", choices = NULL),
  tableOutput("data")
)
server <- function(input, output, session) {
  observeEvent(input$continent, {
    updateSelectInput(
      inputId = "country",
      choices = ifelse(
        input$continent == "All", gapminder$country,
        filter(gapminder, continent == input$continent)$country
      )
    )
  })

  output$data <- renderTable({
    req(input$country)
    gapminder %>% filter(country == input$country)
  })
}

# 10.2 Dynamic visibility -------------------------------------------------


## 10.2.3 Exercises --------------------------------------------------------


# 10.3 Creating UI with code ----------------------------------------------


## 10.3.5 Exercises --------------------------------------------------------


shinyApp(ui, server)
