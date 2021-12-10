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
gapminder <- gapminder %>% mutate(country = as.character(country))

ui <- fluidPage(
  selectInput("continent", "Continent", choices = continents),
  selectInput("country", "Country", choices = NULL),
  tableOutput("data")
)

server <- function(input, output, session) {
  observeEvent(input$continent, {
    updateSelectInput(
      inputId = "country",
      choices = unique(filter(gapminder, continent == input$continent)$country)
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
    if(input$continent == "All") {
      countries <- unique(gapminder$country)
    } else {
      countries <-
        unique(filter(gapminder, continent == input$continent)$country)
    }
    updateSelectInput(
      inputId = "country",
      choices = countries
    )
  })

  output$data <- renderTable({
    req(input$country)
    gapminder %>% filter(country == input$country)
  })
}

# 10.2 Dynamic visibility -------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("controller", "Show", choices = paste0("panel", 1:3))
    ),
    mainPanel(
      tabsetPanel(
        id = "switcher",
        type = "hidden",
        tabPanelBody("panel1", "Panel 1 content"),
        tabPanelBody("panel2", "Panel 2 content"),
        tabPanelBody("panel3", "Panel 3 content")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$controller, {
    updateTabsetPanel(inputId = "switcher", selected = input$controller)
  })
}

parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("normal",
           numericInput("mean", "mean", value = 1),
           numericInput("sd", "standard deviation", min = 0, value = 1)
  ),
  tabPanel("uniform",
           numericInput("min", "min", value = 0),
           numericInput("max", "max", value = 1)
  ),
  tabPanel("exponential",
           numericInput("rate", "rate", value = 1, min = 0),
  )
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Distribution",
                  choices = c("normal", "uniform", "exponential")
      ),
      numericInput("n", "Number of samples", value = 100),
      parameter_tabs,
    ),
    mainPanel(
      plotOutput("hist")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$dist, {
    updateTabsetPanel(inputId = "params", selected = input$dist)
  })

  sample <- reactive({
    switch(input$dist,
           normal = rnorm(input$n, input$mean, input$sd),
           uniform = runif(input$n, input$min, input$max),
           exponential = rexp(input$n, input$rate)
    )
  })
  output$hist <- renderPlot(hist(sample()), res = 96)
}

ui <- fluidPage(
  tabsetPanel(
    id = "wizard",
    type = "hidden",
    tabPanel("page_1",
             "Welcome!",
             actionButton("page_12", "next")
    ),
    tabPanel("page_2",
             "Only one page to go",
             actionButton("page_21", "prev"),
             actionButton("page_23", "next")
    ),
    tabPanel("page_3",
             "You're done!",
             actionButton("page_32", "prev")
    )
  )
)

server <- function(input, output, session) {
  switch_page <- function(i) {
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  }

  observeEvent(input$page_12, switch_page(2))
  observeEvent(input$page_21, switch_page(1))
  observeEvent(input$page_23, switch_page(3))
  observeEvent(input$page_32, switch_page(2))
}


## 10.2.3 Exercises --------------------------------------------------------

p <- ggplot(diamonds, aes(carat))

plot_tabset <- tabsetPanel(
  id = "plot_tabset", type = "hidden",
  tabPanel("geom_histogram",
           numericInput("bins", "Number of bins",
                        min = 3, max = 100, value = 20)),
  tabPanel("geom_freqpoly",
           numericInput("binwidth", "Bin width",
                        value = 0.5, min = 0.2, max = 2.0)),
  tabPanel("geom_density",
           numericInput("bandwidth_adjust", "Bandwidth adjustment factor",
                        value = 1.0, min = 0.1, max = 2.0))
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_func", "Plotting function",
                  choices = c("geom_histogram", "geom_freqpoly", "geom_density")),
      plot_tabset
    ),
    mainPanel(
      plotOutput("carat_distribution")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$plot_func,
               updateTabsetPanel(inputId = "plot_tabset",
                                 selected = input$plot_func))

  p_dist <- reactive({
    switch(
      input$plot_func,
      geom_histogram = p + geom_histogram(bins = input$bins),
      geom_freqpoly = p + geom_freqpoly(binwidth = input$binwidth),
      geom_density = p + geom_density(adjust = input$bandwidth_adjust),
           uniform = runif(input$n, input$min, input$max),
           exponential = rexp(input$n, input$rate)
    )
  })
  output$carat_distribution <- renderPlot(p_dist(), res = 96)
}


# 10.3 Creating UI with code ----------------------------------------------

ui <- fluidPage(
  textInput("label", "label"),
  selectInput("type", "type", c("slider", "numeric")),
  uiOutput("numeric")
)
server <- function(input, output, session) {
  output$numeric <- renderUI({
    if (input$type == "slider") {
      sliderInput("dynamic", input$label, value = 0, min = 0, max = 10)
    } else {
      numericInput("dynamic", input$label, value = 0, min = 0, max = 10)
    }
  })
}

ui <- fluidPage(
  numericInput("n", "Number of colours", value = 5, min = 1),
  uiOutput("col"),
  textOutput("palette")
)

server <- function(input, output, session) {
  col_names <- reactive(paste0("col", seq_len(input$n)))

  output$col <- renderUI({
    map(col_names(), ~ textInput(.x, NULL))
  })

  output$palette <- renderText({
    map_chr(col_names(), ~ input[[.x]] %||% "")
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of colours", value = 5, min = 1),
      uiOutput("col"),
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  col_names <- reactive(paste0("col", seq_len(input$n)))

  output$col <- renderUI({
    map(col_names(), ~ textInput(.x, NULL, value = isolate(input[[.x]])))
  })

  output$plot <- renderPlot({
    cols <- map_chr(col_names(), ~ input[[.x]] %||% "")
    # convert empty inputs to transparent
    cols[cols == ""] <- NA

    barplot(
      rep(1, length(cols)),
      col = cols,
      space = 0,
      axes = FALSE
    )
  }, res = 96)
}

make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      map(names(iris), ~ make_ui(iris[[.x]], .x))
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)
server <- function(input, output, session) {
  selected <- reactive({
    each_var <- map(names(iris), ~ filter_var(iris[[.x]], input[[.x]]))
    reduce(each_var, ~ .x & .y)
  })

  output$data <- renderTable(head(iris[selected(), ], 12))
}

dfs <- keep(ls("package:datasets"), ~ is.data.frame(get(.x, "package:datasets")))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", label = "Dataset", choices = dfs),
      uiOutput("filter")
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)
server <- function(input, output, session) {
  data <- reactive({
    get(input$dataset, "package:datasets")
  })
  vars <- reactive(names(data()))

  output$filter <- renderUI(
    map(vars(), ~ make_ui(data()[[.x]], .x))
  )

  selected <- reactive({
    each_var <- map(vars(), ~ filter_var(data()[[.x]], input[[.x]]))
    reduce(each_var, `&`)
  })

  output$data <- renderTable(head(data()[selected(), ], 12))
}

## 10.3.5 Exercises --------------------------------------------------------

# Exercise 1

ui <- fluidPage(
  selectInput("type", "type", c("slider", "numeric")),
)
server <- function(input, output, session) {
  output$numeric <- renderUI({
    if (input$type == "slider") {
      sliderInput("n", "n", value = 0, min = 0, max = 100)
    } else {
      numericInput("n", "n", value = 0, min = 0, max = 100)
    }
  })
}

ui <- fluidPage(
  selectInput("input_type", "Input type", choices = c("slider", "numeric")),
  tabsetPanel(
    id = "type_panel",
    type = "hidden",
    tabPanel(
      "slider",
      sliderInput("n", "n", value = 0, min = 0, max = 100)
    ),
    tabPanel(
      "numeric",
      numericInput("n", "n", value = 0, min = 0, max = 100)
    )
  )
)
server <- function(input, output, session) {
  isolate(input$n)
  observeEvent(
    input$input_type,
    updateTabsetPanel(inputId = "type_panel", selected = input$input_type)
  )
}

# Exercise 2

ui <- fluidPage(
  actionButton("go", "Enter password"),
  textOutput("text")
)
server <- function(input, output, session) {
  observeEvent(input$go, {
    showModal(modalDialog(
      passwordInput("password", NULL),
      title = "Please enter your password"
    ))
  })

  output$text <- renderText({
    browser()
    if (!isTruthy(input$password)) {
      "No password"
    } else {
      "Password entered"
    }
  })
}

shinyApp(ui, server)
