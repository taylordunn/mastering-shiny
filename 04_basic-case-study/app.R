library(shiny)
#library(ggplot2)
library(tidyverse)
library(vroom)
library(here)

data_dir <- here("04_basic-case-study", "neiss")
injuries <- vroom::vroom(here(data_dir, "injuries.tsv.gz"))
products <- vroom::vroom(here(data_dir, "products.tsv"))
population <- vroom::vroom(here(data_dir, "population.tsv"))

prod_codes <- setNames(products$prod_code, products$title)

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)), .groups = "drop")
}

ui <- fluidPage(
  fluidRow(
    column(6,
      selectInput("code", "Product", choices = prod_codes, width = "100%")
    ),
    column(2,
      selectInput("y", "Y axis", c("rate", "count"))
    ),
    column(3,
      sliderInput("tab_nrows", "Table rows", min = 2, max = 20, value = 5)
    ),
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("prev_story", "Previous story")),
    column(2, actionButton("next_story", "Next story")),
    column(8, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  d_selected <- reactive(
      injuries %>% filter(prod_code == input$code)
  )

  d_summary <- reactive(
    d_selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  )

  output$diag <- renderTable(
    d_selected() %>% count_top(diag, n = input$tab_nrows - 1),
    width = "100%"
  )
  output$body_part <- renderTable(
    d_selected() %>% count_top(body_part, n = input$tab_nrows - 1),
    width = "100%"
  )
  output$location <- renderTable(
    d_selected() %>% count_top(location, n = input$tab_nrows - 1),
    width = "100%"
  )

  output$age_sex <- renderPlot({
    if (input$y == "count") {
      d_summary() %>%
        ggplot(aes(age, n, color = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      d_summary() %>%
        ggplot(aes(age, rate, color = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)

  story_index <- reactiveVal(1)
  observeEvent(d_selected(), story_index(1))
  observeEvent(
    input$prev_story,
    story_index((story_index() - 2) %% nrow(d_selected()) + 1)
  )
  observeEvent(
    input$next_story,
    story_index((story_index() + 1) %% nrow(d_selected()))
  )

  narrative_sample <- eventReactive(
    list(story_index(), d_selected()),
    pull(d_selected(), narrative)[story_index()]
  )
  output$narrative <- renderText(narrative_sample())
}

shinyApp(ui, server)
