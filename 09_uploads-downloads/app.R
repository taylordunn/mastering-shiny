library(shiny)

# 9.1 Upload -----------------------------------------------

ui <- fluidPage(
  fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
  tableOutput("files")
)
server <- function(input, output, session) {
  output$files <- renderTable(input$upload)
}

ui <- fluidPage(
  fileInput("upload", NULL, accept = c(".csv", ".tsv")),
  numericInput("n", "Rows", value = 5, min = 1, step = 1),
  tableOutput("head")
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })

  output$head <- renderTable({
    head(data(), input$n)
  })
}


# 9.2 Download -----------------------------------------------

ui <- fluidPage(
  selectInput("dataset", "Pick a dataset", ls("package:datasets")),
  tableOutput("preview"),
  downloadButton("download", "Download .tsv")
)

server <- function(input, output, session) {
  data <- reactive({
    out <- get(input$dataset, "package:datasets")
    if (!is.data.frame(out)) {
      validate(paste0("'", input$dataset, "' is not a data frame"))
    }
    out
  })

  output$preview <- renderTable({
    head(data())
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$dataset, ".tsv")
    },
    content = function(file) {
      vroom::vroom_write(data(), file)
    }
  )
}

# 9.3 Case study -----------------------------------------------

ui_upload <- sidebarLayout(
  sidebarPanel(
    fileInput("file", "Data", buttonLabel = "Upload..."),
    textInput("delim", "Delimiter (leave blank to guess)", ""),
    numericInput("skip", "Rows to skip", 0, min = 0),
    numericInput("rows", "Rows to preview", 10, min = 1)
  ),
  mainPanel(
    h3("Raw data"),
    tableOutput("preview1")
  )
)

ui_clean <- sidebarLayout(
  sidebarPanel(
    checkboxInput("snake", "Rename columns to snake case?"),
    checkboxInput("constant", "Remove constant columns?"),
    checkboxInput("empty", "Remove empty cols?")
  ),
  mainPanel(
    h3("Cleaner data"),
    tableOutput("preview2")
  )
)

ui_download <- fluidRow(
  column(width = 12, downloadButton("download", class = "btn-block"))
)

ui <- fluidPage(
  ui_upload,
  ui_clean,
  ui_download
)

server <- function(input, output, session) {
  # Upload ---------------------------------------------------------
  raw <- reactive({
    req(input$file)
    delim <- if (input$delim == "") NULL else input$delim
    vroom::vroom(input$file$datapath, delim = delim, skip = input$skip)
  })
  output$preview1 <- renderTable(head(raw(), input$rows))

  # Clean ----------------------------------------------------------
  tidied <- reactive({
    out <- raw()
    if (input$snake) {
      names(out) <- janitor::make_clean_names(names(out))
    }
    if (input$empty) {
      out <- janitor::remove_empty(out, "cols")
    }
    if (input$constant) {
      out <- janitor::remove_constant(out)
    }

    out
  })
  output$preview2 <- renderTable(head(tidied(), input$rows))

  # Download -------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), ".tsv")
    },
    content = function(file) {
      vroom::vroom_write(tidied(), file)
    }
  )
}

# 9.4 Exercises -----------------------------------------------

# Exercise 2

ui <- fluidPage(
  fileInput("upload", NULL, buttonLabel = "Upload...", multiple = FALSE),
  selectInput("data_var", "Choose a variable", choices = NULL),
  tableOutput("file_head"),
  verbatimTextOutput("ttest")
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })

  observeEvent(data(),
    updateSelectInput(inputId = "data_var", choices = names(data()))
  )

  output$ttest <- renderPrint(
    t.test(data()[input$data_var])
  )

  output$file_head <- renderTable({
    head(data())
  })
}

# Exercise 3

ui <- fluidPage(
  fileInput("upload", NULL, buttonLabel = "Upload...", multiple = FALSE),
  selectInput("data_var", "Choose a variable", choices = NULL),
  plotOutput("hist"),
  downloadButton("download", "Download PNG")
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })

  observeEvent(data(),
    updateSelectInput(inputId = "data_var", choices = names(data()))
  )

  data_var_hist <- reactive({
    req(data(), input$data_var)
    ggplot(data = data(), aes_string(x = input$data_var)) +
      geom_histogram(bins = 20)
  })

  output$hist <- renderPlot({data_var_hist()}, res = 72)

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$upload$name, ".png")
    },
    content = function(file) {
      ggsave(file, data_var_hist())
    }
  )
}

shinyApp(ui, server)
