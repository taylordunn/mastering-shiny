library(tidyverse)
library(shiny)

reactiveConsole(TRUE)
temp_c <- reactiveVal(10) # create
temp_c() # get
temp_c(20)
temp_c()

temp_f <- reactive({
  message("Converting..")
  (temp_c() * 9 / 5) + 32
})

temp_f()
temp_c(-5)
temp_f()
