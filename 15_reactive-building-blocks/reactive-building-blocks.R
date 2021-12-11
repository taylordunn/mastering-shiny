library(tidyverse)
library(shiny)
reactiveConsole(TRUE)

x <- reactiveVal(10)
x()       # get
x(20)     # set
x()       # get

r <- reactiveValues(x = 10)
r$x       # get
r$x <- 20 # set
r$x       # get
