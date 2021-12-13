library(tidyverse)
library(shiny)
reactiveConsole(TRUE)

# 15.1 Reactive values ----------------------------------------------------

x <- reactiveVal(10)
x()       # get
x(20)     # set
x()       # get

r <- reactiveValues(x = 10)
r$x       # get
r$x <- 20 # set
r$x       # get


# 15.1.1 Exercises --------------------------------------------------------

l1 <- reactiveValues(a = 1, b = 2)
l2 <- list(a = reactiveVal(1), b = reactiveVal(2))

l1$a # 1
l1$a <- 5
l1$a # 5

l2$a() # 1
l2$a(5)
l2$a() # 5


# 15.2 Reactive expressions -----------------------------------------------

r <- reactive(stop("Error occured at ", Sys.time(), call. = FALSE))
r()


# 15.3 Observers and outputs ----------------------------------------------

y <- reactiveVal(10)
observe({
  message("`y` is ", y())
})

y(5)
y(4)

x <- reactiveVal(1)
y <- observe({
  x()
  observe(print(x()))
})

x(2)
x(3)


# 15.4 Isolating code -----------------------------------------------------

r <- reactiveValues(count = 0, x = 1)
class(r)
#> [1] "rv_flush_on_write" "reactivevalues"
observe({
  r$x
  r$count <- isolate(r$count) + 1
})

r$x <- 1
r$x <- 2
r$count

r$x <- 3
r$count


# 15.5 Timed invalidation -------------------------------------------------

x <- reactive({
  invalidateLater(500)
  rnorm(10)
})

x()
