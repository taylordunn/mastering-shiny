library(tidyverse)
library(vroom)

data_dir <- "R/shiny/mastering-shiny/04_basic-case-study/neiss/"
dir.create(data_dir)
download <- function(name) {
  url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
  download.file(paste0(url, name),
                paste0(data_dir, name), quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom::vroom(paste0(data_dir, "injuries.tsv.gz"))
injuries
products <- vroom::vroom(paste0(data_dir, "products.tsv"))
products
population <- vroom::vroom(paste0(data_dir, "population.tsv"))
population

selected <- injuries %>% filter(prod_code == 649)
nrow(selected)
