library(tidyverse)
library(vroom)
library(here)

data_dir <- here("04_basic-case-study", "neiss")
dir.create(data_dir)
download <- function(name) {
  url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
  download.file(paste0(url, name),
                paste0(data_dir, "/", name), quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom::vroom(here(data_dir, "injuries.tsv.gz"))
injuries
products <- vroom::vroom(here(data_dir, "products.tsv"))
products
population <- vroom::vroom(here(data_dir, "population.tsv"))
population

injuries_selected <- injuries %>% filter(prod_code == 649)
nrow(injuries_selected)

injuries_selected %>% count(location, wt = weight, sort = T)
injuries_selected %>% count(body_part, wt = weight, sort = T)
injuries_selected %>% count(diag, wt = weight, sort = T)

injuries_selected %>%
  count(age, sex, wt = weight) %>%
  ggplot(aes(age, n, color = sex)) +
  geom_line() +
  labs(y = "Estimated number of injuries")

injuries_selected %>%
  count(age, sex, wt = weight) %>%
  left_join(population, by = c("age", "sex")) %>%
  mutate(rate = n / population * 1e4) %>%
  ggplot(aes(age, rate, color = sex)) +
  geom_line(na.rm = TRUE) +
  labs(y = "Injuries per 10,000 people")

