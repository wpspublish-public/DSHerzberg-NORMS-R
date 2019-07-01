library(mise)
library(purrr)
library(magrittr)
library(tidyverse)

# create freq table by age based on ersatz data.
ages <- c(5,6,7,8)
freq_table <- function(x) {
  set.seed(x)
  tibble(score = abs(round(rnorm(10000) * 100))) %>% count(score) %>%
    mutate(
      perc = round(100 * (n / sum(n)), 4),
      cum_per = round(100 * (cumsum(n) / sum(n)), 4),
      age = x
    ) %>% select(age, everything()) %>% assign(paste0('df', x), ., envir = .GlobalEnv)
}
freq_list<- rbind(map(ages, freq_table))
freq_table_age <- invoke(.f=rbind, .x=freq_list, make.row.names = FALSE)
rm(df5,df6,df7,df8)

