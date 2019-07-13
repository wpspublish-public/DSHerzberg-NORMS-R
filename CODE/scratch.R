suppressMessages(library(here))
# library(reshape2)
# suppressMessages(library(moderndive))
# library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))
# suppressMessages(library(ggpmisc))
# library(ggrepel)

ant_norms <- read_csv(here('OUTPUT-FILES/ANT_total-raw-SS-lookup.csv'))

agestrat <- c(60, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 93, 96, 102, 108, 
              114, 120, 126, 132, 138, 144, 150, 156, 168, 180, 192, 228)

map(agestrat, ~ ant_norms %>% 
      select(rawscore, paste0('mo_', .x)) %>% 
      rename(SS=paste0('mo_', .x)) %>% 
      rename(!!paste0('mo_', .x):=rawscore) %>%
      select(SS, paste0('mo_', .x)) %>% 
      # complete(SS = 40:160, fill = list(!!paste0('mo_', .x) := '-')) %>%
      complete(SS = 40:160) %>%
      # replace_na(list(paste0('mo_', .x)), '-') %>%
      group_by(SS) %>%
      filter(n() == 1| n() > 1 & row_number()  %in% c(1, n())) %>%
      # summarise(!!paste0('mo_', .x) := str_c(paste0('mo_', .x), collapse = '-')) %>%
      summarise(!!paste0('mo_', .x) := str_c(eval(as.name(paste0('mo_', .x))), collapse = '-')) %>%
      mutate_at(
        vars(
          paste0('mo_', .x)
        ), ~ case_when(
          is.na(.x) ~ '-',
          TRUE ~ .x
        )) %>% 
      arrange(desc(SS)) %>%
      assign(paste0('raw_SS_', .x), ., envir = .GlobalEnv))



# agestrat1 <- agestrat[1:2]

file_names <- paste0('raw_SS_', agestrat)

mylist <- lapply(file_names, get)

norms_pub <- mylist %>% reduce(left_join, by = "SS")

# EVERYTHING BEFORE HERE WORKS


input <- ant_norms %>% select(rawscore, mo_60) %>% rename(raw=rawscore, SS=mo_60)

input_test <- tribble(
  ~raw,	~SS,
  0, 75,
  1,	78,
  2,	80,
  3,	83,
  4,	83,
  5,	83,
  6,	90,
  7,	93,
  8,	95,
  9,	98
)

output <- tribble(
  ~SS, ~raw,
  100, '-',
  99, '-',
  98, '9',
  97, '-',
  96, '-',
  95, '8',
  94, '-',
  93, '7',
  92, '-',
  91, '-',
  90, '6',
  89, '-',
  88, '-',
  87, '-',
  86, '-',
  85, '-',
  84, '-',
  83, '3-5',
  82, '-',
  81, '-',
  80, '2',
  79, '-',
  78, '1',
  77, '-',
  76, '-',
  75, '0',
  74, '-',
  73, '-',
  72, '-',
  71, '-',
  70, '-'
)

out1 <- input  %>% 
  # Expand SS range to 70-100, replace NA raw values in new rows with '-' (doing
  # this changes raw from num to char)
  complete(SS = 40:160, fill = list(raw = '-')) %>%
  # group_by collects SS values with multiple rows on input table
  group_by(SS) %>%
  # summarises collapses rows by the grouping variable SS, so that multiple rows
  # with same SS value become a single row. Within summarise, raw is recoded
  # conditionally. If the number of rows within a single SS group is > 1, range
  # returns a vector of the min and max of the raw values within that group,
  # str_c joins the min and max values into a single string with '-' separating
  # the two numbers. If number of rows with a single SS group = 1, raw is not recoded.
  summarise(raw = if(n() > 1) str_c(range(raw), collapse='-') else raw) %>% 
  # sort descending on SS.
  arrange(desc(SS)) 


interim <- input %>% select(
  SS, raw
    ) %>% 
  mutate_at(
    vars(
      raw
      ), ~ as.character(.x)
    ) %>% 
df1_output <- input %>% select(
  SS, raw
    ) %>% 
  complete(
      SS = 70:100
      ) %>% 
  arrange(
      desc(
        SS
        )
      ) %>% 
  mutate_at(
    vars(
      raw
    ), ~ case_when(
      is.na(.x) ~ '-',
      TRUE ~ .x
    )
  )

# map(agestrat, ~ ant_norms %>% 
#       select(rawscore, paste0('mo_', .x)) %>% 
#       rename(raw=rawscore, SS=paste0('mo_', .x)) %>%
#       complete(SS = 40:160, fill = list(raw = '-')) %>%
#       group_by(SS) %>% 
#       filter(n() == 1| n() > 1 & row_number()  %in% c(1, n())) %>% 
#       summarise(raw = str_c(raw, collapse = '-')) %>%
#       arrange(desc(SS)) %>%
#       rename(!!paste0('mo_', .x):=SS) %>%
#       assign(paste0('raw_SS_', .x), ., envir = .GlobalEnv))



# ant_norms %>%
#   select(rawscore, mo_60) %>%
#   rename(SS=mo_60) %>%
#   rename(mo_60=rawscore) %>% 
#   select(SS, mo_60) %>% 
#   complete(SS = 40:160, fill = list(mo_60 = '-')) %>%
#   group_by(SS) %>%
#   filter(n() == 1| n() > 1 & row_number()  %in% c(1, n())) %>%
#   summarise(mo_60 = str_c(mo_60, collapse = '-')) %>%
#   arrange(desc(SS)) %>%
#   assign('raw_SS_60', ., envir = .GlobalEnv)

# map(agestrat, ~ ant_norms %>% 
#       select(rawscore, paste0('mo_', .x)) %>% 
#       rename(SS=paste0('mo_', .x)) %>% 
#       rename(!!paste0('mo_', .x):=rawscore) %>%
#       complete(SS = 40:160, fill = list(paste0('mo_', .x) = '-')) %>%
#       group_by(SS) %>% 
#       filter(n() == 1| n() > 1 & row_number()  %in% c(1, n())) %>% 
#       summarise(paste0('mo_', .x) = str_c(paste0('mo_', .x), collapse = '-')) %>%
#       # arrange(desc(SS)) %>%      
#       arrange(desc(paste0('mo_', .x))) %>%
#       # rename(!!paste0('mo_', .x):=SS) %>%
#       assign(paste0('raw_SS_', .x), ., envir = .GlobalEnv))

# map(agestrat, ~ ant_norms %>% 
#       select(rawscore, paste0('mo_', .x)) %>% 
#       rename(raw=rawscore, SS=paste0('mo_', .x)) %>%
#       complete(SS = 40:160, fill = list(raw = '-')) %>%
#       group_by(SS) %>% 
#       filter(n() == 1| n() > 1 & row_number()  %in% c(1, n())) %>% 
#       summarise(raw = str_c(raw, collapse = '-')) %>%
#       arrange(desc(SS)) %>%
#       rename(!!paste0('mo_', .x):=SS) %>%
#       assign(paste0('raw_SS_', .x), ., envir = .GlobalEnv))

