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

# Next code prepares print-ready raw-to-SS lookup tables, with SS in far left
# column, sorted descending, and raw columns for each agestrat, with raw ranges
# per SS value (e.g., '31-61')
# Iteration over all agestrats is done using purrr::map
map(
  agestrat,
  ~ ant_norms %>%
    # select SS columns and rawscore
    select(paste0('mo_', .x), rawscore) %>%
    # rename those columns so that the agestrat label refers to the rawscore column
    rename(SS = paste0('mo_', .x),!!paste0('mo_', .x) := rawscore) %>%
    # expand the table vertically, adding new rows, so there's a row for every possible SS value
    complete(SS = 40:160) %>%
    # because SS-to-raw is one-to-many, there will be rows with identical values
    # of SS, collect these rows into group, so that there is one group for each
    # value of SS
    group_by(SS) %>%
    # filter step retains all 1-row groups, and the first and last rows of any
    # multi-row groups. n() == 1 returns 1-row groups; n() > 1 & row_number()
    # %in% c(1, n()) returns rows of multi-row groups with the row number of
    # either 1 (first row), or n() which is the number or rows and also the
    # number of the last row. The first and last rows hold the min and max
    # values of raw for that value of SS (the grouping variable)
    filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
      # !! (unquote) is required for summarise to evaluate paste0('mo_', .x),
      # beccause dplyr verbs quote their inputs (:= is also required instead of
      # =). Summarise creates a table with one row per group (one row per
      # possible value of SS). For the 1-row groups, str_c simply passes the
      # value of raw as a string; for the multi-row groups, str_c joins the min
      # and max values of raw with the '=' separator.
    summarise(!!paste0('mo_', .x) := str_c(eval(as.name(
      paste0('mo_', .x)
    )), collapse = '-')) %>%
    # recode missing values of raw to '-'
    mutate_at(vars(paste0('mo_', .x)), ~ case_when(is.na(.x) ~ '-', TRUE ~ .x)) %>%
    # sort descending on SS
    arrange(desc(SS)) %>%
    # save an interim file with SS and rawscore, the latter in a column named for agestrat
    assign(paste0('raw_SS_', .x), ., envir = .GlobalEnv)
)


file_names <- paste0('raw_SS_', agestrat)

mylist <- lapply(file_names, get)

norms_pub <- mylist %>% reduce(left_join, by = "SS")

write_csv(norms_pub, here('OUTPUT-FILES/ANT_total-raw-SS-lookup_print_table.csv'))


# Below is a simplified version of a problem that involves transforming multiple
# input tables and joining the transformed output into a single table.

# Each input table is processed and summarized, yielding three output tables
# with identical `x` columns. `x` can thus be used as index variable to combine
# the tables with `left_join`.

# `out_all` is the desired final output table, with index column `x` and summary
# columns 'd', `e`, and `f`.

# This code achives the desired output, but it's not efficient for handling a
# large set of input tables.

# What I hope to achieve, perhaps using `purr::map` functions or a loop
# structure, is to keep joining the new summary columns to the current iteration
# of the ouput table. Rather than pausing the workflow to save out the latest
# output, I want to feed that output back into the beginning of loop so that it
# forms the LHS of the next version of itself, with a new summary column added
# on the RHS.

library(tidyverse)
in1 <- tribble(
~x, ~a, 
1, 1, 
1, 2, 
1, 3, 
2, 4, 
3, 5 
)

in2 <- tribble(
~x, ~b, 
1, 1, 
2, 2, 
2, 3, 
2, 4, 
3, 5
)


in3 <- tribble(
~x, ~c, 
1, 1, 
2, 2, 
3, 3, 
3, 4, 
3, 5
)

out1 <- in1 %>% 
  group_by(x) %>% 
  summarize(d = mean(a))

out2 <- in2 %>% 
  group_by(x) %>% 
  summarize(e = mean(b))

out12 <- left_join(out1, out2, by = 'x')

out3 <- in3 %>% 
  group_by(x) %>% 
  summarize(f = mean(c))

out_all <- left_join(out12, out3, by = 'x')


# df <- data.frame(id=1:5,matrix(runif(n=26*5),ncol=26))
# 
# df1 <- df %>% gather(k,v,-id) %>% group_by(id) %>% 
#   summarise(m=mean(v))

# extract agestrat labels for processing below
ant_norms_names <- names(ant_norms)[-1]

norms_pub <- ant_norms %>% 
  # gather collapses wide table into three-column tall table with key-value
  # pairs: rawscore, agestrat(key var, many rows for each agestrat), SS(value
  # var, one row for each value of SS within each agestrat)
  gather(agestrat, SS,-rawscore) %>% 
  group_by(agestrat) %>%
  # expand the table vertically, adding new rows, so there's a row for every possible SS value
  complete(SS = 40:160) %>% 
  ungroup() %>%
  # regroup table by two levels
  group_by(agestrat, SS) %>%
  # filter step retains all 1-row groups, and the first and last rows of any
  # multi-row groups. n() == 1 returns 1-row groups; n() > 1 & row_number()
  # %in% c(1, n()) returns rows of multi-row groups with the row number of
  # either 1 (first row), or n() which is the number or rows and also the
  # number of the last row. The first and last rows hold the min and max
  # values of raw for that value of SS (the grouping variable)
  filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
  # Summarise creates a table with one row per group (one row per
  # possible value of SS). For the 1-row groups, str_c simply passes the
  # value of raw as a string; for the multi-row groups, str_c joins the min
  # and max values of raw with the '=' separator.
  summarise(rawscore = str_c(rawscore, collapse = '-')) %>%
  # recode missing values of raw to '-'
  mutate_at(vars(rawscore), ~ case_when(is.na(.x) ~ '-', TRUE ~ .x)) %>%
  # sort on two levels
  arrange(agestrat, desc(SS)) %>% 
  # spread table back to wide, all values of SS (one row for each), agestrat
  # columns filled with values of rawscore
  spread(agestrat, rawscore) %>%
  # sort descending on SS
  arrange(desc(SS)) %>% 
  # apply desired final column names
  select(SS, ant_norms_names)


 