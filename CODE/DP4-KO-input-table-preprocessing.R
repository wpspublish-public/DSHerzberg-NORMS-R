suppressMessages(library(here))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))

# read KO's input data
KO_final_DP4_norms_data <- suppressMessages(
  read_csv(
    here(
      'INPUT-FILES/KO-final-DP4-norms-data.csv'
    )
  )
)

# extract score names
cols <- names(KO_final_DP4_norms_data)[1:24]

# save one output .csv per score, each with ID, agestrat and [rawscore] columns
map(
  cols,
  ~ KO_final_DP4_norms_data %>% 
    select(ID, agestrat, .x) %>% 
    arrange(agestrat) %>% 
    write_csv(
      here(
        paste0('INPUT-FILES/', .x, '-DP4-norms-data.csv')
      )
    )
)

# save list of col names for reference
write.csv(cols,
  file = here(
    'INPUT-FILES/score-names.csv'
  )
)
