#$$$$$$$$$$$NOTE: LIBRARY CODE BELOW NOT PROPIGATED TO MARKDOWN OR CASL-2 SCRIPTS
suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
library(reshape2) # RESHAPE DATA FROM WIDE TO TALL
library(broom) # TIDY MODEL OUTPUTS
suppressMessages(library(moderndive)) # USER-FRIENDLY LINEAR MODELING, REGRESSION AND CORRELATION TOOLS.
# note use of `suppressWarnings` to silence chatter during interactive session
library(magrittr) # PIPE OPERATORS
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS

input_parameters_prompt <- function() {
  writeLines(
    "To initiate the norming process, R requires certain input parameters. Please choose a method for entering these parameters:\n\n1: Interactively at the console\n2: From a file named 'Input-Parameters.R' located in '[PROJECT DIRECTORY]/INPUT-FILES'\n"
  )
  input_choice <- as.numeric(0)
  while (is.na(input_choice) || (!(input_choice %in% 1:2))) {
    input_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter choice: ")))
    if (is.na(input_choice)) {
      writeLines("Please enter 1 or 2\n")
    } else {
      if (input_choice == 1) {
        cat("Enter the name of the input file, including the .csv suffix.\nUse exact spelling and capitalization.")
          suppressWarnings(as.character(
            readline(prompt = "Input file: ")
          )) %>% assign('input_file_name', ., envir = .GlobalEnv)
        cat("\nEnter the column name of the score to be normed.\nUse exact spelling and capitalization.")
          suppressWarnings(as.character(
            readline(prompt = "Score name: ")
          )) %>% assign('score_name', ., envir = .GlobalEnv)
        cat("\nEnter the HIGHEST possible raw score for the score to be normed.")
          suppressWarnings(as.integer(
            readline(prompt = "Maximum raw score: ")
          )) %>% assign('max_raw', ., envir = .GlobalEnv)
        cat("\nEnter the LOWEST possible raw score for the score to be normed.")
          suppressWarnings(as.integer(
            readline(prompt = "Minimum raw score: ")
          )) %>% assign('min_raw', ., envir = .GlobalEnv)
        break
      } else if (input_choice == 2) {
        source(here('INPUT-FILES/Input-Parameters.R'))
        break
      } else {
        writeLines("Please enter 1 or 2\n")
      }
    }
  }
}
input_parameters_prompt()




