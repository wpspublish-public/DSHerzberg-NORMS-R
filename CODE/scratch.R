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

# enable hand smoothing of medians
med_hand_smooth_prompt <- function() {
  writeLines("\nDo you want to adjust median_sm (smoothed median)?")
  repeat {
    adjust_med <-
      suppressWarnings(as.character(readline(prompt = "Enter Y or N: ")))
    if (adjust_med %in% c('Y', 'y', 'Yes', 'yes')) {
      writeLines("\nChoose row in which you want to adjust median_sm.")
      repeat {
        med_hand_smooth_row_choice <-
          suppressWarnings(as.numeric(readline(prompt = "Enter row number: ")))
        if (med_hand_smooth_row_choice %in% 1:nrow(smooth_med_SD))
        {
          med_new_value <- as.numeric(0)
          while (is.na(med_new_value) ||
                 (!(med_new_value > 0 &&
                    med_new_value < 500))) {
            med_new_value <-
              suppressWarnings(as.numeric(
                readline(prompt = "Enter new value (between 0 and 500) for median_sm: ")
              ))
            smooth_med_SD %>%
              mutate_at(
                vars(median_sm),
                ~ case_when(
                  group == med_hand_smooth_row_choice ~ med_new_value,
                  TRUE ~ .x
                )
              ) %>%
              assign('smooth_med_SD', ., envir = .GlobalEnv)
          }
          smooth_med_SD_fun()
          writeLines(
            "\nExamine table above to see effect of adjusted median_sm on values of diff_minus_2SD, diff_plus_2SD in same row.
            \nDo you want to adjust median_sm in another row?"
          )
          repeat {
            adjust_again <-
              suppressWarnings(as.character(readline(prompt = "Enter Y or N: ")))
            if (adjust_again %in% c('Y', 'y', 'Yes', 'yes')) {
              break
              # break exits one level of nested loops
            }
            else if (adjust_again %in% c('N', 'n', 'No', 'no')) {
              # return() exits all nested loops
              return()
            }
          }
        }
      }
    } else if (adjust_med %in% c('N', 'n', 'No', 'no')) {
      break
    }
  }
}


# create function to choose to adjust medians, SDs by hand.
hand_smooth_choice_fun <- function() {
  repeat {
    hand_smooth_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter choice: ")))
    if (hand_smooth_choice == 1) {
      break
    } else if (hand_smooth_choice == 2) {
      lo_SD_hand_smooth_prompt()
      hi_SD_hand_smooth_prompt()
      writeLines("\nDo you want to repeat the process of adjusting medians, SDs by hand?")
      repeat {
        repeat_adjust <-
          suppressWarnings(as.character(readline(prompt = "Enter Y or N: ")))
        if (repeat_adjust %in% c('Y', 'y', 'Yes', 'yes')) {
          break
        } else if (repeat_adjust %in% c('N', 'n', 'No', 'no')) {
          return()
        }
      }
    }
  }
}


# prompt whether user wants to inspect histograms by agestrat
hist_prompt <- function() {
  writeLines("\nDo you want to examine histograms showing the distribution of the score to be normed, by agestrat?")
  repeat {
    show_hist <-
      suppressWarnings(as.character(readline(prompt = "Enter Y or N: ")))
    if (show_hist %in% c('Y', 'y', 'Yes', 'yes')) {
      hist_plot <- ggplot(data = eval(as.name(paste0(score_name, '_raw_by_agestrat'))), aes(eval(as.name(score_name)))) +
        geom_histogram(
          binwidth = .2,
          col = "red"
        ) +
        scale_y_continuous(breaks = seq(0, 20, 1)) +
        labs(title = "Frequency Distribution", x = "Each bin is a count of a specific Total Score value", y = "Each histogram is an agestrat") +
        stat_function(
          fun = function(x, mean, sd, n){
            n * dnorm(x = x, mean = mean, sd = sd)
          },
          args = with(eval(as.name(paste0(score_name, '_raw_by_agestrat'))), c(mean = mean(eval(as.name(score_name))), sd = sd(eval(as.name(score_name))), n
                                                                               = length(eval(as.name(score_name)))))
        ) +
        theme(panel.grid.minor=element_blank()) +
        facet_wrap(~agestrat)
      print(hist_plot)
      writeLines(c("\n", 
                   "Examine histograms.",
                   "Then press [enter] to continue."
      ))
      readline()
      break
    } else if (show_hist %in% c('N', 'n', 'No', 'no')) {
      break
    }
  }
}
hist_prompt()

