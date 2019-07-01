# Generate norms for test scores with age-related developmental curve.

#$$$$$$$$$$$NOTE: LIBRARY CODE BELOW NOT PROPIGATED TO MARKDOWN OR CASL-2 SCRIPTS
suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
library(reshape2) # RESHAPE DATA FROM WIDE TO TALL
library(broom) # TIDY MODEL OUTPUTS
library(moderndive) # USER-FRIENDLY LINEAR MODELING, REGRESSION AND CORRELATION TOOLS.
# note use of `suppressWarnings` to silence chatter during interactive session
library(magrittr) # PIPE OPERATORS
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS
#$$$$$$$$$$$

ILraw_by_agestrat <-
  suppressMessages(
    read_csv(
      here(
        'INPUT FILES/ILraw_STAND.csv'
      )
    )
  ) %>% 
  mutate_at(
    vars(
      agestrat
    ), ~ case_when(
      .x == 9.0 ~ 108,
      .x == 9.6 ~ 114,
      .x == 10.0 ~ 120,
      .x == 10.6 ~ 126,
      .x == 11.0 ~ 132,
      .x == 11.6 ~ 138,
      .x == 12.0 ~ 144,
      .x == 12.6 ~ 150,
      .x == 13.0 ~ 156,
      .x == 14.0 ~ 168,
      .x == 15.0 ~ 180,
      .x == 1618.0 ~ 192,
      .x == 1921.0 ~ 228,
      TRUE ~ NA_real_
    )
  ) %>% 
  group_by(agestrat)

IL_tot_freq_agestrat <- ILraw_by_agestrat %>% count(IL_total) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(IL_total), lag_cum_per = lag(cum_per))

IL_tot_desc_agestrat <-
  ILraw_by_agestrat %>% arrange(agestrat) %>% summarise(n = n(),
                                                         median = round(median(IL_total), 2),
                                                         mean = round(mean(IL_total), 2),
                                                         sd = round(sd(IL_total), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:13))

agestrat <- IL_tot_desc_agestrat %>% pull(agestrat)

# Create Z score table
perc_z <- tribble(
  ~lohi_value,	~z_score,
  5, 1.6449,
  10,	1.2816,
  15,	1.0364,
  20,	0.8416,
  25,	0.6745,
  75,	0.6745,
  80,	0.8416,
  85,	1.0364,
  90,	1.2816,
  95,	1.6449
)

# Generate table of lo1, lo2, hi1, hi2 SD adjustment points by agestrat
IL_tot_age_lo1lo2_hi1hi2 <-
  full_join(
    IL_tot_freq_agestrat,
    (
      IL_tot_freq_agestrat %>%
        group_by(agestrat) %>%
        summarise(min = min(cum_per)) %>%
        mutate(lo1 = case_when(
          min < 5 ~ 5,
          min < 10 ~ 10,
          min < 15 ~ 15,
          min < 20 ~ 20,
          TRUE ~ 25
        ))
    ),
    by = 'agestrat'
  ) %>%
  group_by(agestrat) %>% mutate(flag = case_when(cum_per > lo1 &
                                                   cum_per < 95 ~ 1,
                                                 TRUE ~ 0)) %>% filter(flag == 1) %>% summarise(min = min(cum_per),
                                                                                                max = max(cum_per),
                                                                                                lo1 = first(lo1)) %>%
  mutate(
    lo2 = case_when(
      lo1 == 5 & min < 10 ~ 10,
      lo1 == 5 & min < 15 ~ 15,
      lo1 == 5 & min < 20 ~ 20,
      lo1 == 5 & min >= 20 ~ 25,
      lo1 == 10 & min < 15 ~ 15,
      lo1 == 10 & min < 20 ~ 20,
      lo1 == 10 & min >= 20 ~ 25,
      lo1 == 15 & min < 20 ~ 20,
      lo1 == 15 & min >= 20 ~ 25,
      TRUE ~ 25
    ),
    hi1 = case_when(max > 90 ~ 90,
                    max > 85 ~ 85,
                    max > 80 ~ 80,
                    TRUE ~ 75),
    hi2 = 95
  ) %>% select(-min,-max)

# Join hi-lo norm perc values to freq table, add imputed raw scores (IRS) for
# the five distribution points (lo1, lo2, med, hi1, hi2), and flag the rows that
# contain the correct IRS values for each distribution point (note how ties are
# handled <= cum_per). Drop all rows containing missing values, such that
# remaining rows are only those that contain the five IRS for each agestrat.
# Then collapse IRS values into a single row, with the correct IRS paired with
# its dist_point label. Use left_join to look up Z-scores for distribution
# points. Drop columns no longer needed.

norm_build1 <-
  IL_tot_freq_agestrat %>% left_join(IL_tot_age_lo1lo2_hi1hi2, by = 'agestrat') %>% mutate (
    IRS_lo1 = ((lo1 - lag_cum_per) / perc) * (IL_total - lag_tot) + lag_tot,
    IRS_lo2 = ((lo2 - lag_cum_per) /
                 perc) * (IL_total - lag_tot) + lag_tot,
    IRS_med = ((50 - lag_cum_per) /
                 perc) * (IL_total - lag_tot) + lag_tot,
    IRS_hi1 = ((hi1 - lag_cum_per) /
                 perc) * (IL_total - lag_tot) + lag_tot,
    IRS_hi2 = ((hi2 - lag_cum_per) /
                 perc) * (IL_total - lag_tot) + lag_tot,
    dist_point = case_when(
      lo1 <= cum_per & lo1 > lag_cum_per ~ 'lo1',
      lo2 <= cum_per &
        lo2 > lag_cum_per ~ 'lo2',
      50 <= cum_per &
        50 > lag_cum_per ~ 'med',
      hi1 <= cum_per &
        hi1 > lag_cum_per ~ 'hi1',
      hi2 <= cum_per &
        hi2 > lag_cum_per ~ 'hi2',
      TRUE ~ NA_character_
    )
  ) %>% drop_na() %>% mutate(IRS = case_when(dist_point == 'lo1' ~ IRS_lo1,
                                                             dist_point == 'lo2' ~ IRS_lo2,
                                                             dist_point == 'med' ~ IRS_med,
                                                             dist_point == 'hi1' ~ IRS_hi1,
                                                             dist_point == 'hi2' ~ IRS_hi2,
                                                             TRUE ~ NA_real_),
                                             RSD = case_when(dist_point == 'lo1' ~ lead(lead(IRS))-IRS,
                                                             dist_point == 'lo2' ~ lead(IRS)-IRS,
                                                             dist_point == 'med' ~ IRS,
                                                             dist_point == 'hi1' ~ IRS-lag(IRS),
                                                             dist_point == 'hi2' ~ IRS-lag(lag(IRS)),
                                                             TRUE ~ NA_real_),
                                             lohi_value = case_when(dist_point == 'lo1' ~ lo1,
                                                             dist_point == 'lo2' ~ lo2,
                                                             dist_point == 'hi1' ~ hi1,
                                                             dist_point == 'hi2' ~ hi2,
                                                             TRUE ~ 50)) %>% 
  select(agestrat, lo1, lo2, hi1, hi2, dist_point, lohi_value, IRS, RSD) %>% left_join(perc_z, by = 'lohi_value') %>% 
  mutate(std_RSD = RSD/z_score, SD = case_when(dist_point == 'lo1' | dist_point == 'hi1' ~ (std_RSD+lead(std_RSD))/2,
                                                                                            TRUE ~ NA_real_),
         median = case_when(dist_point == 'med' ~ IRS,
                            TRUE ~ NA_real_),
         lo_SD = case_when(dist_point == 'lo1' ~ SD,
                           TRUE ~ NA_real_),
         hi_SD = case_when(dist_point == 'hi1' ~ SD,
                           TRUE ~ NA_real_))
    
# Get summary table
norm_build_med_hilo_sum <-
  norm_build1 %>% summarise(
    lo1 = first(lo1),
    lo2 = first(lo2),
    hi1 = first(hi1),
    hi2 = first(hi2),
    median = first(na.omit(median)),
    lo_SD = first(na.omit(lo_SD)),
    hi_SD = first(na.omit(hi_SD))
  ) %>% mutate(ES = (median - lag(median))/((hi_SD+lag(hi_SD)+lo_SD+lag(lo_SD))/4), group = 1:13) %>% select(group, everything())

# clean up environment
rm(IL_tot_age_lo1lo2_hi1hi2, ILraw_by_agestrat, norm_build1)

# Specify functions to obtain smoothed medians for 1st-, 2nd-, and 3rd-order polynomial smoothing formulas

model_median_2nd <- function() {
  lm(median ~ group + I(group ^ 2), data = norm_build_med_hilo_sum) %>% assign('model_median', ., envir = .GlobalEnv)
  get_regression_points(model_median) %>% pull(median_hat) %>% assign('median_sm', ., envir = .GlobalEnv)
}

model_median_2nd()


# Specify functions to obtain smoothed lo_SDs for 1st-order polynomial and mean substitution smoothing formulas

model_lo_SD_1st <- function() {
  lm(lo_SD ~ group, data = norm_build_med_hilo_sum) %>% assign('model_lo_SD', ., envir = .GlobalEnv)
  get_regression_points(model_lo_SD) %>% pull(lo_SD_hat) %>% assign('lo_SD_sm', ., envir = .GlobalEnv)
}

model_lo_SD_1st()


# Specify functions to obtain smoothed hi_SDs for 1st-order polynomial and mean substitution smoothing formulas

model_hi_SD_1st <- function() {
  lm(hi_SD ~ group, data = norm_build_med_hilo_sum) %>% assign('model_hi_SD', ., envir = .GlobalEnv)
  get_regression_points(model_hi_SD) %>% pull(hi_SD_hat) %>% assign('hi_SD_sm', ., envir = .GlobalEnv)
}

model_hi_SD_1st()

# Create table needed for next phase of norms process
smooth_med_SD <-
  cbind(norm_build_med_hilo_sum, median_sm, lo_SD_sm, hi_SD_sm) %>% dplyr::select(-(lo1:hi2)) %>%
  mutate(ES_sm = round((median_sm - lag(median_sm)) / ((
    lo_SD_sm + lag(lo_SD_sm) + hi_SD_sm + lag(hi_SD_sm)
  ) / 4), 3))


# Clean up environment
rm(list = ls()[!ls() %in% c("smooth_med_SD", "model_median", "model_lo_SD", "model_hi_SD")])


# Next code section can deal with situation where smoothed medians have effect
# sizes between adjacent age groups that are too large (ES > .33) for
# well-functioning norms. 

prep_impute <- function() {
  # flag rows with too-big effect sizes
  invisible(
    smooth_med_SD %>%
      mutate(flag = case_when(ES_sm > .33 ~ "flag",
                              TRUE ~ NA_character_)) %>%
      # insert alternating blank lines
      map_dfr(rbind,
              NA) %>%
      # remove blank lines except those preceeding flagged rows.
      filter(!(is.na(lead(flag)) & is.na(group))) %>%
      # insert ".5" group numbers in blank lines, dummy values.
      mutate_at(vars(group),
                ~ case_when(is.na(.x) ~ lag(.x) + .5,
                            TRUE ~ as.double(.x))) %>% 
      assign('smooth_med_SD', ., envir = .GlobalEnv)
  )
}

preds_impute <- function() {
  # adding `newdata` arg to `get_regression_points` allows you to use previously
  # defined regression equation to get predicted values with new data.
  get_regression_points(model_median, newdata = smooth_med_SD) %>% pull(median_hat)  %>% assign('median_sm_imp', ., envir = .GlobalEnv)
  
  if (is_tibble(model_lo_SD)) {
    rep(as.numeric(model_lo_SD), length(median_sm_imp)) %>% assign('lo_SD_sm_imp', ., envir = .GlobalEnv)
  } else {
    get_regression_points(model_lo_SD, newdata = smooth_med_SD) %>% pull(lo_SD_hat) %>% assign('lo_SD_sm_imp', ., envir = .GlobalEnv)
  }
  
  if (is_tibble(model_hi_SD)) {
    rep(as.numeric(model_hi_SD), length(median_sm_imp)) %>% assign('hi_SD_sm_imp', ., envir = .GlobalEnv)
  } else {
    get_regression_points(model_hi_SD, newdata = smooth_med_SD) %>% pull(hi_SD_hat) %>% assign('hi_SD_sm_imp', ., envir = .GlobalEnv)
  }
}

table_impute <- function() {
  invisible(
    add_column(smooth_med_SD, median_sm_imp, lo_SD_sm_imp, hi_SD_sm_imp) %>%
      select(group, agestrat, median_sm_imp, lo_SD_sm_imp, hi_SD_sm_imp) %>%
      rename(median_sm = median_sm_imp,
             lo_SD_sm = lo_SD_sm_imp,
             hi_SD_sm = hi_SD_sm_imp) %>%
      mutate(ES_sm = round((median_sm - lag(median_sm)) / ((
        lo_SD_sm + lag(lo_SD_sm) + hi_SD_sm + lag(hi_SD_sm)
      ) / 4), 3)) %>%
      mutate_at(
        vars(
          agestrat
        ), ~ case_when(
          is.na(.x) ~ (lag(.x) + lead(.x))/2,
          TRUE ~ as.double(.x)
        )
      ) %>% 
      assign('smooth_med_SD', ., envir = .GlobalEnv)
  )
}

prep_impute()
preds_impute()
table_impute()

# prep table of smoothed medians, SDs for treatment of any score reversals at distribution extremes.
smooth_med_SD_fun <- function() {
  smooth_med_SD %>%
    mutate(
      med_minus_2SD = median_sm - (2 * lo_SD_sm),
      med_plus_2SD = median_sm + (2 * hi_SD_sm),
      diff_minus_2SD = med_minus_2SD - lag(med_minus_2SD),
      diff_plus_2SD = med_plus_2SD - lag(med_plus_2SD),
      med = median_sm
    ) %>%
    select(
      group,
      agestrat,
      diff_minus_2SD,
      med_minus_2SD,
      lo_SD_sm,
      median_sm,
      hi_SD_sm,
      med_plus_2SD,
      diff_plus_2SD
    ) %>%
    print(n = nrow(.), na.print = NULL) %>% 
    assign('smooth_med_SD', ., envir = .GlobalEnv)
}
smooth_med_SD_fun()

# process lo_SD score reversals
lo_SD_hand_smooth_prompt <- function() {
  writeLines("\nChoose row in which you want to modify lo_SD_sm.")
  lo_SD_hand_smooth_row_choice <- as.numeric(0)
  while (is.na(lo_SD_hand_smooth_row_choice) ||
         (!(lo_SD_hand_smooth_row_choice %in% 1:nrow(smooth_med_SD)))) {
    lo_SD_hand_smooth_row_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter row number: ")))
    if (lo_SD_hand_smooth_row_choice %in% 1:nrow(smooth_med_SD))
    {
      lo_SD_new_value <- as.numeric(0)
      while (is.na(lo_SD_new_value) ||
             (!(lo_SD_new_value > 0 && lo_SD_new_value < 100))) {
        lo_SD_new_value <-
          suppressWarnings(as.numeric(
            readline(prompt = "Enter new value (between 0 and 100) for lo_SD_sm: ")
          ))
        smooth_med_SD %>%
          mutate_at(
            vars(lo_SD_sm),
            ~ case_when(
              group == lo_SD_hand_smooth_row_choice ~ lo_SD_new_value,
              TRUE ~ .x
            )
          ) %>%
          assign('smooth_med_SD', ., envir = .GlobalEnv)
      }
      smooth_med_SD_fun()
      writeLines(
        "\nExamine table above to see effect of adjusted lo_SD_sm on value of diff_minus_2SD in same row.
        \nDo you want to modify lo_SD_sm in another row?"
      )
      modify_again <- as.character('x')
      while (is.na(modify_again) ||
             (!(
               modify_again %in% c('Y', 'y', 'Yes', 'yes', 'N', 'n', 'No', 'no')
             ))) {
        modify_again <-
          suppressWarnings(as.character(readline(prompt = "Enter Y or N: ")))
      }
      if (modify_again %in% c('N', 'n', 'No', 'no')) {
        if (any(na.omit(smooth_med_SD$diff_plus_2SD) < 0)) {
          writeLines(
            '\nR found score reversals, between age groups, at 2 SD above imputed median (see negative values for diff_plus_2SD in table printed above).
            \nPress [Enter] to correct reversals by manually adjusting hi_SD_sms.'
          )
          readline()
          hi_SD_hand_smooth_prompt()
        } else {
          writeLines('\nPress [Enter] to generate standard-score look-up tables.')
          invisible(readline())
          break
        }
      } else if (modify_again %in% c('Y', 'y', 'Yes', 'yes')) {
        lo_SD_hand_smooth_row_choice <- NA
      }
    }
  }
}

# process hi_SD score reversals
hi_SD_hand_smooth_prompt <- function() {
  writeLines("\nChoose row in which you want to modify hi_SD_sm.")
  hi_SD_hand_smooth_row_choice <- as.numeric(0)
  while (is.na(hi_SD_hand_smooth_row_choice) ||
         (!(hi_SD_hand_smooth_row_choice %in% 1:nrow(smooth_med_SD)))) {
    hi_SD_hand_smooth_row_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter row number: ")))
    if (hi_SD_hand_smooth_row_choice %in% 1:nrow(smooth_med_SD))
    {
      hi_SD_new_value <- as.numeric(0)
      while (is.na(hi_SD_new_value) ||
             (!(hi_SD_new_value > 0 &&
                hi_SD_new_value < 100))) {
        hi_SD_new_value <-
          suppressWarnings(as.numeric(
            readline(prompt = "Enter new value (between 0 and 100) for hi_SD_sm: ")
          ))
        smooth_med_SD %>%
          mutate_at(
            vars(hi_SD_sm),
            ~ case_when(
              group == hi_SD_hand_smooth_row_choice ~ hi_SD_new_value,
              TRUE ~ .x
            )
          ) %>%
          assign('smooth_med_SD', ., envir = .GlobalEnv)
      }
      smooth_med_SD_fun()
      writeLines(
        "\nExamine table above to see effect of adjusted hi_SD_sm on value of diff_plus_2SD in same row.
        \nDo you want to modify hi_SD_sm in another row?"
      )
      modify_again <- as.character('x')
      while (is.na(modify_again) ||
             (!(
               modify_again %in% c('Y', 'y', 'Yes', 'yes', 'N', 'n', 'No', 'no')
             ))) {
        modify_again <-
          suppressWarnings(as.character(readline(prompt = "Enter Y or N: ")))
      }
      if (modify_again %in% c('N', 'n', 'No', 'no')) {
        writeLines(
          '\nPress [Enter] to generate standard-score look-up tables.'
        )
        invisible(readline())
        break
      } else if (modify_again %in% c('Y', 'y', 'Yes', 'yes')) {
        hi_SD_hand_smooth_row_choice <- NA
      }
    }
  }
}

# route input tables into code streams depending presense of lo and/or hi score reversals.
if (!any(na.omit(smooth_med_SD$diff_minus_2SD) < 0) &&
    !any(na.omit(smooth_med_SD$diff_plus_2SD) < 0)) {
  writeLines(
    '\nR found no score reversals, going from one age group to the next, at either 2 SD above or 2 SD below the imputed median.
    \nPress [Enter] to generate standard-score look-up tables.'
  )
  invisible(readline())
} else if (any(na.omit(smooth_med_SD$diff_minus_2SD) < 0)) {
  writeLines(
    '\nR found score reversals, between age groups, at 2 SD below imputed median (see negative values for diff_minus_2SD in table printed above).
    \nPress [Enter] to correct reversals by manually adjusting lo_SD_sm.'
  )
  readline()
  lo_SD_hand_smooth_prompt()
} else if (any(na.omit(smooth_med_SD$diff_plus_2SD) < 0)) {
  writeLines(
    '\nR found score reversals, between age groups, at 2 SD above imputed median (see negative values for diff_plus_2SD in table printed above).
    \nPress [Enter] to correct reversals by manually adjusting hi_SD_sm.'
  )
  readline()
  hi_SD_hand_smooth_prompt()
}

# Clean up environment
rm(list = ls()[!ls() %in% c("smooth_med_SD")])

# next code section generates raw-to-SS look-up tables.

# create table with only vars needed to calc standard scores
final_med_SD <- smooth_med_SD %>% select(agestrat, median_sm, lo_SD_sm, hi_SD_sm) %>% mutate_at(vars(agestrat), ~ paste0("mo_", .x))
# define column names for lookup stable by creating charvec of agestrat labels
# agelabels <- final_med_SD %>% pull(agestrat) %>% paste0("mo_", .)
# create empty look up table by binding column of all possible raw scores to set of columns holding numerical NAs, naming each column in set using agelabels charvec
raw_to_SS_lookup_empty <- bind_cols(
  enframe(0:54, name = NULL, value = 'rawscore'), 
  data.frame(matrix(NA_real_, nrow = 55, ncol = 13)) %>% 
    set_colnames(final_med_SD$agestrat)
)

# fill empty raw-to-SS lookup with standard scores by age strat.
raw_to_SS_lookup <- raw_to_SS_lookup_empty %>%
  # gather collapses the empty wide rawscore by agestrat table into tall table
  # with three columns, rawscore, agestrat, and an empty SS column. Rawscore
  # sequence is repeated down its column, once per agestrat, and value of
  # agestrat column is uniform for the entire rawscore sequence. `-rawscore` is
  # `dplyr::select` code that drops all vars except rawscore.
  gather(agestrat, SS, -rawscore) %>% 
  # This tall table can now be joined with `final_med_SD`, which contains
  # smoothed meds and SDs per agestrat, because both tables have an `agestrat`
  # column that can be used as a `by` var. In the newly-constituted tall table,
  # `median_sm`, `lo_SD_sm`, and `hi_SD_sm` cols hold the correct values for
  # each agestrat.
  left_join(final_med_SD, by = 'agestrat') %>% 
  # calculate SS above and below the median. `meidan_SM = NULL` drops this now
  # unnecessary column from the piped object.
  mutate(SS = case_when(
    rawscore <= median_sm ~ round(100 + (((rawscore - median_sm) / lo_SD_sm) *15)),
    TRUE ~ round(100 + (((rawscore - median_sm) / hi_SD_sm) *15))
  ), median_sm = NULL, lo_SD_sm = NULL, hi_SD_sm = NULL) %>%
  # truncate SS distribution at 40 and 160.
  mutate_at(
    vars(SS), ~ case_when(
      .x < 40 ~ 40,
      .x > 160 ~ 160,
      TRUE ~ .x
    )
  ) %>% 
  # spread converts table back from tall to wide. Resulting table has one row
  # per `rawscore`. Each value of `agestrat` gets its own column, and each of
  # these columns is populated with the value of `SS` that matches `rawscore`,
  # within that `agestrat`.
  spread(agestrat, SS) %>% 
  # select reorders vars so to give the correct sequence of `agestrat` going
  # left-to-right. That order of agestrats is given by the char vec
  # `c(final_med_SD$agestrat)`.
  select(rawscore, c(final_med_SD$agestrat))
rm(raw_to_SS_lookup_empty, final_med_SD, smooth_med_SD)

# next code section combines R raw-to-SS lookup with SPSS raw-to-SS lookup, in a
# format that can be used in Excel to compare results of the two methods.

# names for calculated columns
calc_cols <- c(paste0("calc_", 23:35))

# df containing empty calc columns that can be joined to other tables.
calc_cols_df <- bind_cols(
  enframe(0:54, name = NULL, value = 'rawscore'), 
  data.frame(matrix(NA_real_, nrow = 55, ncol = 13)) %>% 
    set_colnames(calc_cols)
)

# create lisl containing three tables to join: R raw-SS-lookup, SPSS
# raw-SS-lookup, and empty columns for calculation.
tables_to_join <- list(raw_to_SS_lookup,
                       suppressMessages(read_csv(
                         here(
                           'CASL-2 COMPS/RAW-TO-SS-LOOKUP-TABLES-SPSS/SPSS-LOOKUPS/IL-Age-Norms-Raw-to-SS-from-SPSS.csv'
                         )
                       )),
                       calc_cols_df
)

# `purrr::reduce` can be used to apply `left_join` to a list of tables that share a `by` variable
R_SPSS_comps_table <- tables_to_join %>% 
  reduce(
    left_join, by = 'rawscore'
  ) %>% 
  select(
    rawscore, mo_108, a0900, 
    calc_23, mo_114, a0960, calc_24, mo_120, a1000, calc_25, mo_126, a1060, calc_26, mo_132, 
    a1100, calc_27, mo_138, a1160, calc_28, mo_144, a1200, calc_29, mo_150, a1260, calc_30, 
    mo_156, a1300, calc_31, mo_168, a1400, calc_32, mo_180, a1500, calc_33, mo_192, a1618, 
    calc_34, mo_228, a1921, calc_35
  )

# gets an integer vector containing position numbers of columns needing calculation
calc_cols_pos <- which(startsWith(names(R_SPSS_comps_table), "calc"))

# do the calculation on cols via simple assignment. Within single brackets,
# position numbers can be added and subtracted to refer to leading and lagging columns.
R_SPSS_comps_table[calc_cols_pos] <- 
  R_SPSS_comps_table[calc_cols_pos - 1] - R_SPSS_comps_table[calc_cols_pos - 2]


# write table for doing SS comps in excel
write_csv(R_SPSS_comps_table, 
          here('CASL-2 COMPS/RAW-TO-SS-LOOKUP-TABLES-SPSS/COMPS/IL-R-SPSS-SS-comps.csv'
          )
)

