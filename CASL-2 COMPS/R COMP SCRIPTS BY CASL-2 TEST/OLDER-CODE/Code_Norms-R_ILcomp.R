# Generate norms for test scores with age-related developmental curve.

suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
library(broom) # TIDY MODEL OUTPUTS
library(moderndive) # USER-FRIENDLY LINEAR MODELING, REGRESSION AND CORRELATION TOOLS.
library(magrittr) # PIPE OPERATORS
# note use of `suppressWarnings` to silence chatter during interactive session
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS

ILraw_by_agestrat <-
  suppressMessages(read_csv(here('INPUT FILES/ILraw_STAND.csv')
  )) %>% group_by(agestrat)

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

# Plot raw score means, SDs; pause execution for user to examine plot.

mean_plot <- ggplot(data = IL_tot_desc_agestrat, aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  scale_x_continuous(breaks = seq(1, 13, 1), labels = agestrat) +
  scale_y_continuous(breaks = seq(0, 55, 5), limits = c(0, 55)) +
  labs(title = "Raw Score Means (with SDs)", x = "Agestrat", y = "Mean Total Score") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "blue",
    size = 0.2,
    width = 0.2
  )
print(mean_plot)

mean_plot_prompt <- function() {
  writeLines(c(
    strrep("\u2500", 40),
    "Examine plot of raw score means and SDs.",
    "Then press [enter] to continue.",
    strrep("\u2500", 40)
  ))
  readline()
}
mean_plot_prompt ()

# # generate histograms by agestrat
# ggplot(data = ILraw_by_agestrat, aes(IL_total)) +
#   geom_histogram(
#     binwidth = .2,
#     col = "red"
#   ) +
#   scale_y_continuous(breaks = seq(0, 20, 1)) +
#   labs(title = "Frequency Distribution") +
#   # stat_function(
#   #   fun = function(x, mean, sd, n){
#   #     n * dnorm(x = x, mean = mean, sd = sd)
#   #   },
#   #   args = with(ILraw_by_agestrat, c(mean = mean(IL_total), sd = sd(IL_total), n
#   #                     = length(IL_total)))
#   # ) +
#   theme(panel.grid.minor=element_blank()) +
#   facet_wrap(~agestrat)
# 
# # plot means, SDs by agestrat
# ggplot(data = IL_tot_desc_agestrat, aes(group, mean)) +
#   geom_point(
#     col = "blue",
#     fill = "blue",
#     alpha = .5,
#     size = 3,
#     shape = 23
#   ) +
#   scale_x_continuous(breaks = seq(1, 13, 1), labels = agestrat) +
#   scale_y_continuous(breaks = seq(0, 50, 5), limits = c(0, 50)) +
#   labs(title = "Means", x = "Agestrat", y = "Mean Total Score")
# 
# ggplot(data = IL_tot_desc_agestrat, aes(group, sd)) +
#   geom_point(
#     col = "red",
#     fill = "red",
#     alpha = .5,
#     size = 3,
#     shape = 23
#   ) +
#   scale_x_continuous(breaks = seq(1, 13, 1), labels = agestrat) +
#   scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
#   labs(title = "SDs", x = "Agestrat", y = "SD Total Score")

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

# Plot median, lo_SD, hi_SD with regression lines and fit statistics.

# median.

frm <- c('y ~ x', 'y ~ x + I(x^2)', 'y ~ x + I(x^2) + I(x^3)')
title <-
  c('1st order polynomial',
    '2nd order polynomial',
    '3rd order polynomial')
lo_hi <- c('lo_SD', 'hi_SD')

med_plots <- map2(
  title,
  frm,
  ~
    ggplot(norm_build_med_hilo_sum, aes(x = group, y = median)) +
    geom_point(
      col = 'red',
      fill = 'red',
      alpha = .5,
      size = 3,
      shape = 23
    ) +
    labs(
      x = 'age group'
      ,
      y = 'median'
      ,
      title = paste0('Median: ', .x)
    ) +
    scale_x_continuous(breaks = seq(1, 13, 1), labels = agestrat) +
    scale_y_continuous(breaks = seq(0, 50, 5)) +
    geom_smooth(
      method = 'lm',
      se = FALSE,
      formula = .y
    ) +
    stat_poly_eq(
      formula = .y,
      eq.with.lhs = 'italic(hat(y))~`=`~',
      aes(label = paste(..eq.label.., ..rr.label.., sep = '*plain(\',\')~')),
      parse = TRUE
    )
)
print(med_plots)

# Specify functions to obtain smoothed medians for 1st-, 2nd-, and 3rd-order polynomial smoothing formulas

model_median_1st <- function() {
  lm(median ~ group, data = norm_build_med_hilo_sum) %>% assign('model_median', ., envir = .GlobalEnv)
  get_regression_points(model_median) %>% pull(median_hat) %>% assign('median_sm', ., envir = .GlobalEnv)
}

model_median_2nd <- function() {
  lm(median ~ group + I(group ^ 2), data = norm_build_med_hilo_sum) %>% assign('model_median', ., envir = .GlobalEnv)
  get_regression_points(model_median) %>% pull(median_hat) %>% assign('median_sm', ., envir = .GlobalEnv)
}

model_median_3rd <- function() {
  lm(median ~ group + I(group ^ 2) + I(group ^ 3), data = norm_build_med_hilo_sum) %>% assign('model_median', ., envir = .GlobalEnv)
  get_regression_points(model_median) %>% pull(median_hat) %>% assign('median_sm', ., envir = .GlobalEnv)
}

# Run interactive session allowing user to select median smoothing model at console

model_prompt_median <- function() {
  writeLines(
    "Examine plots and choose smoothing model for medians:\n\n1: First-order polynomial: y = ax + b\n2: Second-order polynomial: y= ax^2 + bx + c\n3: Third-order polynomial: y= ax^3 + bx^2 + cx + d\n"
  )
  model_choice <- as.numeric(0)
  while (is.na(model_choice) || (!(model_choice %in% 1:3))) {
    model_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter choice: ")))
    if (is.na(model_choice)) {
      writeLines("Please enter 1, 2, or 3\n")
    } else {
      if (model_choice == 1) {
        model_median_1st()
        break
      } else if (model_choice == 2) {
        model_median_2nd()
        break
      } else if (model_choice == 3) {
        model_median_3rd()
        break
      } else {
        writeLines("Please enter 1, 2, or 3\n")
      }
    }
  }
}
model_prompt_median()


# lo_SD 1st-order polynomial and mean substitution plots

lo_SD_1st_plot <-
  ggplot(norm_build_med_hilo_sum, aes(x = group, y = lo_SD)) +
  geom_point(
    col = 'red',
    fill = 'red',
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  labs(x = 'age group'
       ,
       y = "lo_SD"
       ,
       title = 'lo_SD 1st order polynomial') +
  scale_x_continuous(breaks = seq(1, 13, 1), labels = agestrat) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  geom_smooth(method = 'lm',
              se = FALSE,
              formula = y ~ x) +
  stat_poly_eq(
    formula = y ~ x,
    eq.with.lhs = 'italic(hat(y))~`=`~',
    aes(label = paste(..eq.label.., ..rr.label.., sep = '*plain(\',\')~')),
    parse = TRUE
  )

mean_lo_SD <- round(mean(norm_build_med_hilo_sum$lo_SD), 2)
lo_SD_mean_plot <-
  ggplot(norm_build_med_hilo_sum, aes(x = group, y = lo_SD)) +
  geom_point(
    col = 'red',
    fill = 'red',
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  labs(x = 'age group'
       ,
       y = 'lo_SD'
       ,
       title = 'lo_SD mean substitution') +
  scale_x_continuous(breaks = seq(1, 13, 1), labels = agestrat) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  geom_hline(yintercept = mean_lo_SD) +
  geom_label(aes(5, mean_lo_SD, label = paste0("Mean lo_SD = ", mean_lo_SD)))

print(lo_SD_mean_plot)
print(lo_SD_1st_plot)

# Specify functions to obtain smoothed lo_SDs for 1st-order polynomial and mean substitution smoothing formulas

model_lo_SD_1st <- function() {
  lm(lo_SD ~ group, data = norm_build_med_hilo_sum) %>% assign('model_lo_SD', ., envir = .GlobalEnv)
  get_regression_points(model_lo_SD) %>% pull(lo_SD_hat) %>% assign('lo_SD_sm', ., envir = .GlobalEnv)
}

model_lo_SD_mean <- function() {
  norm_build_med_hilo_sum %>% mutate(mean_lo_SD = mean_lo_SD) %>% pull(mean_lo_SD) %>% assign('lo_SD_sm', ., envir = .GlobalEnv)
}

# Run interactive session allowing user to select lo_SD smoothing model at console

model_prompt_lo_SD <- function() {
  writeLines(
    "\nExamine plots and choose smoothing model for lo_SDs:\n\n1: First-order polynomial: y = ax + b\n2: Mean substitution: y= mean(x)\n"
  )
  model_choice <- as.numeric(0)
  while (is.na(model_choice) || (!(model_choice %in% 1:2))) {
    model_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter choice: ")))
    if (is.na(model_choice)) {
      writeLines("Please enter 1 or 2\n")
    } else {
      if (model_choice == 1) {
        model_lo_SD_1st()
        break
      } else if (model_choice == 2) {
        model_lo_SD_mean()
        break
      } else {
        writeLines("Please enter 1 or 2\n")
      }
    }
  }
}
model_prompt_lo_SD()


# hi_SD 1st-order polynomial and mean substitution plots

hi_SD_1st_plot <-
  ggplot(norm_build_med_hilo_sum, aes(x = group, y = hi_SD)) +
  geom_point(
    col = 'red',
    fill = 'red',
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  labs(x = 'age group'
       ,
       y = "hi_SD"
       ,
       title = 'hi_SD 1st order polynomial') +
  scale_x_continuous(breaks = seq(1, 13, 1), labels = agestrat) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  geom_smooth(method = 'lm',
              se = FALSE,
              formula = y ~ x) +
  stat_poly_eq(
    formula = y ~ x,
    eq.with.lhs = 'italic(hat(y))~`=`~',
    aes(label = paste(..eq.label.., ..rr.label.., sep = '*plain(\',\')~')),
    parse = TRUE
  )

mean_hi_SD <- round(mean(norm_build_med_hilo_sum$hi_SD), 2)
hi_SD_mean_plot <-
  ggplot(norm_build_med_hilo_sum, aes(x = group, y = hi_SD)) +
  geom_point(
    col = 'red',
    fill = 'red',
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  labs(x = 'age group'
       ,
       y = 'hi_SD'
       ,
       title = 'hi_SD mean substitution') +
  scale_x_continuous(breaks = seq(1, 13, 1), labels = agestrat) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  geom_hline(yintercept = mean_hi_SD) +
  geom_label(aes(5, mean_hi_SD, label = paste0("Mean hi_SD = ", mean_hi_SD)))

print(hi_SD_mean_plot)
print(hi_SD_1st_plot)

# Specify functions to obtain smoothed hi_SDs for 1st-order polynomial and mean substitution smoothing formulas

model_hi_SD_1st <- function() {
  lm(hi_SD ~ group, data = norm_build_med_hilo_sum) %>% assign('model_hi_SD', ., envir = .GlobalEnv)
  get_regression_points(model_hi_SD) %>% pull(hi_SD_hat) %>% assign('hi_SD_sm', ., envir = .GlobalEnv)
}

model_hi_SD_mean <- function() {
  norm_build_med_hilo_sum %>% mutate(mean_hi_SD = mean_hi_SD) %>% pull(mean_hi_SD) %>% assign('hi_SD_sm', ., envir = .GlobalEnv)
}

# Run interactive session allowing user to select hi_SD smoothing model at console

model_prompt_hi_SD <- function() {
  writeLines(
    "\nExamine plots and choose smoothing model for hi_SDs:\n\n1: First-order polynomial: y = ax + b\n2: Mean substitution: y= mean(x)\n"
  )
  model_choice <- as.numeric(0)
  while (is.na(model_choice) || (!(model_choice %in% 1:2))) {
    model_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter choice: ")))
    if (is.na(model_choice)) {
      writeLines("Please enter 1 or 2\n")
    } else {
      if (model_choice == 1) {
        model_hi_SD_1st()
        break
      } else if (model_choice == 2) {
        model_hi_SD_mean()
        break
      } else {
        writeLines("Please enter 1 or 2\n")
      }
    }
  }
}
model_prompt_hi_SD()

# Create table needed for next phase of norms process
smooth_med_SD <-
  cbind(norm_build_med_hilo_sum, median_sm, lo_SD_sm, hi_SD_sm) %>% dplyr::select(-(lo1:hi2)) %>%
  mutate(ES_sm = round((median_sm - lag(median_sm)) / ((
    lo_SD_sm + lag(lo_SD_sm) + hi_SD_sm + lag(hi_SD_sm)
  ) / 4), 3))

# Clean up environment
rm(list = ls()[!ls() %in% c('smooth_med_SD')])
