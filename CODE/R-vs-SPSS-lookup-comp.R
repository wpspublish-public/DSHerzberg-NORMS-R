# next code section combines R raw-to-SS lookup with SPSS raw-to-SS lookup, in a
# format that can be used in Excel to compare results of the two methods.

# names for calculated columns
calc_cols <- c(paste0("calc_", 1:num_agestrat))

# df containing empty calc columns that can be joined to other tables.
calc_cols_df <- bind_cols(
  enframe(min_raw:max_raw, name = NULL, value = 'rawscore'),
  data.frame(matrix(NA_real_, nrow = max_raw + 1, ncol = num_agestrat)) %>%
    set_colnames(calc_cols)
)

# create list containing three tables to join: R raw-SS-lookup, SPSS
# raw-SS-lookup, and empty columns for calculation.
tables_to_join <- list(raw_to_SS_lookup,
                       suppressMessages(read_csv(
                         here(
                           'CASL-2 COMPS/RAW-TO-SS-LOOKUP-TABLES-SPSS/SPSS-LOOKUPS/RV-Age-Norms-Raw-to-SS-from-SPSS.csv'
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
    rawscore, mo_36, a0300, calc_1, mo_39, a0330, calc_2, mo_42, a0360, calc_3, mo_45, a0390,
    calc_4, mo_48, a0400, calc_5, mo_51, a0430, calc_6, mo_54, a0460, calc_7, mo_57, a0490,
    calc_8, mo_60, a0500, calc_9, mo_63, a0530, calc_10, mo_66, a0560, calc_11, mo_69, a0590,
    calc_12, mo_72, a0600, calc_13, mo_75, a0630, calc_14, mo_78, a0660, calc_15, mo_81,
    a0690, calc_16, mo_84, a0700, calc_17, mo_87, a0730, calc_18, mo_90, a0760, calc_19,
    mo_93, a0790, calc_20, mo_96, a0800, calc_21, mo_102, a0860, calc_22, mo_108, a0900,
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
  abs(R_SPSS_comps_table[calc_cols_pos - 1] - R_SPSS_comps_table[calc_cols_pos - 2])

# write table for doing SS comps in excel
write_csv(R_SPSS_comps_table,
          here('CASL-2 COMPS/RAW-TO-SS-LOOKUP-TABLES-SPSS/COMPS/RV-R-SPSS-SS-comps.csv'
          )
)
