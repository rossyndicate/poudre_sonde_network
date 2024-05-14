#' Add flags to the `flag` column of a dataframe based on large swaths of suspect data.
#' "24hr anomaly flag" is added if more than 50% of the data points in a 24 hour window are flagged.
#' "anomaly window" flag is added if the point is included in a 24hr anomaly.
#' @param df A dataframe with a `flag` column.
#' @return A dataframe with a `flag` column that has been updated with the relevant large anomaly flags.
#' @examples
#' add_large_anomaly_flags(df = all_data_flagged$`archery-Actual Conductivity`)
#' add_large_anomaly_flags(df = all_data_flagged$`boxelder-Temperature`)

library(testthat)
library(tidyverse)
library(zoo)

add_suspect_flag <- function(df) {

  flag_string <- "sonde not employed|missing data|site visit|sv window"

  # Define a function to check if a given 3-hour window has >= 50% fails
  check_3_hour_window_fail <- function(x) {
    sum(x) / length(x) >= 0.5
  }

  df_test <- df %>%
    mutate(flag_binary = ifelse((is.na(flag) | grepl(flag_string, flag)), 0, 1)) %>%
    #arrange(timestamp) %>%
    mutate(over_50_percent_fail_window = ifelse(is.na(over_50_percent_fail_window),
                                                zoo::rollapply(flag_binary, width = 12, FUN = check_3_hour_window_fail, fill = NA, align = "right"),
                                                over_50_percent_fail_window)) %>%
    add_flag(over_50_percent_fail_window == TRUE & !grepl("suspect data", flag), "suspect data") # is this flagging the correct data?

  return(df_test)

}

# Read in lists of subsets of data ----
test_data_dir <- "test_env/test_data/"

head_csv_files <- list.files(test_data_dir, pattern = "\\_head_6hr.csv$", full.names = T)
head_data_list <- map(head_csv_files, read_csv)

tail_csv_files <- list.files(test_data_dir, pattern = "\\_tail_6hr.csv$", full.names = T)
tail_data_list <- map(tail_csv_files, read_csv)

random_csv_files <- list.files(test_data_dir, pattern = "\\_rdm_6hr.csv$", full.names = T)
random_data_list <- map(random_csv_files, read_csv)

test_data_list <- list(head_data_list, tail_data_list, random_data_list)
# Test the function on real data ----

## test on a single row of data

### lists of samples
single_row_list <- map(test_data_list,
                       function(data) {
                         map(data, ~{slice_sample(.x, n = 1)})
                       })

### test
single_row_test <- function(row){
  test_that(desc = "'suspect data' flag is not added to a single row",
            code = {
              test <- add_suspect_flag(row) # Runs the function
              # Test that the object returned is a data frame
              expect_true(is.data.frame(test))
              # Test that the flag column does not contain 'suspect flag'
              expect_no_match(test$flag, "suspect flag")
              }
            )
}

### test sample rows
walk(single_row_list, function(data) {
  walk(data, ~{single_row_test(.x)})
})

## test on data with 0% flags

# I know that the dfs that I am about to test do not have any flags,
# so we should not expect the "suspect flag" to be present in any of `flag`
# columns after the `add_suspect_flag()` function is applied. (JD)

### lists of samples
df_no_flag_list <- map(test_data_list, ~.x[[1]])

### test
no_flag_test <- function(df) {
  test_that(desc = "'suspect data' flag is not added to a df with no flags",
            code = {
              test <- add_suspect_flag(df)
              expect_true(is.data.frame(test))
              expect_no_match(test$flag, "suspect flag")
              }
            )
}

### test sample rows
walk(df_no_flag_list, no_flag_test)

## test synthetic data ----

### test on data with flag string present ----
flag_string <- c("sonde not employed","missing data","site visit","sv window")

sv_data <- tibble(flag = rep(flag_string, times = 3), over_50_percent_fail_window = NA, .rows = 12)

test_that("no flags add to data that contains only flag_string strings", code = {
  test <- add_suspect_flag(sv_data)
  expect_true(is.data.frame(test))
  expect_no_match(test$flag, "suspect data")
})

sv_mix_data <- tibble(flag = rep(paste0(flag_string, ";\\n", rev(flag_string)), times = 3), over_50_percent_fail_window = NA, .rows = 12)

test_that("no flags add to data that contains only flag_string strings", code = {
  test <- add_suspect_flag(sv_mix_data)
  expect_true(is.data.frame(test))
  expect_no_match(test$flag, "suspect data")
})

### create single 3 hour chunk of data ----

values <- c("sample flag", NA)

# 0%
leq50_0 <- tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 12)

# 25%
eq25_top <- bind_rows(tibble(flag = "sample flag", over_50_percent_fail_window = NA, .rows = 3),
                       tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 9))

eq25_bottom <- bind_rows(tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 9),
                          tibble(flag = "sample flag", over_50_percent_fail_window = NA, .rows = 3))

eq25_middle <- bind_rows(tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 4),
                          tibble(flag = "sample flag", over_50_percent_fail_window = NA, .rows = 3),
                          tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 5))

eq25_alternate <- bind_rows(tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 3),
                             tibble(flag = rep(values, times = 3), over_50_percent_fail_window = NA, .rows = 6),
                             tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 6))

eq25_list <- list(eq25_top,eq25_bottom,eq25_middle,eq25_alternate)

# 50%
eq50_top <- bind_rows(tibble(flag = "sample flag", over_50_percent_fail_window = NA, .rows = 6),
                       tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 6))

eq50_bottom <- bind_rows(tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 6),
                          tibble(flag = "sample flag", over_50_percent_fail_window = NA, .rows = 6))

eq50_middle <- bind_rows(tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 3),
                          tibble(flag = "sample flag", over_50_percent_fail_window = NA, .rows = 6),
                          tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 3))

eq50_alternate <- bind_rows(tibble(flag = rep(values, times = 6), over_50_percent_fail_window = NA, .rows = 12))

eq50_list <- list(eq50_top, eq50_bottom, eq50_middle, eq50_alternate)

### test on data with 75% flags
eq75_top <- bind_rows(tibble(flag = "sample flag", over_50_percent_fail_window = NA, .rows = 9),
                       tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 3))

eq75_bottom <- bind_rows(tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 3),
                          tibble(flag = "sample flag", over_50_percent_fail_window = NA, .rows = 9))

eq75_middle <- bind_rows(tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 1),
                          tibble(flag = "sample flag", over_50_percent_fail_window = NA, .rows = 9),
                          tibble(flag = NA, over_50_percent_fail_window = NA, .rows = 2))

eq75_list <- list(eq75_top, eq75_bottom, eq75_middle)

### test on data with 100% flags
geq50_100 <- tibble(flag = "sample_flag", over_50_percent_fail_window = NA, .rows = 12)

### test on 3 hour chunk of synthetic data ----

# 0% data flagged
test_that("no flags added to 0% flagged data", code = {
  test <- add_suspect_flag(leq50_0)
  expect_true(is.data.frame(test))
  expect_no_match(test$flag, "suspect data")
})

# 25% data flagged
test_eq_25 <- function(data) {
  test_that("no flags added to 25% flagged data", code = {
    test <- add_suspect_flag(data)
    expect_true(is.data.frame(test))
    expect_no_match(test$flag, "suspect data")
  })
}

walk(eq25_list, test_eq_25)

# 50% data flagged
test_eq_50 <- function(data) {
  test_that("suspect flag added to 50% flagged data", code = {
    test <- add_suspect_flag(data)
    expect_true(is.data.frame(test))
    expect_match(test$flag[12], "suspect data")
  })
}

walk(eq50_list, test_eq_50)

# 75% data flagged
test_eq_75 <- function(data) {
  test_that("suspect flag added to 75% flagged data", code = {
    test <- add_suspect_flag(data)
    expect_true(is.data.frame(test))
    expect_match(test$flag[12], "suspect data")
  })
}

walk(eq75_list, test_eq_75)

# 100% data flagged
test_that("suspect flag added to 100% flagged data", code = {
  test <- add_suspect_flag(geq50_100)
  expect_true(is.data.frame(test))
  expect_match(test$flag[12], "suspect data")
})

### create a possible combinations of 3 hour chunks to imitate 6 hour chunks of data that will be flagged together ----
combo_list <- list(
  leq50_0 = leq50_0,
  eq25_alternate = eq25_alternate,
  eq25_bottom = eq25_bottom,
  eq25_middle = eq25_middle,
  eq25_top = eq25_top,
  eq50_alternate = eq50_alternate,
  eq50_bottom = eq50_bottom,
  eq50_middle = eq50_middle,
  eq50_top = eq50_top,
  eq75_bottom = eq75_bottom,
  eq75_middle = eq75_middle,
  eq75_top = eq75_top,
  geq50_100 = geq50_100
)

combo_list_crossings <- cross2(combo_list, combo_list)

combo_list_names <- cross2(names(combo_list), names(combo_list)) %>%
  map(., function(item) {
    paste(item[[1]], item[[2]], sep = "...")
  })

pair_combos <- map(combo_list_crossings, bind_rows)

names(pair_combos) <- combo_list_names

### test all possible combinations to see which combinations fail tests ----

flag_pair_combos <- map(pair_combos, add_suspect_flag)

### pair combos with no suspect data flag added
no_flag_flag_pair_combos <- keep(flag_pair_combos, ~ !any(str_detect(.x$flag, "suspect data"), na.rm = T))

### pair combos with suspect data flag added
flagged_flag_pair_combos <- keep(flag_pair_combos, ~ any(str_detect(.x$flag, "suspect data"), na.rm = T))

### THIS FUNCTION IS FLAWED. ----

# 1. Let's take this as an example:
flagged_flag_pair_combos[["eq25_bottom...eq25_top"]] -> test_flagged_data
## We can see that this function worked as expected here...

## 2. But when we take this subset of data and treat it as the appended historical
## flagged data for the next chunk of data we can see where the error is happening.
subset_test_flagged_data <- slice_tail(test_flagged_data, n = 12)
# 3. Bind this data with a df that has flag = NA
bound_subset_test_flagged_data <- bind_rows(subset_test_flagged_data, leq50_0)
# 4. Flag this new df
flagged_bound_subset_test_flagged_data <- add_suspect_flag(bound_subset_test_flagged_data)
View(flagged_bound_subset_test_flagged_data)

# We can see here that the "suspect data" flag was counted as a flag, but we actually want to avoid this.
# I think that the solution for this is pretty simple. We will source the updated function and repeat
# step 4 with the updated function.

new_add_suspect_flag <- function(df) {

  flag_string <- "sonde not employed|missing data|site visit|sv window|suspect data" # include the flag added by this function

  # Define a function to check if a given 3-hour window has >= 50% fails
  check_3_hour_window_fail <- function(x) {
    sum(x) / length(x) >= 0.5
  }

  df_test <- df %>%
    mutate(flag_binary = ifelse((is.na(flag) | grepl(flag_string, flag)), 0, 1)) %>%
    #arrange(timestamp) %>%
    mutate(over_50_percent_fail_window = ifelse(is.na(over_50_percent_fail_window),
                                                zoo::rollapply(flag_binary, width = 12, FUN = check_3_hour_window_fail, fill = NA, align = "right"),
                                                over_50_percent_fail_window)) %>%
    add_flag(over_50_percent_fail_window == TRUE & !grepl("suspect data", flag), "suspect data") # is this flagging the correct data?

  return(df_test)

}

new_flagged_bound_subset_test_flagged_data <- new_add_suspect_flag(bound_subset_test_flagged_data)
View(new_flagged_bound_subset_test_flagged_data)

# Now we can see that the "suspect data" flag is not added on to the flag column
# unnecessarily
