# Gemensamma funktioner och import
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, lubridate, mgcv, magrittr, broom, forecast, zoo, rkt, DT, furrr,plan, here)

plan(multiprocess)

rkt_out <- function(rkt){
  tibble(p.value = ifelse(is.na(rkt$sl.corrected)==T, rkt$sl, rkt$sl.corrected), statistic = rkt$S, slope = rkt$B, tau=rkt$tau)
}

mse <- function(sm){sum(sm$residuals^2)/(length(sm$residuals) - 2)}

periods <- function(data,
                    variable,
                    threshold = 3, # number of years allowed missing in a period
                    filter_less_than = NA){ # filters periods with less than N values (not the length from min to max!)
  #threshold <- threshold+1
  variable_enquo <- enquo(variable)
  groups_vec <- data %>% group_vars() %>% syms
  out <- data %>%
    dplyr::arrange(!!variable_enquo) %>%
    mutate(period = cumsum(c(1, diff(!!variable_enquo) >= threshold+1))) %>%
    group_by(period, add=T) %>%
    mutate(n_year_period = n_distinct(!!variable_enquo))

  if(!is_empty(groups_vec)){out <- out %>% group_by(!!!groups_vec)}else{out <- out %>% ungroup}

  if(!is.na(filter_less_than)){out <- out %>%
    filter(n_year_period >= filter_less_than) %>%
    mutate(period = cumsum(c(1, diff(!!variable_enquo) >= threshold+1)))
  }

  return(out)
}

#import_slu_mvm_data <- function(filename, numeric_var = NA, bad_quality_na = T){ # function for importing UTF-16 files into R
#  #numeric var: the first column with measured numeric variables
#  replace_less_than <- function(variable=variable, data=data, bad_quality_na = T){ # function for replacing values at the detection limit with half values
#    data_parsed <- data
#    data_parsed[[variable]] <- data[[variable]] %>% as.character() %>% parse_number(locale = locale(decimal_mark = ","))
#
#    which_intervals <- which(substring(data[[variable]],1,1) == "[")
#    which_less_than <- which(substring(data[[variable]],1,1) == "<")
#    ## Testa att ersätta alla värden under  högsta detektionsgränsen med halva högsta detektionsgränesn
#    ## Ersätt istället med halva detektionsgränsen, det blir bättre då filerna läses in en och en
#    if (length(which_less_than) > 0) {
#      values_less_than_half <- data[[variable]][which_less_than] %>% gsub(",", ".", .) %>% gsub("<","",.) %>% as.numeric()
#      data_parsed[[variable]][which_less_than] <- values_less_than_half/2}
#
#    if (bad_quality_na == T) {data[[variable]][which_intervals] <- NA}
#    else{
#      values_intervals <- data[[variable]][which_intervals] %>% gsub("\\[","", .) %>% gsub("\\]","", .) %>% gsub(",",".", .) %>% as.numeric()
#      data_parsed[[variable]][which_intervals] <- values_intervals}
#
#    return(data_parsed)
#  }
#
#  out <- file(filename, encoding = "UCS-2LE") %>% readLines() #read all lines
#  out[1] <- str_sub(out[1], 1, str_length(out[1]) - 1) #remove tab in the end of first line
#  counts <- str_count(out[1],"\t") # cound number of tabs (cols-1)
#  errorous_rows <- which(sapply(out, FUN = function(x){str_count(x,"\t") != counts}, simplify = T, USE.NAMES = F)) #find pairs of errorous rows
#  error_matrix <- matrix(errorous_rows, ncol = 2) # put pairs in a matrix
#  if (nrow(error_matrix) > 0) { #check whether there are any errors
#    for (i in nrow(error_matrix):1) {
#      out[error_matrix[i,1]] <- paste(out[error_matrix[i,1]], out[error_matrix[i,2]], sep = "") #for each pair, combine strings and put into the first row
#      out <- out[-error_matrix[i,2]] #remove second row # goes down to up
#    }}
#
#  names_of_cols <- strsplit(out[1], split = "\t")[[1]] # extract column names
#  out <- read.table(text = out, sep = "\t", fill = T, header = T, stringsAsFactors = F, dec = ".") # make a data.frame
#  names(out) <- names_of_cols
#  out[out == ""] <- NA
#  if (is.na(numeric_var) == T) {warning("Supply the column index of first measured numeric variable")}else{
#    for (i in numeric_var:ncol(out)) {
#      out <- replace_less_than(i, data = out, bad_quality_na = bad_quality_na) #replace less than with half the minimum of detection limit in each column, remove intervals
#    }}
#  for (i in 1:(numeric_var - 1)) {
#    out[, i] <- out[, i] %>% as.character %>% parse_guess(locale = locale(decimal_mark = ","))
#  }
#  out
#}

import_slu_mvm_data_excel <- function(filename, numeric_var = NA, bad_quality_na = TRUE, sheet = 2){ # function for importing excel files into R
  #numeric var: the first column with measured numeric variables
  replace_less_than_var <- function(values, bad_quality_na){ # function for replacing values at the detection limit with half values
    values_parsed <- values %>% as.character() %>% parse_number(locale = locale(decimal_mark = ","))

    which_intervals <- which(substring(values,1,1) == "[")
    which_less_than <- which(substring(values,1,1) == "<")
    ## Testa att ersätta alla värden under  högsta detektionsgränsen med halva högsta detektionsgränesn
    ## Ersätt istället med halva detektionsgränsen, det blir bättre då filerna läses in en och en
    if (length(which_less_than) > 0) {
      values_less_than_half <- values[which_less_than] %>% gsub(",", ".", .) %>% gsub("<","",.) %>% as.numeric()
      values_parsed[which_less_than] <- values_less_than_half/2}

    if (bad_quality_na == TRUE) {values_parsed[which_intervals] <- NA}
    else{
      values_intervals <- values[which_intervals] %>%
        gsub("\\[","", .) %>%
        gsub("\\]","", .) %>%
        gsub(",",".", .) %>%
        as.numeric()
      values_parsed[which_intervals] <- values_intervals}

    return(values_parsed)
  }

  if (is.na(numeric_var) == T) {stop("Supply the column index of first measured numeric variable")}
out <- suppressWarnings(read_excel(filename, sheet = sheet))
out <-
  mutate_at(out, c(numeric_var:ncol(out)),
            replace_less_than_var,
            bad_quality_na = bad_quality_na)
out <-
  mutate_at(out, c(1:(numeric_var - 1)),
            (function(x) {
                          x %>%
                            as.character() %>%
                            parse_guess(locale = locale(decimal_mark = ","))
              })
           )
  return(out)
}

