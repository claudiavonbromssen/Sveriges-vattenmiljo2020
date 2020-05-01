# Gemensamma funktioner och import
library(tidyverse)
library(readxl)
library(lubridate)
library(mgcv)
library(magrittr)
library(broom)
library(forecast)
library(zoo)
library(rkt)
library(DT)
library(furrr)

plan(multiprocess)

rkt_out <- function(rkt){
  tibble(p.value = ifelse(is.na(rkt$sl.corrected)==T, rkt$sl, rkt$sl.corrected), statistic = rkt$S, slope = rkt$B, tau=rkt$tau)
}

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
out <- read_excel(filename, sheet = sheet)
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

#Indexberakningar_data <- import_slu_mvm_data_excel("Trend_stora_vplankton.xlsx", numeric_var = 31, sheet = 3)
#IKEU_indexberakningar <- import_slu_mvm_data_excel("IKEU_vplankton.xlsx", numeric_var = 31, sheet = 3)
#
#indexberakningar <- full_join(Indexberakningar_data, IKEU_indexberakningar)
#
#rm(Indexberakningar_data)
#rm(IKEU_indexberakningar)
#
#saveRDS(indexberakningar, "indexberakningar.RDS")
indexberakningar <- readRDS("indexberakningar.RDS")

#Vaxtplankton_data <- import_slu_mvm_data_excel("Trend_stora_vplankton.xlsx", numeric_var = 29, sheet = 2)
#IKEU_Vaxtplankton <- import_slu_mvm_data_excel("IKEU_vplankton.xlsx", numeric_var = 29, sheet = 2)
#
#vaxtplankton <- full_join(Vaxtplankton_data, IKEU_Vaxtplankton)
#rm(Vaxtplankton_data)
#rm(IKEU_Vaxtplankton)
#saveRDS(vaxtplankton, "vaxtplankton.RDS")

vaxtplankton <- readRDS("vaxtplankton.RDS")

vaxtplankton <- vaxtplankton %>% 
  mutate_at(29:42, Vectorize(function(x){if(is.na(x)){0}else{x}})) %>% 
  mutate(Biovolym2 = rowSums(.[,29:42]) %>% round(3))
#Vattenkemi_data <- import_slu_mvm_data_excel("trend_stora_kemi.xlsx", 26)
#IKEU_Vattenkemi <- import_slu_mvm_data_excel("IKEU_kemi.xlsx", 26) %>% 
#  rename(`Kfyll (µg/l)` = `Kfyll (µg/l)...54`) %>% 
#  mutate(`Kfyll (µg/l)` = coalesce(`Kfyll (µg/l)`, `Kfyll (µg/l)...55`) %>% coalesce(`Kfyll (mg/m3)`)) %>% 
#  select(-`Kfyll (µg/l)...55`, -`Kfyll (mg/m3)`)
#
#vattenkemi <- full_join(Vattenkemi_data, IKEU_Vattenkemi)
#rm(Vattenkemi_data)
#rm(IKEU_Vattenkemi)
#saveRDS(vattenkemi,"vattenkemi.RDS")
vattenkemi_sjo <- readRDS("vattenkemi.RDS")


#joined_dataset <- indexberakningar %>% # join the files
#  full_join(vattenkemi) %>% 
#  full_join(vaxtplankton) %>% 
#  mutate(#`Kfyll (µg/l)` = coalesce(`Kfyll (mg/m3)`, `Kfyll (µg/l)`), # same unit, different names
#    `Min provdjup (m)` = coalesce(`Min provdjup (m)`, `Min Provdjup (m)`), # merge duplicate column
#    `Max provdjup (m)` = coalesce(`Max provdjup (m)`, `Max Provdjup (m)`),
#    `Biovolym (mm3/l)` = coalesce(`Biovolym (mm3/l)`, Biovolym2)) %>% 
#  select(#-`Kfyll (mg/m3)`, 
#    -`Min Provdjup (m)`,-`Max Provdjup (m)`,-Biovolym2)
#joined_dataset %>% saveRDS("joined_dataset.RDS")
vaxtplankton <- vaxtplankton %>% select(-Biovolym2)


#Växtplankton och kemi

joined_dataset <- readRDS("joined_dataset.RDS")%>%
 mutate(
  `EU id`= case_when(`EU id`== "SE655587-158869"~ "SE655605-158820", #Stora Envättern   dessa tre från fisk eftersom de finns med i VISS
                     `EU id`== "SE656419-164404"~ "SE656515-164330", #Stensjön
                     `EU id`== "SE656590-164240"~ "SE656640-164224",#Längsjön
                     TRUE~`EU id`),
                     
#   `EU id`== "SE655275-153234"~ "NW655349-153274",  #Älgsjön
#                            `EU id`== "SE656612-164132"~ "SE656574-164098", #Årsjön
#                            `EU id`== "SE627443-149526"~ "NW627437-149509", #Brunnsjön
#                            `EU id`== "SE653737-125017"~ "NW653647-125036", #Ejgdesjön
#                            `EU id`== "SE645289-128665"~ "NW645343-128665", #Fräcksjön
#                            `EU id`== "SE649314-149514"~ "NW649253-149503", #Geten
#                            `EU id`== "SE633209-141991"~ "NW633246-141963", #Gyslättasjön
#                            `EU id`== "SE643914-127698"~ "NW643960-127717", #Härsvatten
#                            `EU id`== "SE683582-154935"~ "NW683582-154935", #Källsjön
#                            `EU id`== "SE656640-164224"~ "SE656590-164240", #Längsjön
#                            `EU id`== "SE662682-132860"~ "SE656590-164240", #Örvattnet
#                            `EU id`== "SE674570-141911"~ "NW674570-141911", #Rådsjön
#                            `EU id`== "SE652902-125783"~ "NW652888-125811", #Rotehogstjärnen
#                            `EU id`== "SE666268-142230"~ "NW666191-142252", #Skifsen
#                            `EU id`== "SE656515-164330"~ "SE656419-164404", #Stensjön
#                            `EU id`== "SE664620-148590"~ "NW664611-148550", #Västa Skälsjön
#                            TRUE~`EU id`),
          `MS_CD C3`=case_when(`EU id`== "SE656612-164132"~ "WA17355956", #Årsjön
                               `EU id`== "SE649314-149514"~"WA74940653", #Geten
                               `EU id`== "SE683582-154935"~ "WA29887255", #Källsjön
                               `EU id`== "SE674570-141911"~ "WA24280365", #Rådsjön
                               `EU id`== "SE666268-142230"~ "WA70203019", #Skifsen
                               TRUE ~`MS_CD C3`),
          Stationsnamn=case_when(Stationsnamn =="V. Skälsjön" ~"Västra Skälsjön",
                                 TRUE ~Stationsnamn))
  

 
                            
        

#trendvattendrag_kemi <- import_slu_mvm_data_excel("Trendvattendrag.xlsx", numeric_var = 26)
#trendvattendrag_kemi %>% saveRDS("trendvattendrag_kemi.RDS")
trendvattendrag_kemi <- readRDS("trendvattendrag_kemi.RDS")



#fisk_sjo<-read.csv("SverigesVattenmiljöFiskSjöNpueB&Narter.csv")
#fisk_sjo %>% mutate(Stationsnamn=NAMN, Provtagningsår=Anno, `Insjöfisk totalmängd`= NpueBtotal, `Insjöfisk abborre`=NpueBAbb, `Insjöfisk gädda`=NpueBGäd, `Insjöfisk mört`=NpueBMör, 
#                    `Insjöfisk röding`=NpueBRöd, `Insjöfisk antal arter`=Narter, `Stationskoordinat N/X`= S99TM_N, `Stationskoordinat E/Y`=S99TM_E) %>% saveRDS("fisk_sjo.RDS")
fisk_sjo <- readRDS("fisk_sjo.RDS")%>%
mutate(`EU_CD`= case_when(`EU_CD`== "NW655349-153274"~ "SE655275-153234",  #Älgsjön
                          `EU_CD`== "SE656574-164098"~ "SE656612-164132", #Årsjön
                          `EU_CD`== "NW627437-149509"~ "SE627443-149526", #Brunnsjön
                          `EU_CD`== "NW653647-125036"~ "SE653737-125017", #Ejgdesjön
                          `EU_CD`== "NW645343-128665"~ "SE645289-128665", #Fräcksjön
                          `EU_CD`== "NW649253-149503"~ "SE649314-149514", #Geten
                          `EU_CD`== "NW633246-141963"~ "SE633209-141991", #Gyslättasjön
                          `EU_CD`== "NW643960-127717"~ "SE643914-127698", #Härsvatten
                          `EU_CD`== "NW683582-154935"~ "SE683582-154935", #Källsjön
                          `EU_CD`== "SE656590-164240"~ "SE656640-164224", #Längsjön
                          `EU_CD`== "NW662756-132817"~ "SE662682-132860", #Örvattnet
                          `EU_CD`== "NW674570-141911"~ "SE674570-141911", #Rådsjön
                          `EU_CD`== "NW652888-125811"~ "SE652902-125783", #Rotehogstjärnen
                          `EU_CD`== "NW666191-142252"~ "SE666268-142230", #Skifsen
                          `EU_CD`== "SE656419-164404"~ "SE656515-164330", #Stensjön
                          `EU_CD`== "NW664611-148550"~ "SE664620-148590", #Västa Skälsjön
                          `EU_CD`== "SE655587-158869"~ "SE655605-158820", #Stora Envättern
                          TRUE ~as.character(`EU_CD`)))

#fisk_vdr<-read.csv("FiskVattendrag.csv")
#fisk_vdr %>% transmute(Stationsnamn=Vdragnamn, Provtagningsår=År, `Vattendrag lax`= Medeltäthet_Lax100m2, `Vattendrag öring`=Medeltäthet_Öring100m2, `Vattendrag totalmängd fisk`=Totaltäthet100m2, `Vattendrag antal arter`=Medelantalfiskarter, 
#                     EU_CD=case_when(EU_CD==as.character("SE673902-577077") ~ as.character("SE673809-153401"), #Björnbackån
#                                    TRUE~as.character(EU_CD)),
#                     `Stationskoordinat N/X`=XRT90,
#                     `Stationskoordinat E/Y`=YRT90,
#                     MS_CD=MS_CD)%>% 
#  saveRDS("fisk_vdr.RDS")
fisk_vdr <- readRDS("fisk_vdr.RDS")%>%
  mutate(`EU_CD`= case_when(`EU_CD`== "SE666830-151744" ~"SE666881-151821", #	Bjurforsbäcken
                            `EU_CD`=="SE724171-151176" ~	"SE724097-151160", #	Skansnäsån
                            `EU_CD`=="SE640224-138043" ~	"SE640085-138148", #	Helgaboån
                            `EU_CD`=="SE723329-161534" ~	"SE723232-161455", #	Mattjockbäcken
                            `EU_CD`=="SE692738-153336" ~	"SE692688-153260", #	Viskansbäcken
                            `EU_CD`=="SE753561-165305" ~	"SE753460-165285", #	Akkarjåkkå
                            `EU_CD`=="SE639661-133346" ~	"SE639719-133565", #	Lindåsabäcken
                            `EU_CD`=="SE699392-139462" ~	"SE699780-139553", #	Lekarån
                            `EU_CD`=="SE737666-158035" ~	"SE737675-158280", #	Viepsajåkkå
                            `EU_CD`=="SE741654-169042" ~	"SE741419-169012", #	Muddusälven
                            `EU_CD`=="SE727831-165123" ~	"SE728070-165120", #	Bergmyrbäcken
                            `EU_CD`=="SE754016-181191" ~	"SE753778-181417", #	Kitkiöjoki
                            `EU_CD`=="SE708384-145451" ~	"SE708380-145455", #	Hökvattsån
                            `EU_CD`=="SE641905-137438" ~	"SE641953-137406", #	Kolarebäcken
                            `EU_CD`=="SE703665-153560" ~	"SE703626-153615", #	Kvarnån
                            `EU_CD`=="SE639676-147775" ~	"SE639020-148650",	#Silverån
                            TRUE~`EU_CD`))

