if (exists("joined_dataset") == FALSE) {
  Indexberakningar_data <- import_slu_mvm_data_excel(here("Indata2019/Sjöar/Växtplankton/Trend_stora_vplankton.xlsx"), numeric_var = 31, sheet = 3)
  IKEU_indexberakningar <- import_slu_mvm_data_excel(here("Indata2019/Sjöar/Växtplankton/IKEU_vplankton.xlsx"), numeric_var = 31, sheet = 3)

  indexberakningar <- full_join(Indexberakningar_data, IKEU_indexberakningar)

  Vaxtplankton_data <- import_slu_mvm_data_excel("Indata2019/Sjöar/Växtplankton/Trend_stora_vplankton.xlsx", numeric_var = 29, sheet = 2)
  IKEU_Vaxtplankton <- import_slu_mvm_data_excel("Indata2019/Sjöar/Växtplankton/IKEU_vplankton.xlsx", numeric_var = 29, sheet = 2)

  vaxtplankton <- full_join(Vaxtplankton_data, IKEU_Vaxtplankton) %>%
    mutate_at(29:42, Vectorize(function(x) {
      if (is.na(x)) {
        0
      } else {
        x
      }
    })) %>%
    mutate(Biovolym2 = rowSums(.[, 29:42]) %>% round(3))

  Vattenkemi_data <- import_slu_mvm_data_excel(here("Indata2019/Sjöar/Vattenkemi/trend_stora_kemi.xlsx"), 26)
  IKEU_Vattenkemi <- import_slu_mvm_data_excel(here("Indata2019/Sjöar/Vattenkemi/IKEU_kemi.xlsx"), 26)
    #rename(`Kfyll (µg/l)` = `Kfyll (µg/l)...54`) %>%
   # mutate(`Kfyll (µg/l)` = coalesce(`Kfyll (µg/l)`, `Kfyll (µg/l)...55`) %>% coalesce(`Kfyll (mg/m3)`)) %>%
    #select(-`Kfyll (µg/l)...55`, -`Kfyll (mg/m3)`)

  vattenkemi <- full_join(Vattenkemi_data, IKEU_Vattenkemi)

  indexberakningar %>% # join the files
    full_join(vattenkemi) %>%
    full_join(vaxtplankton) %>%
    mutate( # `Kfyll (µg/l)` = coalesce(`Kfyll (mg/m3)`, `Kfyll (µg/l)`), # same unit, different names
      `Min provdjup (m)` = coalesce(`Min provdjup (m)`, `Min Provdjup (m)`), # merge duplicate column
      `Max provdjup (m)` = coalesce(`Max provdjup (m)`, `Max Provdjup (m)`),
      `Biovolym (mm3/l)` = coalesce(`Biovolym (mm3/l)`, Biovolym2)
    ) %>%
    select( #-`Kfyll (mg/m3)`,
      -`Min Provdjup (m)`, -`Max Provdjup (m)`, -Biovolym2
    ) %>%
    mutate(
      `EU id` = case_when(
        `EU id` == "SE655587-158869" ~ "SE655605-158820", # Stora Envättern   dessa tre från fisk eftersom de finns med i VISS
        `EU id` == "SE656419-164404" ~ "SE656515-164330", # Stensjön
        `EU id` == "SE656590-164240" ~ "SE656640-164224", # Längsjön"SE655275-153234"~ "NW655349-153274",  #Älgsjön
        `EU id` == "SE656612-164132" ~ "SE656574-164098", # Årsjön
        `EU id` == "SE627443-149526" ~ "NW627437-149509", # Brunnsjön
        `EU id` == "SE653737-125017" ~ "NW653647-125036", # Ejgdesjön
        `EU id` == "SE645289-128665" ~ "NW645343-128665", # Fräcksjön
        `EU id` == "SE649314-149514" ~ "NW649253-149503", # Geten
        `EU id` == "SE633209-141991" ~ "NW633246-141963", # Gyslättasjön
        `EU id` == "SE643914-127698" ~ "NW643960-127717", # Härsvatten
        `EU id` == "SE683582-154935" ~ "NW683582-154935", # Källsjön
        `EU id` == "SE656640-164224" ~ "SE656590-164240", # Längsjön
        `EU id` == "SE662682-132860" ~ "SE656590-164240", # Örvattnet
        `EU id` == "SE674570-141911" ~ "NW674570-141911", # Rådsjön
        `EU id` == "SE652902-125783" ~ "NW652888-125811", # Rotehogstjärnen
        `EU id` == "SE666268-142230" ~ "NW666191-142252", # Skifsen
        `EU id` == "SE656515-164330" ~ "SE656419-164404", # Stensjön
        `EU id` == "SE664620-148590" ~ "NW664611-148550", # Västa Skälsjön
        TRUE ~ `EU id`
      ),
      `MS_CD C3` = case_when(
        `EU id` == "SE656612-164132" ~ "WA17355956", # Årsjön
        `EU id` == "SE649314-149514" ~ "WA74940653", # Geten
        `EU id` == "SE683582-154935" ~ "WA29887255", # Källsjön
        `EU id` == "SE674570-141911" ~ "WA24280365", # Rådsjön
        `EU id` == "SE666268-142230" ~ "WA70203019", # Skifsen
        TRUE ~ `MS_CD C3`
      ),
      Stationsnamn = case_when(
        Stationsnamn == "V. Skälsjön" ~ "Västra Skälsjön",
        TRUE ~ Stationsnamn
      )
    ) -> joined_dataset

}


if(exists("Indexberakningar_data")==T){rm(Indexberakningar_data)}
if(exists("IKEU_indexberakningar")==T){rm(IKEU_indexberakningar)}
if(exists("indexberakningar")==T){rm(indexberakningar)}
if(exists("Vaxtplankton_data")==T){rm(Vaxtplankton_data)}
if(exists("IKEU_Vaxtplankton")==T){rm(IKEU_Vaxtplankton)}
if(exists("vaxtplankton")==T){rm(vaxtplankton)}
if(exists("Vattenkemi_data")==T){rm(Vattenkemi_data)}
if(exists("IKEU_Vattenkemi")==T){rm(IKEU_Vattenkemi)}
if(exists("vattenkemi")==T){rm(vattenkemi)}
