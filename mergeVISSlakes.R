library(tidyverse)
library(here)

sjöar <- read.csv(here("Koordinatfiler/sjöar med long lat1.csv")) %>%
  mutate(Stationsnamn = case_when(
    Stationsnamn == "V. Skälsjön" ~ "Västra Skälsjön",
    TRUE ~ as.character(Stationsnamn)
  ))

centroid <- read.csv(here("Koordinatfiler/centroiderVISS_med long lat.csv"))

fisk <- read.csv(here("Indata2018/Fiske/SverigesVattenmiljöFiskSjöNpueB&Narter.csv"))
fisk1 <- distinct(fisk, MS_CD, .keep_all = TRUE)

new <- full_join(sjöar, fisk1, by = c("EU.id" = "EU_CD")) %>% select(X, Y, Stationsnamn, EU.id, MS_CD.C3, MS_CD, NAMN, WGS84_N, WGS84_E)

VISS <- centroid %>%
  select(X, Y, MS_CD, EU_CD, SJONAMN) %>%
  right_join(new, merge, by = c("EU_CD" = "EU.id")) %>%
  mutate(MS_CD.C3 = case_when(
    EU_CD == "SE656612-164132" ~ "WA17355956", # Årsjön
    EU_CD == "SE649314-149514" ~ "WA74940653", # Geten
    EU_CD == "SE683582-154935" ~ "WA29887255", # Källsjön
    EU_CD == "SE674570-141911" ~ "WA24280365", # Rådsjön
    EU_CD == "SE666268-142230" ~ "WA70203019", # Skifsen
    TRUE ~ as.character(MS_CD.C3)
  ))

# Lägg till Bolmen vid tillfälle: miljögiftssjö
# Bolmen	SE629511-136866	WA29456646	Sjö	Sjöar och Vattendrag	Södra Sverige	13.70307857	56.92638332


# MS_CD, MS_CD.C3
Koordinatfil <- VISS %>%
  mutate(
    long = ifelse(is.na(X.y), ifelse(is.na(X.x), WGS84_E, X.x), X.y),
    lat = ifelse(is.na(X.y), ifelse(is.na(Y.x), WGS84_N, Y.x), Y.y),
    StationsID = EU_CD,
    VattenförekomstID = ifelse(is.na(MS_CD.C3), ifelse(is.na(MS_CD.x), as.character(MS_CD.y), as.character(MS_CD.x)), as.character(MS_CD.C3)), Stationsnamn = ifelse(is.na(Stationsnamn), as.character(NAMN), as.character(Stationsnamn)),
    Typ = "Sjö", Vattenmiljö = "Sjöar och Vattendrag", Region = ""
  ) %>%
  select(Stationsnamn, StationsID, VattenförekomstID, Typ, Vattenmiljö, Region, long, lat) %>%
  # arrange(desc(StationsID))
  arrange(Stationsnamn, desc(StationsID)) %>%
  group_by(VattenförekomstID, Stationsnamn) %>%
  distinct(VattenförekomstID, .keep_all = TRUE)

write.csv(Koordinatfil, file = here("Koordinatfiler/LakeHierarkies_utan_regioner.csv"), row.names = FALSE, na = "")
