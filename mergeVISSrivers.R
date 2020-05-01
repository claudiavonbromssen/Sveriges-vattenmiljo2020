library(tidyverse)
library(readxl)

vdr<-read.csv("C:\\Users\\claudia\\Documents\\Projekt\\Havsmiljöinstitutet\\Sveriges Vattenmiljö 2018\\Sötvatten\\Koordinatfiler\\vattendrag med long lat.csv")%>%
    mutate(Stationsnamn=case_when(Stationsnamn==as.character("Mattjåkkbäcken")~as.character("Mattjockbäcken"),
                            TRUE ~as.character(Stationsnamn)))

fisk<-read.csv("C:\\Users\\claudia\\Documents\\Projekt\\Havsmiljöinstitutet\\Sveriges Vattenmiljö 2018\\Sötvatten\\Koordinatfiler\\FiskVattendrag_lat_long.csv")
fisk1<-distinct(fisk, MS_CD, .keep_all = TRUE)%>%
  filter(EU_CD !="SE673902-577077")%>%  #Björnbackån
  mutate(Vdragnamn=case_when(Vdragnamn==as.character("Mattjokkbäcken")~as.character("Mattjockbäcken"),
                                TRUE ~as.character(Vdragnamn)))

new_vdr<-full_join(vdr, fisk1, by=c("MS_CD.C3"="MS_CD", "Stationsnamn"="Vdragnamn")) %>% select(X.x,Y.x, X.y, Y.y, Stationsnamn, EU.id, MS_CD.C3, EU_CD)

#olika<-full_join(vdr, fisk1, by=c("MS_CD.C3"="MS_CD", "EU.id"="EU_CD"))%>%
#  select(EU.id, MS_CD.C3, Stationsnamn, Vdragnamn)
#write.csv(olika, file = "C:\\Users\\claudia\\Documents\\Projekt\\Havsmiljöinstitutet\\Sveriges Vattenmiljö 2018\\Sötvatten\\Koordinatfiler\\olikastationer_sammavattendrag.csv",row.names=FALSE, na="")


#MS_CD, MS_CD.C3
#Koordinatfil<-VISS %>% mutate(long = ifelse(is.na(X.y), ifelse(is.na(X.x), WGS84_E,X.x), X.y), lat=ifelse(is.na(X.y), ifelse(is.na(Y.x), WGS84_N, Y.x), Y.y), stationsID=ifelse(is.na(MS_CD.C3), ifelse(is.na(MS_CD.x), as.character(MS_CD.y),as.character(MS_CD.x)), as.character(MS_CD.C3)))%>%
#            mutate(stationsID=stationsID,ifelse(is.na(stationsID), EU_CD, stationsID))%>%
#          mutate(Stationsnamn=ifelse(is.na(Stationsnamn), as.character(NAMN), as.character(Stationsnamn)), Typ="Sjö", Vattenmiljö="Sjöar och Vattendrag", Region="") %>%
#    select(Vattenförekomst, stationsID,Typ, Vattenmiljö, Region, long, lat)

flodmynningar<-read_delim("stationer_flodmynningar.csv", 
                          ",", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                          trim_ws = TRUE)%>%
  select(StationsID, StnNamn)

Regioner<-read_delim("C:\\Users\\claudia\\Documents\\Projekt\\Havsmiljöinstitutet\\Sveriges Vattenmiljö 2018\\Sötvatten\\Koordinatfiler\\Vattendrag_Regioner.csv", 
                     ",", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                     trim_ws = TRUE)
  

Koordinatfil<-new_vdr %>%   mutate(StationsID=case_when(is.na(`EU.id`)~as.character(EU_CD),
                                                        TRUE~as.character(`EU.id`)),
                                   long=case_when(is.na(X.x)~X.y,
                                                  TRUE~X.x),
                                   lat=case_when(is.na(Y.x)~Y.y,
                                                 TRUE~Y.x),
                                  VattenförekomstID=`MS_CD.C3`)%>%
  left_join(flodmynningar)%>%
  left_join(Regioner)%>%
  mutate(Typ="Vattendrag",
         Vattenmiljö=case_when(is.na(StnNamn)~"Sjöar och Vattendrag",
                               StationsID=="SE646771-129330"~"Sjöar och Vattendrag",
                       TRUE~"Flodmynningar")) %>%
    select(Stationsnamn, StationsID, VattenförekomstID, Typ, Vattenmiljö, Region, long, lat) %>%arrange(Stationsnamn)



write.csv(Koordinatfil, file = "C:\\Users\\claudia\\Documents\\Projekt\\Havsmiljöinstitutet\\Sveriges Vattenmiljö 2018\\Sötvatten\\Koordinatfiler\\RiverHierarkies_R.csv",row.names=FALSE, na="")

