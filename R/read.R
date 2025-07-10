library(httr2)
library(readr)
library(here)
library(dplyr)


url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/sbs_sc_ovw/1.0/*.*.*.*.*?c[freq]=A&c[indic_sbs]=ENT_NR,EMP_NR,NETTUR_MEUR&c[nace_r2]=C10,C11,G4617,G4631,G4632,G4633,G4634,G4636,G4637,G4638,G4639,G4711,G4721,G4722,G4723,G4724,G4725,G4729,G4781,I56&c[size_emp]=TOTAL,0_1,0-9,2-9,10-19,20-49,50-249,GE250&c[geo]=EU27_2020,BE,BG,CZ,DK,DE,EE,IE,EL,ES,FR,HR,IT,CY,LV,LT,LU,HU,MT,NL,AT,PL,PT,RO,SI,SK,FI,SE,IS,NO,CH,BA,ME,MK,AL,RS&c[TIME_PERIOD]=2023,2022,2021&compress=true&format=csvdata&formatVersion=2.0&lang=en&labels=name"

resp <- httr2::request(url) |> httr2::req_perform()

resp |>
  httr2::resp_body_raw() |>
  writeBin(con = here("data", "c10c11.csv.gz"))

data <- readr::read_csv(here("data", "c10c11.csv.gz")) |> 
  janitor::clean_names() |> 
  filter(size_emp %in% c("0-9", "10-19", "20-49", "50-249", "GE250")) |> 
  mutate(sector = case_when(nace_r2 %in% c("C10", "C11") ~ "Manufacturing",
                             nace_r2 %in% c("G4617", "G4631", "G4632", "G4633", "G4634", "G4636", "G4637", "G4638", "G4639") ~ "Wholesale",
                             nace_r2 %in% c("G4711", "G4721", "G4722", "G4723", "G4724", "G4725", "G4729", "G4781") ~ "Retail",
                             nace_r2 == "I56" ~ "Catering"),
         nis2 = case_when(size_emp %in% c("50-249", "GE250") ~ "Y",
                          .default = "N"))





x <- data |> 
  filter(economical_indicator_for_structural_business_statistics == "Enterprises - number") |> 
  group_by(geopolitical_entity_reporting, time_period, sector, nis2) |> 
  summarise(tot = sum(obs_value))

