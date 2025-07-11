library(httr2)
library(readr)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/sbs_sc_ovw/1.0/*.*.*.*.*?c[freq]=A&c[indic_sbs]=ENT_NR,EMP_NR,NETTUR_MEUR&c[nace_r2]=C10,C11,G463,G472,I56&c[size_emp]=TOTAL,0_1,0-9,2-9,10-19,20-49,50-249,GE250&c[geo]=EU27_2020,BE,BG,CZ,DK,DE,EE,IE,EL,ES,FR,HR,IT,CY,LV,LT,LU,HU,MT,NL,AT,PL,PT,RO,SI,SK,FI,SE,IS,NO,CH,BA,ME,MK,AL,RS&c[TIME_PERIOD]=2023,2022,2021&compress=true&format=csvdata&formatVersion=2.0&lang=en&labels=name"

resp <- httr2::request(url) |> httr2::req_perform()

resp |>
  httr2::resp_body_raw() |>
  writeBin(con = here("data", "eubp.csv.gz"))

data <- readr::read_csv(here("data", "eubp.csv.gz")) |> 
  janitor::clean_names() |> 
  filter(size_emp %in% c("0-9", "10-19", "20-49", "50-249", "GE250")) |> 
  mutate(sector = case_when(nace_r2 %in% c("C10", "C11") ~ "Manufacturing",
                             nace_r2 == "G463" ~ "Wholesale",
                             nace_r2 == "G472" ~ "Retail",
                             nace_r2 == "I56" ~ "Catering"),
         nis2 = case_when(size_emp %in% c("50-249", "GE250") ~ "Y",
                          .default = "N")) |> 
  filter(economical_indicator_for_structural_business_statistics == "Enterprises - number") |> 
  group_by(geopolitical_entity_reporting, time_period, sector, nis2) |> 
  summarise(tot = sum(obs_value))


x <- data |> 
  filter(time_period == 2023) |> 
  pivot_wider(names_from = nis2,values_from = tot) |> 
  mutate(pct_nis2 = Y/(N+Y)*100)

x |> 
  ggplot() +
  geom_col(aes(x = sector, y = pct_nis2)) +
  facet_wrap(vars(geopolitical_entity_reporting))
