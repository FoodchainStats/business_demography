library(httr2)
library(readr)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fsp)

# Eurostat data ----------------------------------------------------------------

url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/sbs_sc_ovw/1.0/*.*.*.*.*?c[freq]=A&c[indic_sbs]=ENT_NR,EMP_NR,NETTUR_MEUR&c[nace_r2]=C10,C11,G463,G472,I56&c[size_emp]=TOTAL,0_1,0-9,2-9,10-19,20-49,50-249,GE250&c[geo]=EU27_2020,BE,BG,CZ,DK,DE,EE,IE,EL,ES,FR,HR,IT,CY,LV,LT,LU,HU,MT,NL,AT,PL,PT,RO,SI,SK,FI,SE,IS,NO,CH,BA,ME,MK,AL,RS&c[TIME_PERIOD]=2023,2022,2021&compress=true&format=csvdata&formatVersion=2.0&lang=en&labels=name"

resp <- httr2::request(url) |> httr2::req_perform()

resp |>
  httr2::resp_body_raw() |>
  writeBin(con = here("data", "eubp.csv.gz"))

data <- readr::read_csv(here("data", "eubp.csv.gz")) |> 
  janitor::clean_names() |> 
  filter(!is.na(obs_value)) |> 
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


eu <- data |> 
  filter(time_period == 2023) |> 
  pivot_wider(names_from = nis2,values_from = tot) |> 
  mutate(pct_nis2 = Y/(N+Y)*100) |> 
  select(year = time_period, country = geopolitical_entity_reporting, sector, N, Y, pct_nis2)

# UK data ----------------------------------------------------------------------

bpe <- get_bpe(year = 2023, sic_digits = 3)

uk <- bpe |> 
  filter(stringr::str_starts(category, "Business number")) |> 
  mutate(size = stringr::str_squish(size), 
         sector = case_when(sic_id %in% c("101", "102", "103", "104", "105", "106", "107", "108", "109", "110") ~ "Manufacturing",
                            sic_id == "463" ~ "Wholesale",
                            sic_id == "472" ~ "Retail",
                            sic_id %in% c("561", "562", "563") ~ "Catering"),
         nis2 = case_when(size %in% c("Medium (50 to 249 employees)",
                                      "Large (250 or more employees)") ~"Y",
                          .default = "N")) |> 
  filter(!is.na(sector), size != "All employers") |> 
  group_by(year, sector, nis2) |> 
  summarise(tot = sum(value)) |> 
  pivot_wider(names_from = nis2,values_from = tot) |> 
  mutate(pct_nis2 = Y/(N+Y)*100) |> 
  mutate(country = "UK") |> 
  select(year, country, sector, N, Y, pct_nis2)

# Joined up data ---------------------------------------------------------------

nis2 <- eu |> bind_rows(uk)



nis2 |> 
  filter(!is.na(pct_nis2), sector == "Retail") |> 
  mutate(isuk = ifelse(country == "UK", "Y", "N")) |> 
  ggplot() +
  geom_col(aes(x = forcats::fct_reorder(country, pct_nis2), y = pct_nis2, fill = isuk)) +
  # facet_wrap(vars(sector)) +
  scale_fill_manual(values = c(Y = "red", N = "grey")) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank()) +
  labs(x = NULL, y = "Percent of businesses with >49 employees", title = "Retail" )


nis2 |> 
  filter(country %in% c("UK", "European Union - 27 countries (from 2020)")) |> 
  ggplot() +
  geom_col(aes(x = country, y = pct_nis2), fill = afcolours::af_colours()[1]) +
  facet_wrap(vars(sector)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) +
  labs(x = NULL, title = "Percent of businesses with >49 employees", y = NULL)

 