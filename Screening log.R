## Author: Elke van Daal
## Project: Fit4Surgery PREHAB trial - Screening log
## Start date: 6-12-2023

## load packages
library(tidyverse)
library(haven)
library(readxl)
library(xlsx)
library(writexl)
library(hablar)
library(janitor)

## Read excel files 6-12-2023
log <- read_excel("Z:\\Data PREHAB trial\\Totaaloverzicht F4S.xlsx", sheet = "Screening new")
scr <- read_excel("Z:\\Data PREHAB trial\\Totaaloverzicht F4S.xlsx", sheet = "screening log check")

## Change dates into correct format
log$`Datum operatie`<- as.Date(as.numeric(log$`Datum operatie`), origin = "1899-12-30")
log$`Meetmoment 1`<- as.Date(as.numeric(log$`Meetmoment 1`), origin = "1899-12-30")
log$`Meetmoment 2`<- as.Date(as.numeric(log$`Meetmoment 2`), origin = "1899-12-30")
log$`Meetmoment 3`<- as.Date(as.numeric(log$`Meetmoment 3`), origin = "1899-12-30")

scr$`Geïnformeerd op:` <- as.Date(as.numeric(scr$`Geïnformeerd op:`), origin = "1899-12-30")
scr$`OK datum` <- as.Date(as.numeric(scr$`OK datum`), origin = "1899-12-30")

## Use lower cases
log <- mutate_all(log, .funs = tolower)
scr <- mutate_all(scr, .funs = tolower)

## Store MDN as numeric
log <- log %>%
  rename(MDN = `Patiëntnummer (EPIC)`) %>%
  mutate(MDN = as.numeric(MDN))

scr <- scr %>%
  mutate(MDN = as.numeric(MDN))

## Remove log mdn's from scr mdn's 
screening <- anti_join(scr, log, by = 'MDN')

## Save as excel file
write_xlsx(screening, "Z:\\Data PREHAB trial\\screening_log.xlsx")

