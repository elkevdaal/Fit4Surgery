## Author: Elke van Daal
## Project: Fit4Surgery PREHAB trial - Counting groups
## Start date: 5-12-2023

## load packages
library(tidyverse)
library(haven)
library(readxl)
library(xlsx)

## Read excel files
f4s <- read_excel("Z:\\Data PREHAB trial\\Totaaloverzicht F4S.xlsx", sheet = "F4S inclusies") #data F4S participants
excel <- read_excel("Z:\\Data PREHAB trial\\Totaaloverzicht F4S.xlsx", sheet = "Excel log")
screening_f4s <- read_excel("Z:\\Data PREHAB trial\\Totaaloverzicht F4S.xlsx", sheet = "Screening log")

## Change all cases to lower
f4s <- mutate_all(f4s, .funs = tolower)
excel <- mutate_all(excel, .funs = tolower)
screening_f4s <- mutate_all(screening_f4s, .funs = tolower)

## Check whether there is no overlap in MDN numbers between f4s and excel
test <- inner_join(f4s, excel, by = "MDN") #delete 1676595 from excel

## Manipulate excel log 
## Remove subjects who were not eligible
excel <- excel %>%
  filter(MDN != 1676595) %>% #delete 1676595 from excel (is already in f4s df)
  filter(`Study eligibility (ja/nee)` == "ja") #Check mdn 8445981, 1731690, 3448151 (study eligibility), 9539121 (missing)

excel <- excel %>%
  mutate(inclusion = ifelse(`Exclusie deelname (ja/nee)` == "ja","nee", "ja")) %>%
  mutate(participation = "no") #none of these patients participated

View(excel)
## note: reden "ok te snel" in interventie groep is exclusiecriteria! ##
## hier moet dus bij exclusie deelname "ja" worden ingevuld ##

## Count excel log
excel %>%
  count()  #count study eligibility

excel %>%
  count(inclusion) #count inclusions 
                    #check missings "exclusie deelname"
excel %>%
  count(inclusion, `Groep (controle/interventie)`) #count inclusions by group (many controls here!)
                                                  #check groep = ja, check missings in groep
excel %>%
  count(inclusion, Zorgpad) #count inclusions by zorgpad

excel %>%
  count(inclusion, Zorgpad, `Groep (controle/interventie)`) #count inclusions by zorgpad and group

excel %>%
  count(participation) #count participants



test <- inner_join(f4s, excel, by = "MDN")
