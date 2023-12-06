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

## nOte: screening_f4s, patients without OK have been removed, 
## patients who participated (without remarks) have been removed
## all other patients have to be inclusions?? Or do we have to check EPIC?
## check patients with te korte tijd, controls can remain included,
## intervention patients have to be excluded
## then, all patients with bijzonderheden = "excluderen!" should be excluded from f4s file (inclusion variable = no)

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

## Add variables inclusion and participation
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
  filter(is.na(inclusion)) %>%
  filter(is.na(`Groep (controle/interventie)`)) %>%
  pull(MDN)  #pull mdn's with missings

excel %>%
  count(inclusion, Zorgpad) #count inclusions by zorgpad

excel %>%
  count(inclusion, Zorgpad, `Groep (controle/interventie)`) #count inclusions by zorgpad and group

excel %>%
  count(participation) #count participants

## Manipulate f4s 
## Remove subjects who were not eligible (i.e., geen ok, afwijkende procedure, afwijkende indicatie)
## Q: more reasons that make people not suitable for target population?
## Q: what to do with patients <50 jaar thp?

View(f4s)
f4s <- f4s %>%
  filter(!grepl("afwijkend", `Reden exclusie`)) %>% #remove patients with afwijkende procedure / indicatie
  filter(!grepl("geen operatie", `Reden exclusie`)) #remove patients without surgery

## Add variables inclusion and participation (in this df, inclusion and participation should be equal)
f4s <- f4s %>%
  mutate(inclusion = ifelse(`F4S definitieve deelname` == "nee", "nee", "ja")) %>%
  mutate(participation = ifelse(`F4S definitieve deelname` == "ja", "ja", "nee")) #check missings and ?

## Count f4s file
## note: control / intervention to be added
f4s %>%
  count() #Count study eligibility

f4s %>%
  count(inclusion) #count inclusions

f4s %>%
  count(inclusion, Zorgpad) #count inclusions per zorgpad

f4s %>%
  count(participation)

## Manipulate screening_f4s
## check whether patients with "te korte tijd" belonged to control or intervention
screening_check <- read_excel("Z:\\Data PREHAB trial\\Totaaloverzicht F4S.xlsx", sheet = "screening log check")
screening_f4s$MDN <- as.numeric(screening_f4s$MDN)
f4s$MDN <- as.numeric(f4s$MDN)

## Only keep participants present in screening_f4s
View(screening_f4s)
View(screening_check)
screening_f4s <- left_join(screening_f4s, screening_check, by = "MDN")

## If groep = controle, dan te snel ok does not have to be excluded
screening_mdn <- screening_f4s %>%
  filter(Groep == "controle") %>%
  filter(!is.na(Bijzonderheden)) %>%
  filter(Inclusie == "nee") %>%
  filter(!grepl("50", `Reden van exclusie`)) %>%
  filter(!grepl("andere studie", `Reden van exclusie`)) %>%
  filter(!grepl("taal", `Reden van exclusie`)) %>%
  filter(!grepl("contra", `Reden van exclusie`)) %>%
  filter(!grepl("asa", `Reden van exclusie`)) %>%
  filter(!grepl("beperking", `Reden van exclusie`)) %>%
  filter(!grepl("psychiatrisch", `Reden van exclusie`)) %>%
  filter(MDN != 1736385 | MDN != 8212088 |
           MDN != 2103008 | MDN != 1780899 |
           MDN != 1788850 | MDN != 2401678) %>%
  pull(MDN) ##pull mdn of control participants -> change inclusie nee, check MDN 1788850

excl <- c(1736385, 8212088, 2103008, 1780899, 1788850, 2401678)
View(screening_mdn)
View(screening_f4s)

## Change inclusie to no for screening_mdn subjects
## to do: check whether these patients were actually intervention patients! if so, inclusion = nee
screening_f4s$Inclusie[screening_f4s$MDN %in% screening_mdn] <- NA
screening_f4s$Bijzonderheden[screening_f4s$MDN %in% screening_mdn] <- NA

## Check overlap in MDN numbers between f4s and screening_f4s
test2 <- inner_join(f4s, screening_f4s, by = "MDN") #delete 1676595 from excel
View(test2)
