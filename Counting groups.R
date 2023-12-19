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
screening <- read_excel("Z:\\Data PREHAB trial\\Totaaloverzicht F4S.xlsx", sheet = "Screening 6-12")

## nOte: screening_f4s, patients without OK have been removed, 
## patients who participated (without remarks) have been removed
## all other patients have to be inclusions?? Or do we have to check EPIC?
## check patients with te korte tijd, controls can remain included,
## intervention patients have to be excluded
## then, all patients with bijzonderheden = "excluderen!" should be excluded from f4s file (inclusion variable = no)

## Change all cases to lower
f4s <- mutate_all(f4s, .funs = tolower)
excel <- mutate_all(excel, .funs = tolower)
screening <- mutate_all(screening, .funs = tolower)

screening <- screening %>%
  rename(inclusion = Inclusion,
         participation = Participation,
         group = Groep)

excel <- screening %>%
  rename(Groep = group,
         surgery_type = Zorgpad)


## Change MDN to numeric 
f4s <- f4s %>%
  mutate(MDN = as.numeric(MDN))
excel <- excel %>%
  mutate(MDN = as.numeric(MDN))
screening <- screening %>%
  mutate(MDN = as.numeric(MDN))

## Check whether there is no overlap in MDN numbers between f4s and excel / screening
test <- inner_join(f4s, excel, by = "MDN") #delete 1676595 and 8959699 from excel
test2 <- inner_join(f4s, screening, by = "MDN") 

## Manipulate excel log 
## Remove subjects who were not eligible
excel <- excel %>%
  filter(MDN != 1676595 | MDN != 8959699) %>% #delete 1676595 from excel (is already in f4s df)
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
                    
excel %>%
  count(inclusion, `Groep (controle/interventie)`) #count inclusions by group (many controls here!)
                                                  #check groep = ja / NA (MDN 6545594, 9688924)

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

## Add variables inclusion and participation (in this df, inclusion and participation should be equal if "definitieve deelname" equals yes or no)
f4s <- f4s %>%
  mutate(inclusion = ifelse(`F4S definitieve deelname` == "nee", "nee", "ja")) %>%
  mutate(participation = ifelse(`F4S definitieve deelname` == "ja", "ja", "nee")) 

## Count f4s file
## note: control / intervention to be added --> castor
f4s %>%
  count() #Count study eligibility

f4s %>%
  count(inclusion) #count inclusions

f4s %>%
  count(inclusion, Zorgpad) #count inclusions per zorgpad

f4s %>%
  count(participation)

## Manipulate screening_f4s
screening %>%
  count(inclusion)

screening %>%
  count(participation)

## Full join excel, screening and f4s
df1 <- full_join(excel, f4s, by = c("MDN", "Zorgpad", "inclusion", "participation"))

df <- full_join(df1, screening, by = c("MDN", "Zorgpad", "inclusion", "participation")) ## add group variable in f4s dataframe (join with castor)

df_counts <- df %>%
  select(MDN, Zorgpad, group, inclusion, participation)
