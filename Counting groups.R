## Author: Elke van Daal
## Project: Fit4Surgery PREHAB trial - Counting groups
## Start date: 5-12-2023

## load packages
library(tidyverse)
library(haven)
library(readxl)
library(xlsx)
library(gt)

## Read excel files
rm(list = ls())
f4s <- read_excel("Z:\\Data PREHAB trial\\Totaaloverzicht F4S.xlsx", sheet = "F4S inclusies") #data F4S participants
excel <- read_excel("Z:\\Data PREHAB trial\\Totaaloverzicht F4S.xlsx", sheet = "Excel log")
screening <- read_excel("Z:\\Data PREHAB trial\\Totaaloverzicht F4S.xlsx", sheet = "Screening 6-12")

## nOte: screening_f4s, patients without OK have been removed, 
## patients who participated (without remarks) have been removed
## all other patients have to be inclusions?? Or do we have to check EPIC?
## check patients with te korte tijd, controls can remain included,
## intervention patients have to be excluded
## then, all patients with bijzonderheden = "excluderen!" should be excluded from f4s file (inclusion variable = no)

## Change all cases to lower and manipulate few variables
f4s <- f4s %>% 
  rename(id = F4S_ID) %>%
  mutate(id = as.integer(str_remove(id, 'F4S_')))

f4s <- mutate_all(f4s, .funs = tolower)
excel <- mutate_all(excel, .funs = tolower)
screening <- mutate_all(screening, .funs = tolower)

screening <- screening %>%
  rename(inclusion = Inclusion,
         participation = Participation,
         group = Groep)

excel <- excel %>%
  rename(group = Groep)

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
  mutate(inclusion = ifelse(`Exclusie deelname (ja/nee)` == "ja","no", "yes")) %>%
  mutate(participation = "no") #none of these patients participated

## Manipulate f4s 
## Remove subjects who were not eligible (i.e., geen ok, afwijkende procedure, afwijkende indicatie)
## Q: more reasons that make people not suitable for target population?
## Q: what to do with patients <50 jaar thp?

f4s <- f4s %>%
  filter(!grepl("afwijkend", `Reden exclusie`)) %>% #remove patients with afwijkende procedure / indicatie
  filter(!grepl("geen operatie", `Reden exclusie`)) #remove patients without surgery

## Add variables inclusion and participation (in this df, inclusion and participation should be equal if "definitieve deelname" equals yes or no)
f4s <- f4s %>%
  mutate(inclusion = ifelse(`F4S definitieve deelname` == "nee", "no", "yes")) %>%
  mutate(participation = ifelse(`F4S definitieve deelname` == "ja", "yes", "no")) %>%
  mutate(id = as.integer(id))

## add group to f4s file
source("C:\\Users\\Elke\\Documents\\R\\Fit4Surgery\\Cleaning\\Source cleaning and codebook.R") #castor data from 20-12-2023
f4s_group <- full_data %>%
  select(id, group)

f4s_test <- semi_join(f4s_group, f4s, by = "id") # only keep id numbers that are present in f4s sheet
f4s <- full_join(f4s, f4s_test, by = "id") # full join so that f4s contains group variable

## Full join excel, screening and f4s
df1 <- full_join(excel, f4s, by = c("MDN", "Zorgpad", "group", "inclusion", "participation"))

df <- full_join(df1, screening, by = c("MDN", "Zorgpad", "group", "inclusion", "participation")) ## add group variable in f4s dataframe (join with castor)

df_counts <- df %>%
  select(MDN, Zorgpad, group, inclusion, participation)

df_counts <- mutate_all(df_counts, .funs = tolower)
df_counts <- df_counts %>%
  mutate(group = as.factor(group))

df_counts <- df_counts %>%
  mutate(group = fct_collapse(group,
    control = c("control", "controle"),
    intervention = c("intervention", "interventie")
  ))

df_counts <- df_counts %>%
  mutate(inclusion = fct_collapse(inclusion,
      yes = c("ja", "yes"),
      no = c("no", "nee")
  ))

df_counts <- df_counts %>%
  mutate(participation = fct_collapse(participation,
     yes = c("ja", "yes"),
     no = c("no", "nee")
  ))
## count
not_eligible <- df_counts %>%
  filter(inclusion == "no")

df_counts <- df_counts %>%
  filter(inclusion == "yes") %>%
  filter(participation == "yes" | participation == "no")

df_counts %>%
  count(group)  ## check NA's 

df_counts %>%
  count(participation)

df_counts %>%
  count(group, participation) %>%
  gt()

df_counts %>%
  count(group, participation, Zorgpad) %>%
  gt() ## check names of zorgpaden


