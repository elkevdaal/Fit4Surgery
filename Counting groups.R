## Author: Elke van Daal
## Project: Fit4Surgery PREHAB trial - Counting groups
## Start date: 5-12-2023
## Last update: 30-01-2024

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

## general manipulations
f4s <- mutate_all(f4s, .funs = tolower)
excel <- mutate_all(excel, .funs = tolower)
screening <- mutate_all(screening, .funs = tolower)

f4s <- f4s %>% 
  rename(id = F4S_ID) %>%
  mutate(id = as.integer(str_remove(id, 'f4s_')))

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
test <- inner_join(f4s, excel, by = "MDN") 
test2 <- inner_join(f4s, screening, by = "MDN") 

# Manipulate excel log 
## Add variables inclusion and participation
excel <- excel %>%
  mutate(inclusion = ifelse(`Exclusie deelname (ja/nee)` == "ja","no", "yes")) %>%
  mutate(participation = "no") #none of these patients participated

# Manipulate f4s 
## Remove subjects who were not eligible (i.e., geen ok, afwijkende procedure, afwijkende indicatie)
f4s <- f4s %>%
  filter(!grepl("afwijkend", `Reden exclusie`)) %>% #remove patients with afwijkende procedure / indicatie
  filter(!grepl("geen operatie", `Reden exclusie`)) %>% #remove patients without surgery
  filter(!grepl("open-dicht", `Reden exclusie`))

## Add variables inclusion and participation (in this df, inclusion and participation should be equal if "definitieve deelname" equals yes or no)
f4s <- f4s %>%
  mutate(inclusion = ifelse(`F4S definitieve deelname` == "nee", "no", "yes")) %>%
  mutate(participation = ifelse(`F4S definitieve deelname` == "ja", "yes", "no")) %>%
  mutate(id = as.integer(id))

## add group and intention to treat variables to f4s file
source("C:\\Users\\Elke\\Documents\\R\\Fit4Surgery\\Cleaning\\Source cleaning and codebook.R") #castor data from 20-12-2023
f4s_group <- full_data %>%
  select(id, group, intention_to_treat, deviation_intention_to_treat_primary, reason_deviation_intention_to_treat)

f4s_test <- semi_join(f4s_group, f4s, by = "id") # only keep id numbers that are present in f4s sheet
f4s <- full_join(f4s, f4s_test, by = "id") # full join so that f4s contains group variable

f4s <- mutate_all(f4s, .funs = tolower) #all cases to lower

f4s <- f4s %>%
  mutate(MDN = as.numeric(MDN)) #MDN as numeric

f4s %>%
  filter(is.na(deviation_intention_to_treat_primary)) %>%
  pull(id) # check which patients have missing data for ITT variable
# "234"  "497"  "751"  "856"  "1033" "1131" "1134" "1668"

f4s %>%
  filter(deviation_intention_to_treat_primary == "yes" & intention_to_treat == "control") %>%
  pull(id) # check patients with deviation = yes but group = control 
# 1963

## allocate everyone to correct group 
f4s <- f4s %>%
  mutate(group = as.factor(case_when(
    is.na(deviation_intention_to_treat_primary) & 
      group == "intervention" ~ "intervention",
    is.na(deviation_intention_to_treat_primary) &
      group == "control" ~ "control",
    deviation_intention_to_treat_primary == "yes" ~ "intervention",
    .default = as.character(group) # change control to intervention if deviation = yes
  ))) %>%
  mutate(inclusion = as.factor(case_when(
    inclusion == "yes" & 
      reason_deviation_intention_to_treat == "surgery soon" ~ "no",
    inclusion == "yes" & 
      reason_deviation_intention_to_treat == "contra intervention" ~ "no",
    .default = as.character(inclusion)
  ))) #change inclusion from yes to no (not sure about illness and not feasible)

## Full join excel, screening and f4s
df1 <- full_join(excel, f4s, by = c("MDN", "Zorgpad", "group", "inclusion", "participation"))

df <- full_join(df1, screening, by = c("MDN", "Zorgpad", "group", "inclusion", "participation"))

## Manipulate df_counts
## Create factors for group and Zorgpad
df_counts <- df %>%
  select(MDN, Zorgpad, group, inclusion, participation)

df_counts <- mutate_all(df_counts, .funs = tolower)
df_counts <- df_counts %>%
  mutate(group = as.factor(group)) %>%
  mutate(Zorgpad = as.factor(Zorgpad))

df_counts <- df_counts %>%
  mutate(group = fct_collapse(group,
    control = c("control", "controle"),
    intervention = c("intervention", "interventie")
  ))

df_counts <- df_counts %>%
  mutate(Zorgpad = fct_collapse(Zorgpad, 
    aaa_open = c("aaa", "aaa open"),                           
    aaa_evar = c("aaa evar", "evar", "fevar"), 
    auto_breast_reconstruction = c("autologe borstreconstructie",
                                    "autologe mammareconstructie",
                                    "autologe borstreconstructie (ok 12-4-2022)",
                                    "autologe borstreconstructie (ok 31-8-2021)"),
    colon_cancer = "colon",
    bladder_cancer = c("cystectomy", "cystectomie"),
    endometrial_cancer = "endometrium",
    retroperitoneal_malignancies = c("hipec", "retroperitoneaal sarcoom", "retroperitoneaal",
                         "sarcoom", "gist"), # gist hier weghalen?
    liver_cancer = c("lever", "liver"),
    supratentorial_meningeoma = c("meningeoom", "meningeoom\r\n"),
    renal_cancer = c("nefrectomie", "nefrectomy"),
    esophageal_cancer = c("oesofagus", "oesophagus"),
    ovarian_cancer = "ovarium",
    pancreaticobiliary_cancer = "pancreas",
    rectal_cancer = "rectum",
    hip_arthrosis = c("thp primair", "thp primary", "thp primair 11-06-2021",
                    "thp primair 20-12-2021"),
    hip_arthroplasty_failure = c("thp revisie", "thp revisie (ok 14-1-22)",
                    "thp revisie (ok 25-5-22)", "thp revisie (ok 27-8-21)"),
    knee_arthroplasty_failure = "tkp revisie",
    laryngeal_cancer = "tle", 
    oral_cancer = c("vascularized free muscle flap", "vrije lap"),
    vulvar_field_resection = c("vulva", "vulva (15-6-22)", "vulva (25-8-22)", "vulva (7-4-22)")
  )) 

table(df_counts$Zorgpad)
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

## count df_counts 
not_eligible <- df_counts %>%
  filter(inclusion == "no") # vectorize non eligible patients

df_counts <- df_counts %>%
  filter(inclusion == "yes") %>%
  filter(participation == "yes" | participation == "no") # remove non eligible patients

df_counts %>%
  count() # count inclusions 

df_counts %>% # count inclusions per group
  count(group) %>%
  gt()

df_counts %>%
  count(participation) %>%
  gt() # count participants 

df_counts %>%
  count(group, participation) %>%
  gt() # count participation per group

df_counts %>%
  count(Zorgpad) # count inclusions per zorgpad

df_counts %>% # count inclusions per zorgpad and group (most interesting)
  count(group, Zorgpad) %>%
  gt()

 df_counts %>%
  count(group, participation, Zorgpad) %>%
  gt() # count participation per group and zorgpad

## check participation within intervention group
df_counts %>%
  filter(group == "intervention") %>%
  count(participation) #overall participation from all eligible (included) patients
  
df_counts %>%
  filter(group == "intervention") %>%
  count(Zorgpad, participation) %>%
  gt() #count participation per zorgpad 



