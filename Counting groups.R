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
f4s <- f4s %>%
  filter(!grepl("afwijkend", `Reden exclusie`)) %>% #remove patients with afwijkende procedure / indicatie
  filter(!grepl("geen operatie", `Reden exclusie`)) #remove patients without surgery

## Add variables inclusion and participation (in this df, inclusion and participation should be equal if "definitieve deelname" equals yes or no)
f4s <- f4s %>%
  mutate(inclusion = ifelse(`F4S definitieve deelname` == "nee", "no", "yes")) %>%
  mutate(participation = ifelse(`F4S definitieve deelname` == "ja", "yes", "no")) %>%
  mutate(id = as.integer(id))

## add group and intentio to treat variables to f4s file
source("C:\\Users\\Elke\\Documents\\R\\Fit4Surgery\\Cleaning\\Source cleaning and codebook.R") #castor data from 20-12-2023
f4s_group <- full_data %>%
  select(id, group, intention_to_treat, deviation_intention_to_treat_primary, reason_deviation_intention_to_treat)

f4s_test <- semi_join(f4s_group, f4s, by = "id") # only keep id numbers that are present in f4s sheet
f4s <- full_join(f4s, f4s_test, by = "id") # full join so that f4s contains group variable

f4s <- mutate_all(f4s, .funs = tolower) #all cases to lower

f4s <- f4s %>%
  mutate(MDN = as.numeric(MDN)) #MDN as numeric

## allocate everyone to correct group 
table(is.na(f4s$deviation_intention_to_treat_primary))

f4s <- f4s %>%
  mutate(group = as.factor(case_when(
    is.na(deviation_intention_to_treat_primary) & 
      group == "intervention" ~ "intervention",
    is.na(deviation_intention_to_treat_primary) &
      group == "control" ~ "control",
    deviation_intention_to_treat_primary == "yes" ~ "intervention",
    .default = as.character(group) # change control to intervention if deviation = true 
    # check patients with deviation = TRUE but group = control !!
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

df <- full_join(df1, screening, by = c("MDN", "Zorgpad", "group", "inclusion", "participation")) ## add group variable in f4s dataframe (join with castor)

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
    evar_open = c("aaa evar", "evar", "fevar", "aaa", "aaa open"), # is dit 1 zorgpad?
    borstreconstructie = c("autologe borstreconstructie",
                                    "autologe mammareconstructie",
                                    "autologe borstreconstructie (ok 12-4-2022)",
                                    "autologe borstreconstructie (ok 31-8-2021)"),
    colon = "colon",
    cystectomie = c("cystectomy", "cystectomie"),
    endometrium = "endometrium",
    gist = "gist", # is dit een zorgpad?
    hipec_sarcoma = c("hipec", "retroperitoneaal sarcoom", "retroperitoneaal",
                         "sarcoom"),
    lever = c("lever", "liver"),
    meningeoom = c("meningeoom", "meningeoom\r\n"),
    nefrectomie = c("nefrectomie", "nefrectomy"),
    oesofagus = c("oesofagus", "oesophagus"),
    ovarium = "ovarium",
    pancreas = "pancreas",
    rectum = "rectum",
    thp_primair = c("thp primair", "thp primary", "thp primair 11-06-2021",
                    "thp primair 20-12-2021"),
    thp_revisie = c("thp revisie", "thp revisie (ok 14-1-22)",
                    "thp revisie (ok 25-5-22)", "thp revisie (ok 27-8-21)"),
    tkp_revisie = "tkp revisie",
    tle = "tle", # is dit een zorgpad?
    vrije_lap = c("vascularized free muscle flap", "vrije lap"),
    vulva = c("vulva", "vulva (15-6-22)", "vulva (25-8-22)", "vulva (7-4-22)")
  )) # nb: zorgpaden gespecificeerd in protocol zijn niet hetzelfde!

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
# NB: low participation rate borstreconstructie (missed?? reasons??)


