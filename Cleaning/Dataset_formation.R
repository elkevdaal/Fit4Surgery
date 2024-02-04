###### Author: Elke van Daal ######################################
###### Project: Fit4Surgery PREHAB trial - create one dataset #####
###### Start date: 04-02-2024 #####################################

# load packages
library(tidyverse)
library(haven)
library(readxl)
library(xlsx)
library(gt)

# Read excel files
rm(list = ls())
f4s <- read_excel("Z:\\Data PREHAB trial\\Totaaloverzicht F4S.xlsx", sheet = "F4S inclusies") #data F4S participants
excel <- read_excel("Z:\\Data PREHAB trial\\Totaaloverzicht F4S.xlsx", sheet = "Excel log")
screening <- read_excel("Z:\\Data PREHAB trial\\Totaaloverzicht F4S.xlsx", sheet = "Screening 6-12")

################# general manipulations ###############################
## all rows to lower cases 
f4s <- mutate_all(f4s, .funs = tolower)
excel <- mutate_all(excel, .funs = tolower)
screening <- mutate_all(screening, .funs = tolower)

## remove F4S in ID number
f4s <- f4s %>% 
  rename(study_id = F4S_ID) %>%
  mutate(study_id = as.integer(str_remove(study_id, 'f4s_')))

## rename variables 
screening <- screening %>%
  rename(inclusion = Inclusion,
         informed_consent = Participation,
         group = Groep)

excel <- excel %>%
  rename(group = Groep)

## Change MDN and study_id to numeric 
f4s <- f4s %>%
  mutate(MDN = as.numeric(MDN))
excel <- excel %>%
  mutate(MDN = as.numeric(MDN))
screening <- screening %>%
  mutate(MDN = as.numeric(MDN))

f4s <- f4s %>%
  mutate(study_id = as.numeric(study_id))
excel <- excel %>%
  mutate(study_id = as.numeric(study_id))
screening <- screening %>%
  mutate(study_id = as.numeric(study_id))

## Change group variable to factor and collapse
screening <- screening %>% 
  mutate(group = fct_collapse(as.factor(group),
                              control = c("control", "controle"),
                              intervention = c("intervention", "interventie"))
  )

excel <- excel %>% 
  mutate(group = fct_collapse(as.factor(group),
                              control = c("control", "controle"),
                              intervention = c("intervention", "interventie"))
  )
# Check whether there is no overlap in MDN numbers between f4s and excel / screening
test <- inner_join(f4s, excel, by = "MDN") 
test2 <- inner_join(f4s, screening, by = "MDN") 



#################################### excel data ####################################################
## Add variables inclusion, informed_consent, adherence, study_cohort
excel <- excel %>%
  mutate(inclusion = ifelse(`Exclusie deelname (ja/nee)` == "ja","no", "yes")) %>%
  mutate(informed_consent = "no",
         study_cohort = "prospective") %>% #none of these patients provided IC, all these patients were included prospectively
  mutate(adherence = as.factor(case_when(
    group == "control" ~ "nvt",
    group == "intervention" ~ "no" ))) #no intervention patients adhered to F4S in this list



######################################### screening data #########################################3
## Add variables adherence, study_cohort
screening <- screening %>%
  mutate(study_cohort = "prospective") %>% #all these patients were included prospectively
  mutate(adherence = as.factor(case_when(
    group == "control" ~ "nvt",
    group == "intervention" ~ "no" ))) #no intervention patients adhered to F4S in this list



######################################### f4s data ##############################################
## Remove subjects who were not eligible (i.e., geen ok, afwijkende procedure, afwijkende indicatie)
f4s <- f4s %>%
  filter(!grepl("afwijkend", `Reden exclusie`)) %>%
  filter(!grepl("geen operatie", `Reden exclusie`)) %>% 
  filter(!grepl("open-dicht", `Reden exclusie`))

## Add variables inclusion, informed_consent, study_cohort
f4s <- f4s %>%
  mutate(inclusion = ifelse(`F4S definitieve deelname` == "nee", "no", "yes")) %>%
  mutate(informed_consent = ifelse(`F4S definitieve deelname` == "ja", "yes", "no")) %>%
  mutate(study_cohort = "prospective") #all these patients were included prospectively

## add group and intention to treat variables to f4s file
source("C:\\Users\\Elke\\Documents\\R\\Fit4Surgery\\Cleaning\\Source cleaning and codebook.R") #castor data from 20-12-2023
f4s_group <- full_data %>%
  select(id, group, intention_to_treat, deviation_intention_to_treat_primary, reason_deviation_intention_to_treat) %>%
  rename(study_id = id)

f4s_test <- semi_join(f4s_group, f4s, by = "study_id") # only keep id numbers that are present in f4s sheet
f4s <- full_join(f4s, f4s_test, by = "study_id") # full join so that f4s contains group variable

f4s <- mutate_all(f4s, .funs = tolower) #all cases to lower

f4s <- f4s %>%
  mutate(MDN = as.numeric(MDN),
         study_id = as.numeric(study_id)) #MDN and study_ID as numeric

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
  ))) 

## add variable adherence
f4s <- f4s %>% mutate(adherence = as.factor(case_when(
    group == "control" ~ "nvt",
    group == "intervention" ~ "yes" )))



####################################### Full join excel, screening and f4s ########################################
df <- full_join(excel, f4s, by = c("MDN", "study_id", "Zorgpad", "group", "inclusion", "adherence", "informed_consent", "study_cohort"))

df_pros <- full_join(df, screening, by = c("MDN", "study_id", "group", "Zorgpad", "inclusion", "adherence", "informed_consent", "study_cohort")) 


####################################### df_pros #######################################################
# select necessary variables
df_pros <- df_pros %>%
  select(MDN, study_id, group, Zorgpad, inclusion, adherence, informed_consent, study_cohort)

# all rows to lower cases
df_pros <- mutate_all(df_pros, .funs = tolower)

# create factors for variable "Zorgpad" and rename to "indication"
df_pros <- df_pros %>%
  mutate(indication = as.factor(Zorgpad)) %>%
  mutate(indication = fct_collapse(indication, 
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
                                vulvar_field_resection = c("vulva", "vulva (15-6-22)", "vulva (25-8-22)", "vulva (7-4-22)"))) %>%
  select(-Zorgpad) #we use variable "indication" now

# use correct data type for all variables in df_pros
df_pros <- df_pros %>%
  mutate(MDN = as.numeric(MDN),
         study_id = as.numeric(study_id),
         group = as.factor(group),
         inclusion = as.factor(inclusion),
         adherence = as.factor(adherence),
         informed_consent = as.factor(informed_consent),
         study_cohort = as.factor(study_cohort))

# only keep inclusions
df_pros <- df_pros %>%
  filter(inclusion == "yes")

###############################################################################################

# organize global environment (only keep df_pros)
rm(df, excel, f4s, f4s_group, f4s_test, full_data, screening, test, test2)

### next steps: 
### 1.join df_pros with complications from spss (prospective cohort), by MDN ###
### 2. create dataset like df_pros, but then df_retro ###
### 3. join df_retro with complications from spss (retrospective cohort), by MDN ###
### 4. full join df_pros and df_retro, by MDN, study_id, group, inclusion, adherence, informed_consent, study_cohort, indication in to df ###
### 5. remove variable MDN from df ###
### 6. calculate clavien-dindo, CCI into pd (primary data) ###
### 6. join testdata from castor (- select group, zorgpad, other redundant variables?) with pd, by study_id into fd (full data)

