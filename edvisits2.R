library(haven)
library(dplyr)
library(lubridate)
library(tidyverse)

setwd("Y:/patels")

#  data

# ed_prior  <- read_sas("Y:/hill/output/heat_urban/ed_visits_clms_prior_1year.sas7bdat")
# ed_post   <- read_sas("Y:/hill/output/heat_urban/ed_visits_clms_post_1year.sas7bdat")
# mothers   <- read_sas("Y:/hill/output/heat_urban/all_mothers_phila_2016_19.sas7bdat")
# 
# save(ed_post,file='Data/ed_claims_post.Rdata')
# save(ed_prior,file='Data/ed_claims_prior.Rdata')
# save(mothers,file='Data/mothers.Rdata')

load("Data/ed_claims_prior.Rdata")
load("Data/ed_claims_post.Rdata")
load("Data/mothers.Rdata")

# pregnant ed visits
#restrict to <40 weeks, summer months

ed_prior <- ed_prior %>%
  mutate(weeks_before_delivery = as.numeric((delivery_date - claim_date) / 7)) %>%
  filter(weeks_before_delivery < 42, #42 weeks 
         month(claim_date) %in% 5:9,
         year(claim_date) %in% 2016:2019) %>%
  mutate(preg = 1)


#postpartum visits 
#restric to summer months
ed_post <- ed_post %>%
  filter(month(claim_date) %in% 5:9,
         year(claim_date) %in% 2016:2019) %>%
           mutate(wksafterdelivery = as.numeric((claim_date - delivery_date) / 7),
         postpartum = 1)


# merge both
edpregpp_all <- bind_rows(ed_prior, ed_post) %>%
  mutate(claim_id = paste0(id, "_", as.Date(claim_date)), #creates a unique claim id 
         dateonly = claim_date)

# Unique visits (1 visit per person)
edpregpp <- edpregpp_all %>%
  distinct(id, claim_date, .keep_all = TRUE)

# restrict to philly zips
idzip <- mothers %>%
  select(id, zip5) %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(grepl("^191", zip5)) # only Philadelphia

edpregpp     <- inner_join(edpregpp,     idzip, by = "id") # n=51 removed
edpregpp_all <- inner_join(edpregpp_all, idzip, by = "id")


# flag for recurrent visits within 7 days
edpregpp <- edpregpp %>%
  arrange(id, claim_date) %>%
  group_by(id) %>%
  mutate(recurrent7 = if_else(
    !is.na(lead(claim_date)) & (as.numeric(lead(claim_date) - claim_date) <= 7),
    1, 0)) %>%
  ungroup()

edpregpp = edpregpp  %>%
  select(claim_id, claim_date, preg, postpartum, DGNS_CD_1, DGNS_CD_2, weeks_before_delivery, wksafterdelivery, recurrent7)

save(edpregpp, file='Data/edpregpp.Rdata')


#########################################
#########################################
#########################################
#########################################
#########################################

edpregpp_all = edpregpp_all  %>%
  select(claim_id, claim_date, preg, postpartum, DGNS_CD_1, DGNS_CD_2, weeks_before_delivery, wksafterdelivery)

######################

edpregpp_all <- edpregpp_all %>%
  mutate(chapter = substr(DGNS_CD_1, 1, 1), chapter2=substr(DGNS_CD_2, 1, 1))
table(edpregpp_all$chapter)
table(edpregpp_all$chapter2)



edpregpp_all <- edpregpp_all %>%
  mutate(infection = ifelse(chapter == "A"| chapter=="B", 1, 0), 
         blood=ifelse(chapter == "D", 1, 0),
         endo=ifelse(chapter == "E", 1, 0), #endocrine
         mental=ifelse(chapter == "F", 1, 0), #mental
         brain=ifelse(chapter == "G", 1, 0), #nervous system
         eyeear=ifelse(chapter == "H", 1, 0), #eye&ear
         heart=ifelse(chapter == "I", 1, 0), #heart
         lungs=ifelse(chapter == "J", 1, 0), #lungs
         digestive=ifelse(chapter == "K", 1, 0), #digestive
         skin=ifelse(chapter == "L", 1, 0), #skin
         musc=ifelse(chapter == "M", 1, 0), #muscoskeletal
         genit=ifelse(chapter == "N", 1, 0), #genitourinary
         pregrel= ifelse(chapter == "O"| chapter=="P", 1, 0), #pregnancy and perinatal
         injury=ifelse(chapter == "S"| chapter=="T"|chapter == "V"| chapter=="W" | chapter=="X"| chapter=="Y", 1, 0)) #injury & external causes


#infection

infection=edpregpp_all %>% group_by(
  claim_id,claim_date
) %>% summarise(infection=sum(infection)) %>% arrange(desc(infection)) %>%
  mutate(infection=ifelse(infection>=1, 1, 0)) %>%
  filter(infection==1) #n=2097


# n=841; too small
# blood=edpregpp_all %>% group_by(
#   claim_id,claim_date
# ) %>% summarise(blood=sum(blood)) %>% arrange(desc(blood)) %>%
#   mutate(blood=ifelse(blood>=1, 1, 0)) %>%
#   filter(blood==1) #n=2097

# #endocrine related causes
# endo=edpregpp_all %>% group_by(
#   claim_id, claim_date) %>% summarise(endo=sum(endo)) %>% arrange(desc(endo)) %>%
#   mutate(endo=ifelse(endo>=1, 1, 0)) %>%
#   filter(endo==1) #n=898



#mental health related causes
mental=edpregpp_all %>% group_by(
  claim_id,claim_date) %>% summarise(mental=sum(mental)) %>% arrange(desc(mental)) %>%
  mutate(mental=ifelse(mental>=1, 1, 0)) %>%
  filter(mental==1) #n=2188

# 
# #nervous system related causes; too small
# brain=edpregpp_all %>% group_by(
#   claim_id,claim_date) %>% summarise(brain=sum(brain)) %>% arrange(desc(brain)) %>%
#   mutate(brain=ifelse(brain>=1, 1, 0)) %>%
#   filter(brain==1) #n=1068

# #circulatory related causes; probably too small - n=344
# cir=edpregpp_all %>% group_by(
#   claim_id,claim_date) %>% summarise(cir=sum(heart)) %>% arrange(desc(cir)) %>%
#   mutate(endo=ifelse(cir>=1, 1, 0)) %>%
#   filter(cir==1) #n=344


#respiratory related causes
resp=edpregpp_all %>% group_by(
  claim_id,claim_date) %>% summarise(resp=sum(lungs)) %>% arrange(desc(resp)) %>%
  mutate(resp=ifelse(resp>=1, 1, 0)) %>%
  filter(resp==1) #n=3509

#digestive related causes
digestive=edpregpp_all %>% group_by(
  claim_id,claim_date) %>% summarise(digestive=sum(digestive)) %>% arrange(desc(digestive)) %>%
  mutate(digestive=ifelse(digestive>=1, 1, 0)) %>%
  filter(digestive==1) #n=3206


#genitourinary related causes
genit=edpregpp_all %>% group_by(
  claim_id,claim_date) %>% summarise(genit=sum(genit)) %>% arrange(desc(genit)) %>%
  mutate(genit=ifelse(genit>=1, 1, 0)) %>%
  filter(genit==1) #n=6350

# 
# #skin
# skin=edpregpp_all %>% group_by(
#   claim_id,claim_date
# ) %>% summarise(skin=sum(skin)) %>% arrange(desc(skin)) %>%
#   mutate(skin=ifelse(skin>=1, 1, 0)) %>%
#   filter(skin==1) #n=1645


#muscoskeletal
musc=edpregpp_all %>% group_by(
  claim_id,claim_date
) %>% summarise(musc=sum(musc)) %>% arrange(desc(musc)) %>%
  mutate(musc=ifelse(musc>=1, 1, 0)) %>%
  filter(musc==1) #n=5220



#pregnancy related causes
preg=edpregpp_all %>% group_by(
  claim_id,claim_date
   ) %>% summarise(pregc=sum(pregrel)) %>% arrange(desc(pregc)) %>%
  mutate(pregc=ifelse(pregc>=1, 1, 0)) %>%
filter(pregc==1) #n=25677



#injury related causes
injury=edpregpp_all %>% group_by(
  claim_id,claim_date) %>% summarise(injury=sum(injury)) %>% arrange(desc(injury)) %>%
  mutate(injury=ifelse(injury>=1, 1, 0)) %>%
  filter(injury==1)  #n=4302



save(edpregpp_all, file='Data/edpregpp_all.Rdata')





