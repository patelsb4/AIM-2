library(haven)
library(dplyr)
library(lubridate)
library(tidyverse)

setwd("Y:/patels")

load("Data/ed_claims_post.Rdata")
load("Data/mothers.Rdata")

# keep only live + still birth deliveries 
mothers_live_sb <- mothers %>%
  filter(preg_live == 1 | preg_sb == 1)

# fix delivery date - if multiple dates within 90 days use the first one
deliveries_clean <- mothers_live_sb %>%
  arrange(id, delivery_date) %>%
  group_by(id) %>%
  mutate(
    days_since_prior  = as.numeric(delivery_date - lag(delivery_date)),
    new_preg       = if_else(is.na(days_since_prior) | days_since_prior > 90, 1, 0),
    preg_id        = cumsum(new_preg),
  ) %>%
  group_by(id, preg_id) %>%
  mutate(delivery_index_date = min(delivery_date)) %>%
  ungroup() %>%
  select(-days_since_prior, -new_preg, -preg_id)

# create pregnancy windows
deliveries_clean <- deliveries_clean %>%
  arrange(id, delivery_index_date) %>%
  group_by(id) %>%
  mutate(
    next_delivery   = lead(delivery_index_date),
    preg_start_next = next_delivery - 280 # conception date
  ) %>%
  ungroup()

# merge in claims data
ed_post_all <- ed_post %>%
  left_join(
    deliveries_clean %>%
      select(id, delivery_date, delivery_index_date,
             next_delivery, preg_start_next,
             preg_live, preg_sb, AGE, RACE_ETHNCTY_CD,
             zip5, INCM_CD),
    by = c("id", "delivery_date")
  )

# create pregnant variable for those who are pregnant + postpartum
# restrict to summer months and philly zipcodes
ed_post_all <- ed_post_all %>%
  mutate(
    daysafterdelivery = as.numeric(claim_date - delivery_index_date),
    pregnant = if_else(
      !is.na(next_delivery) &
        claim_date >= preg_start_next &
        claim_date <= next_delivery,
      1, 0
    ),
    case = 1
  ) %>%
  filter(
    month(claim_date) %in% 5:9,
    year(claim_date)  %in% 2016:2019,
    daysafterdelivery > 42,
    daysafterdelivery <= 365,
    grepl("^191", zip5) )

# final datasets

# unique ed visit days
ed_postpartum <- ed_post_all %>%
  distinct(id, claim_date, .keep_all = TRUE) %>%
  select(id, case, claim_date, delivery_date, delivery_index_date,
         pregnant, preg_live, preg_sb, DGNS_CD_1, DGNS_CD_2,
         daysafterdelivery, AGE, RACE_ETHNCTY_CD, INCM_CD, zip5) %>%
  arrange(id, delivery_index_date)


# cause specific

ed_post_all <- ed_post_all %>%
  mutate(
    # ICD ch letters
    ch1 = substr(DGNS_CD_1, 1, 1),
    ch2 = substr(DGNS_CD_2, 1, 1),
    # first 3 characters
    num1 = substr(DGNS_CD_1, 1, 3),
    num2 = substr(DGNS_CD_2, 1, 3),
    
    infection = ifelse(ch1 %in% c("A","B") | ch2 %in% c("A","B"), 1, 0),
    neoplasms = ifelse(ch1 %in% c("C") | ch2 %in% c("C"), 1, 0),
    blood     = ifelse(ch1 %in% c("D") | ch2 %in% c("D"), 1, 0),
    endo      = ifelse(ch1 %in% c("E") | ch2 %in% c("E"), 1, 0),
    mental    = ifelse(ch1 %in% c("F") | ch2 %in% c("F"), 1, 0),
    brain     = ifelse(ch1 %in% c("G") | ch2 %in% c("G"), 1, 0),
    eyeear    = ifelse(ch1 %in% c("H") | ch2 %in% c("H"), 1, 0),
    heart     = ifelse(ch1 %in% c("I") | ch2 %in% c("I"), 1, 0),
    lungs     = ifelse(ch1 %in% c("J") | ch2 %in% c("J"), 1, 0),
    digestive = ifelse(ch1 %in% c("K") | ch2 %in% c("K"), 1, 0),
    skin      = ifelse(ch1 %in% c("L") | ch2 %in% c("L"), 1, 0),
    musc      = ifelse(ch1 %in% c("M") | ch2 %in% c("M"), 1, 0),
    genit     = ifelse(ch1 %in% c("N") | ch2 %in% c("N"), 1, 0),
    obstetric = ifelse(ch1 %in% c("O") | ch2 %in% c("O") , 1, 0),
    symptoms  = ifelse(ch1 %in% c("R") | ch2 %in% c("R"), 1, 0), 
    injury    = ifelse(ch1 %in% c("S","T", "V","W","X","Y") |
                       ch2 %in% c("S","T","V","W","X","Y") |
                       (ch1 == "U" & num1 %in% 1:3) |
                       (ch2 == "U" & num2 %in% 1:3),1, 0))

  #remove non injury y codes
ed_post_all <- ed_post_all %>%
  mutate(injury =ifelse((ch1 == "Y" & num1 >= 39)  |
                            (ch2 == "Y" & num2 >=39), 0, injury))

extractcause <- function(data, var) {
  data %>%
    filter({{ var }} == 1) %>%
    distinct(id, claim_date, .keep_all = TRUE)
}

infection <- extractcause(ed_post_all, infection)
blood     <- extractcause(ed_post_all, blood)
endo      <- extractcause(ed_post_all, endo)
mental    <- extractcause(ed_post_all, mental)
brain     <- extractcause(ed_post_all, brain)
eyeear    <- extractcause(ed_post_all, eyeear)
heart     <- extractcause(ed_post_all, heart)
lungs     <- extractcause(ed_post_all, lungs)
digestive <- extractcause(ed_post_all, digestive)
skin      <- extractcause(ed_post_all, skin)
musc      <- extractcause(ed_post_all, musc)
genit     <- extractcause(ed_post_all, genit)
obst   <- extractcause(ed_post_all, obstetric) 
symptoms   <- extractcause(ed_post_all, symptoms)
injury    <- extractcause(ed_post_all, injury) 


# all visits for table 1
allvisits <- bind_rows(
   infection, blood, endo, brain, eyeear, heart, lungs, digestive, skin, musc, genit, symptoms, obst, mental, injury)

# live only
ed_postpartum_live <-  extractcause(ed_postpartum, preg_live)
mental_live        <- extractcause(mental, preg_live)
obst_live          <- extractcause(obst,   preg_live)
injury_live        <- extractcause(injury, preg_live)


# exclude concurrent pregnancy and postpartum period

ed_postpartum_pp= ed_postpartum  %>%
  filter(pregnant==0)
obst_pp= obst  %>%
  filter(pregnant==0)
mental_pp= mental  %>%
  filter(pregnant==0)
injury_pp= injury  %>%
  filter(pregnant==0)

save(ed_postpartum,      file = 'Data/ed_postpartum.Rdata')
save(ed_post_all,        file = 'Data/ed_post_all.Rdata')
save(mental,             file = 'Data/mental.Rdata')
save(mental_live,        file = 'Data/mental_live.Rdata')
save(obst,               file = 'Data/obst.Rdata')
save(obst_live,          file = 'Data/obst_live.Rdata')
save(injury,             file = 'Data/injury.Rdata')
save(injury_live,        file = 'Data/injury_live.Rdata')

#######################
#######################
#######################
#######################

#Table1


# vist level summary
n_total <- length(allvisits$case)

visit_summary <- tibble(
  Category          = c("All ED visits", "Mental health", "Obstetric", "Injury"),
  N_visits          = c(n_total,
                        length(mental$case),
                        length(obst$case),
                        length(injury$case)),
  Pct_of_all        = c(n_total/ n_total,
                        length(mental$case) / n_total,
                        length(obst$case)   / n_total,
                        length(injury$case) / n_total)
) %>%
  mutate(Pct_of_all = scales::percent(Pct_of_all, accuracy = 0.1))

# Unique participants (reported inline; add to table if desired)
n_unique_ids <- ed_postpartum %>% distinct(id) %>% nrow()  # 11,821

print(visit_summary)

# temp thresholds
probs <- c(0.90, 0.95, 0.99)

temp_thresholds <- tibble(
  Percentile = c("90th", "95th", "99th"),
  T_min      = quantile(daily.temp.all$temp.min,  probs, na.rm = TRUE),
  T_mean     = quantile(daily.temp.all$temp.mean, probs, na.rm = TRUE),
  T_max      = quantile(daily.temp.all$temp.max,  probs, na.rm = TRUE)
)

tmin_pctiles  <- setNames(temp_thresholds$T_min,  c("p90","p95","p99"))
tmean_pctiles <- setNames(temp_thresholds$T_mean, c("p90","p95","p99"))
tmax_pctiles  <- setNames(temp_thresholds$T_max,  c("p90","p95","p99"))

print(temp_thresholds)

# counts of events at extreme heat
heatcounts <- function(data, label) {
  data$dateonly <- data$claim_date
  
  daily.temp.all %>%
    left_join(data, by = "dateonly") %>%
    mutate(
      tmin_90  = temp.min  >= tmin_pctiles["p90"],
      tmin_95  = temp.min  >= tmin_pctiles["p95"],
      tmin_99  = temp.min  >= tmin_pctiles["p99"],
      tmean_90 = temp.mean >= tmean_pctiles["p90"],
      tmean_95 = temp.mean >= tmean_pctiles["p95"],
      tmean_99 = temp.mean >= tmean_pctiles["p99"],
      tmax_90  = temp.max  >= tmax_pctiles["p90"],
      tmax_95  = temp.max  >= tmax_pctiles["p95"],
      tmax_99  = temp.max  >= tmax_pctiles["p99"]
    ) %>%
    group_by(case) %>%
    summarise(across(tmin_90:tmax_99, ~ sum(.x, na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-case,
                 names_to  = c("metric", "pctile"),
                 names_pattern = "(t\\w+)_(\\d+)") %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    mutate(category  = label,
           pctile    = paste0(pctile, "th"),
           .before   = 1)
}

heat_table <- bind_rows(
  heatcounts(ed_postpartum, "All ED visits"),
  heatcounts(mental,        "Mental health"),
  heatcounts(obst,          "Obstetric"),
  heatcounts(injury,        "Injury")
) %>%
  arrange(category, case, pctile) %>%
  rename(
    Category   = category,
    Case       = case,
    Percentile = pctile,
    T_min      = tmin,
    T_mean     = tmean,
    T_max      = tmax
  )

print(heat_table)
