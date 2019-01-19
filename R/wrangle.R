library(tidyverse)
library(lubridate)

# script to match the format JM in rstanarm takes which a tibble coag_long
# that stores the longitudinal times and a tibble coag_surv that stores the survival times
# see https://github.com/stan-dev/stancon_talks/blob/master/2018/Contributed-Talks/03_brilleman/notebook.pdf

coag <- read_csv("data/ACITdataset_UCSB_23Feb15.csv") %>%
  select(acitnum, male, age, iss, edarrivaldatetime, datetimeofdeath,
         
         hr0_datetime, hr0_factorii, hr0_ddimer,
         hr2_datetime, hr2_factorii, hr2_ddimer,
         hr3_datetime, hr3_factorii, hr3_ddimer,
         hr4_datetime, hr4_factorii, hr4_ddimer,
         hr6_datetime, hr6_factorii, hr6_ddimer,
         hr12_datetime, hr12_factorii, hr12_ddimer,
         hr24_datetime, hr24_factorii, hr24_ddimer,
         hr48_datetime, hr48_factorii, hr48_ddimer,
         hr72_datetime, hr72_factorii, hr72_ddimer,
         hr96_datetime, hr96_factorii, hr96_ddimer,
         hr120_datetime, hr120_factorii, hr120_ddimer) %>%
  
  #wrangle times
  mutate_each(funs(parse_date_time(., "m/d/y H:M")),
              edarrivaldatetime, datetimeofdeath,
              hr0_datetime, hr2_datetime, hr3_datetime, hr4_datetime, 
              hr6_datetime, hr12_datetime, hr24_datetime, hr48_datetime,
              hr72_datetime, hr96_datetime, hr120_datetime) %>%
  
  mutate(death_time_hours = (edarrivaldatetime %--% datetimeofdeath) / dhours(1)) %>%
  
  mutate(hr0_time_hours = (edarrivaldatetime %--% hr0_datetime) / dhours(1)) %>%
  mutate(hr2_time_hours = (edarrivaldatetime %--% hr2_datetime) / dhours(1)) %>%
  mutate(hr3_time_hours = (edarrivaldatetime %--% hr3_datetime) / dhours(1)) %>%
  mutate(hr4_time_hours = (edarrivaldatetime %--% hr4_datetime) / dhours(1)) %>%
  
  mutate(hr6_time_hours = (edarrivaldatetime %--% hr6_datetime) / dhours(1)) %>%
  mutate(hr12_time_hours = (edarrivaldatetime %--% hr12_datetime) / dhours(1)) %>%
  mutate(hr24_time_hours = (edarrivaldatetime %--% hr24_datetime) / dhours(1)) %>%
  mutate(hr48_time_hours = (edarrivaldatetime %--% hr48_datetime) / dhours(1)) %>%
  
  mutate(hr72_time_hours = (edarrivaldatetime %--% hr72_datetime) / dhours(1)) %>%
  mutate(hr96_time_hours = (edarrivaldatetime %--% hr96_datetime) / dhours(1)) %>%
  mutate(hr120_time_hours = (edarrivaldatetime %--% hr120_datetime) / dhours(1)) %>%
  
  select(-edarrivaldatetime, -datetimeofdeath,
         -hr0_datetime, -hr2_datetime, -hr3_datetime, -hr4_datetime, 
         -hr6_datetime, -hr12_datetime, -hr24_datetime, -hr48_datetime,
         -hr72_datetime, -hr96_datetime, -hr120_datetime)

blood_draw_times <-
  coag %>%
  select(acitnum,
        hr0_time_hours, hr2_time_hours, hr3_time_hours,hr4_time_hours,
         hr6_time_hours, hr12_time_hours, hr24_time_hours, hr48_time_hours,
         hr72_time_hours, hr96_time_hours, hr120_time_hours) %>%
  gather(nominal_hour, actual_hour, -acitnum) %>%
  mutate(nominal_hour = str_extract(nominal_hour, "hr[0-9]+"))

coag_long <-
  coag %>%
  select(-death_time_hours,
         -hr0_time_hours, -hr2_time_hours, -hr3_time_hours, -hr4_time_hours,
         -hr6_time_hours, -hr12_time_hours, -hr24_time_hours, -hr48_time_hours,
         -hr72_time_hours, -hr96_time_hours, -hr120_time_hours) %>%
  gather(protein, value, -acitnum,-male, -age, -iss) %>%
  separate(protein, c("nominal_hour", "protein"), sep = "_") %>%
  spread(protein, value) %>%
  inner_join(blood_draw_times) %>%
  select(acitnum, age, male, iss, hours_since_arrival = actual_hour, factorii, ddimer) %>%
  mutate_at(vars(age, iss), as.double) %>%
  na.omit %>%
  filter(hours_since_arrival >= 0.0) %>%
  filter(hours_since_arrival <= 168) %>%
  mutate(acitnum = factor(acitnum))
  
# only consider first week of deaths else the patient is censored as per the famous
# "trimodal" death time: search google images for "trauma distribution of death times"
coag_surv <-
  coag %>%
  select(acitnum, age, male, iss, death_time_hours) %>%
  mutate(died = !is.na(death_time_hours)) %>%
  mutate(died_first_week = died & (death_time_hours < 7*24)) %>%
  mutate(death_time_hours = ifelse(died_first_week, death_time_hours, 7*24)) %>%
  select(-died) %>%
  mutate_at(vars(age, iss), as.double) %>%
  filter(acitnum %in% unique(coag_long$acitnum)) %>%
  mutate(acitnum = factor(acitnum))
