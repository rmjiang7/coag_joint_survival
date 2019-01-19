library(rstanarm)

fit <- stan_jm(
  formulaLong = list(
    factorii ~ age + male + iss + hours_since_arrival + (hours_since_arrival | acitnum), 
    ddimer ~ age + male + iss + hours_since_arrival + (hours_since_arrival | acitnum)),
  formulaEvent = survival::Surv(death_time_hours, died_first_week) ~ age + male + iss, 
  dataLong = coag_long, dataEvent = coag_surv,
  time_var = "hours_since_arrival",
  chains = 1, refresh = 10, seed = 12345)
ier