
library(haven)
library(tidyverse)
library(sjmisc)

rm(list = ls())

files <- paste0("O:/1. Bases de Datos (Encuestas)/ENE/STATA/Base Censo2017/Bases rectificadas/ene-202310.dta")

base <- haven::read_dta(files) %>% 
  as_tibble()

names(base)

sjmisc::find_var(base, "turno")

base %>% 
  filter(cae_especifico >= 1 & cae_especifico <= 7) %>% 
  sjmisc::frq(turno, weights = fact_cal)
  
db <- base %>% 
  filter(cae_especifico >= 1 & cae_especifico <= 7 & turno == 1) %>% 
  select(turno_d, turno_de, turno_h, turno_t, habituales, fact_cal) %>% 
  mutate(
    across(.cols = c(contains("turno"), habituales),
           .fns = ~ set_na(., na = c(888,999,8888,9999))),
    across(.cols = c(turno_d, turno_de, turno_t),
           .fns = ~ if_else(is.na(.), 0, .))
    ) %>% 
  sjlabelled::remove_all_labels()


db <- db %>% 
  rowwise() %>% 
  mutate(dias = sum(turno_d, turno_de),
         n_turnos_mes = 30/dias,
         n_turnos_mes = if_else(is.infinite(n_turnos_mes), 0, n_turnos_mes),
         horas_mes = turno_t * n_turnos_mes,
         horas_semana = (horas_mes/4.285714)) %>% 
  ungroup()

db$horas_semana <- round(db$horas_semana, 0)

db %>% 
  rowwise() %>% 
  mutate(lgl = if_else(habituales == horas_semana, T, F)) %>% 
  arrange(lgl)

descr(db$habituales)
descr(db$horas_semana)

hist(db$habituales)