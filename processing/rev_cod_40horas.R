# Revision codigo 40 horas

library(pacman)

pacman::p_load(tidyverse, rio, sjlabelled, here, sjmisc)

options(scipen=999)

for (yy in 2010:2023) { 
  for (mm in 1:12) { 
    periodo <- yy*100 + mm 
    if (periodo >= 201710 & periodo <= 2023010){ 
      
      files <- paste0("O:/1. Bases de Datos (Encuestas)/ENE/STATA/Base Censo2017/Bases rectificadas/ene-",yy, ifelse(mm<10, paste0(0, mm), mm),".dta")
      
      base <- rio::import(files) %>% 
        sjlabelled::remove_all_labels(.) %>% 
        mutate(across(.cols = c(habituales, c3_3, efectivas, c2_2_3),.fns = ~ set_na(., na = c(888, 999))),
               asal_priv = if_else(categoria_ocupacion == 3, 1, 0),
               formal = if_else(ocup_form == 1, 1, 0),
               hab=case_when(habituales>=1 & habituales<31 ~ "1 a 30",
                             habituales>30 & habituales<40 ~ "31 a 39",
                             habituales==40 ~ "40",
                             habituales>40 & habituales<45 ~ "41 a 44",
                             habituales==45 ~ "45",
                             habituales>45 ~ "46 o más"),
               cnt=case_when(c3_3>=1 & c3_3<31 ~ "1 a 30",
                             c3_3>30 & c3_3<40 ~ "31 a 39",
                             c3_3==40 ~ "40",
                             c3_3>40 & c3_3<45 ~ "41 a 44",
                             c3_3==45 ~ "45",
                             c3_3>45 ~ "46 o más"),
               efc=case_when(efectivas>=1 & efectivas<31 ~ "1 a 30",
                             efectivas>30 & efectivas<40 ~ "31 a 39",
                             efectivas==40 ~ "40",
                             efectivas>40 & efectivas<45 ~ "41 a 44",
                             efectivas==45 ~ "45",
                             efectivas>45 ~ "46 o más"),
               dos=ifelse(!is.na(c2_2_3), habituales+c2_2_3, habituales),
               dos=case_when(dos>=1 & dos<31 ~ "1 a 30",
                             dos>30 & dos<40 ~ "31 a 39",
                             dos==40 ~ "40",
                             dos>40 & dos<45 ~ "41 a 44",
                             dos==45 ~ "45",
                             dos>45 ~ "46 o más"),
               seg=ifelse(b19==1, 1, 0)) 
      
      tbl1 <- base %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, hab) %>% 
        summarise(estimacion = sum(fact_cal, na.rm = T))
      
      tbl2 <- base %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, cnt) %>% 
        summarise(estimacion = sum(fact_cal, na.rm = T))
      
      tbl3 <- base %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, efc) %>% 
        summarise(estimacion = sum(fact_cal, na.rm = T))
      
      tbl4 <- base %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, dos) %>% 
        summarise(estimacion = sum(fact_cal, na.rm = T))
      
      tbl5 <- base %>% 
        filter(!is.na(hab) & !is.na(seg)) %>% 
        group_by(ano_trimestre, mes_central, seg, hab) %>% 
        summarise(t = sum(fact_cal)) %>% 
        ungroup() %>% 
        group_by(ano_trimestre, mes_central, hab) %>% 
        mutate(prop = prop.table(t)*100) %>% 
        filter(seg == 1) %>% 
        select(hab, prop)
      
      tbl6 <- base %>% 
        filter(!is.na(cnt) & !is.na(seg)) %>% 
        group_by(ano_trimestre, mes_central, seg, cnt) %>% 
        summarise(t = sum(fact_cal)) %>% 
        ungroup() %>% 
        group_by(ano_trimestre, mes_central, cnt) %>% 
        mutate(prop = prop.table(t)*100) %>% 
        filter(seg == 1) %>% 
        select(cnt, prop)
      
      tbl7 <- base %>% 
        filter(!is.na(efc) & !is.na(seg)) %>% 
        group_by(ano_trimestre, mes_central, seg, efc) %>% 
        summarise(t = sum(fact_cal)) %>% 
        ungroup() %>% 
        group_by(ano_trimestre, mes_central, efc) %>% 
        mutate(prop = prop.table(t)*100) %>% 
        filter(seg == 1) %>% 
        select(efc, prop)
      
      tbl8 <- base %>% 
        filter(!is.na(dos) & !is.na(seg)) %>% 
        group_by(ano_trimestre, mes_central, seg, dos) %>% 
        summarise(t = sum(fact_cal)) %>% 
        ungroup() %>% 
        group_by(ano_trimestre, mes_central, dos) %>% 
        mutate(prop = prop.table(t)*100) %>% 
        filter(seg == 1) %>% 
        select(dos, prop)
      
      assign(paste0("tbl1_", periodo), tbl1)
      assign(paste0("tbl2_", periodo), tbl2)
      assign(paste0("tbl3_", periodo), tbl3)
      assign(paste0("tbl4_", periodo), tbl4)
      assign(paste0("tbl5_", periodo), tbl5)
      assign(paste0("tbl6_", periodo), tbl6)
      assign(paste0("tbl7_", periodo), tbl7)
      assign(paste0("tbl8_", periodo), tbl8)
      
    }}}

tbl1 <- do.call("bind_rows", lapply(ls(pattern = "tbl1_"),get)) %>% ungroup()  
tbl2 <- do.call("bind_rows", lapply(ls(pattern = "tbl2_"),get)) %>% ungroup()  
tbl3 <- do.call("bind_rows", lapply(ls(pattern = "tbl3_"),get)) %>% ungroup()  
tbl4 <- do.call("bind_rows", lapply(ls(pattern = "tbl4_"),get)) %>% ungroup()  
tbl5 <- do.call("bind_rows", lapply(ls(pattern = "tbl5_"),get)) %>% ungroup()  
tbl6 <- do.call("bind_rows", lapply(ls(pattern = "tbl6_"),get)) %>% ungroup()  
tbl7 <- do.call("bind_rows", lapply(ls(pattern = "tbl7_"),get)) %>% ungroup()  
tbl8 <- do.call("bind_rows", lapply(ls(pattern = "tbl8_"),get)) %>% ungroup()  


fun_trim <- function(datos) {
  datos %>%
    mutate(ano = as.numeric(ano_trimestre),
           trim = case_when(
             mes_central == 1 ~ paste0(" def"),
             mes_central == 2 ~ paste0(" efm"),
             mes_central == 3 ~ paste0(" fma"),
             mes_central == 4 ~ paste0(" mam"),
             mes_central == 5 ~ paste0(" amj"),
             mes_central == 6 ~ paste0(" mjj"),
             mes_central == 7 ~ paste0(" jja"),
             mes_central == 8 ~ paste0(" jas"),
             mes_central == 9 ~ paste0(" aso"),
             mes_central == 10 ~ paste0(" son"),
             mes_central == 11 ~ paste0(" ond"),
             mes_central == 12 ~ paste0(" nde")),
           anotrim = paste0(ano, trim)) %>% 
    select(-c(ano, trim))
}

tbls <- list(tbl1, tbl2, tbl3, tbl4, tbl5, tbl6, tbl7, tbl8)

proc_db <- map(tbls, fun_trim)


# ------

t1 <- proc_db[[1]] %>%
  na.omit() %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = hab,
              values_from = estimacion)
  
t2 <- proc_db[[2]] %>%
  na.omit() %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = cnt,
              values_from = estimacion)

t3 <- proc_db[[3]] %>%
  na.omit() %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = efc,
              values_from = estimacion)

t4 <- proc_db[[4]] %>%
  na.omit() %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = dos,
              values_from = estimacion)

t5 <- proc_db[[5]] %>%
  na.omit() %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = hab,
              values_from = prop)

t6 <- proc_db[[6]] %>%
  na.omit() %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = cnt,
              values_from = prop)

t7 <- proc_db[[7]] %>%
  na.omit() %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = efc,
              values_from = prop)

t8 <- proc_db[[8]] %>%
  na.omit() %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = dos,
              values_from = prop)




dataset_names <- list(t1, t2, t3, t4, t5, t6, t7, t8)

write.xlsx(dataset_names, file = 'mydata2.xlsx')

writexl::write_xlsx(x = dataset_names, path = 'mydata2.xlsx')




library(MetBrewer)
library(plotly)

g1 <- proc_db[[1]] %>%
  select(-c(ano_trimestre, mes_central)) %>% 
  na.omit() %>% 
  pivot_wider(id_cols = anotrim,
              names_from = hab,
              values_from = estimacion) %>% 
  mutate(anotrim = factor(anotrim, levels = anotrim)) %>% 
  pivot_longer(cols = -anotrim) %>% 
  ggplot(aes(x = anotrim, y = value, group = name)) +
  geom_line(aes(color = name), linewidth = 1) +
  scale_y_continuous(labels = function(x) format(x, big.mark = "."), n.breaks = 10) +
  MetBrewer::scale_colour_met_d(name = "Archambault") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   colour = "#4a4a4a",
                                   size = rel(0.8)))



ggplotly(g1)


g2 <- proc_db[[1]] %>%
  select(-c(ano_trimestre, mes_central)) %>% 
  na.omit() %>% 
  group_by(anotrim) %>% 
  mutate(prop = prop.table(estimacion)*100) %>% 
  pivot_wider(id_cols = anotrim,
              names_from = hab,
              values_from = prop) %>% 
  mutate(anotrim = factor(anotrim, levels = anotrim)) %>% 
  pivot_longer(cols = -anotrim) %>% 
  ggplot(aes(x = anotrim, y = value, group = name)) +
  geom_line(aes(color = name), linewidth = 1) +
  scale_y_continuous(labels = function(prop){paste0(prop, "%")},
                     n.breaks = 10) +
  MetBrewer::scale_colour_met_d(name = "Archambault") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   colour = "#4a4a4a",
                                   size = rel(0.8)))



ggplotly(g2)
