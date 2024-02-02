for (yy in 2020:2023) { 
  for (mm in 1:12) { 
    periodo <- yy*100 + mm 
    if (periodo >= 202201 & periodo <= 202310){ 
      
      files <- paste0("O:/1. Bases de Datos (Encuestas)/ENE/STATA/Base Censo2017/Bases rectificadas/ene-",yy, ifelse(mm<10, paste0(0, mm), mm),".dta")
      
      base <- rio::import(files) %>% 
        sjlabelled::remove_all_labels(.) %>% 
        mutate(
          across(.cols = c(habituales, c3_3, efectivas, c2_2_3),
                 .fns = ~ set_na(., na = c(888, 999))),
          ocupado = if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0),
          asal_priv = if_else(categoria_ocupacion == 3, 1, 0),
          formal = if_else(ocup_form == 1, 1, 0),
          hab_t = case_when(habituales >= 1 & habituales < 31 ~ "1 a 30",
                            habituales > 30 & habituales < 40 ~ "31 a 39",
                            habituales == 40 ~ "40",
                            habituales > 40 & habituales < 45 ~ "41 a 44",
                            habituales == 45 ~ "45",
                            habituales > 45 ~ "46 o más"))
      
      base_pond <- as_survey_design(.data = base, ids = 1, strata = estrato, weights = fact_cal)

      tbl1 <- base_pond %>% 
        filter(asal_priv == 1 & !is.na(hab_t)) %>% 
        group_by(ano_trimestre, mes_central, hab_t) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci"),
                  prop = survey_prop(vartype = "ci", proportion = T)*100) 
        
      tbls <- list(tbl1 = tbl1)
      
      for (i in seq_along(tbls)) {
        assign(paste0(names(tbls)[i], "_", periodo), tbls[[i]])
      }
    
      # Muestra el avance en la consola
      cat("Iteración:", periodo, "\n")
      
    }}}

tbl1 <- do.call("bind_rows", lapply(ls(pattern = "tbl1_"),get)) %>% ungroup()  


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

tbl1 <- tbl1 %>% 
  fun_trim() %>%  
  mutate(mes_central = ifelse(mes_central<10, paste0(0, mes_central), mes_central),
         anotrim_n = paste0(ano_trimestre, mes_central),
         anotrim_n = as.numeric(anotrim_n))

tbl1 %<>% 
  select(ano_trimestre, mes_central, hab_t, prop) %>% 
  mutate(mes_central = as.numeric(mes_central),
         hab_t = str_replace(hab_t, " ", "_"),
         hab_t = str_replace(hab_t, " ", "_")) %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central),
              names_from = hab_t,
              values_from = prop,
              names_glue = "{.value}_{hab_t}")

tbl1 %<>% 
  mutate(periodo  = case_when(mes_central==1 ~"dic-feb",
                              mes_central==2 ~"ene-mar",
                              mes_central==3 ~"feb-abr",
                              mes_central==4 ~"mar-may",
                              mes_central==5 ~"abr-jun",
                              mes_central==6 ~"may-jul",
                              mes_central==7 ~"jun-ago",
                              mes_central==8 ~"jul-sep",
                              mes_central==9 ~"ago-oct",
                              mes_central==10~"sep-nov",
                              mes_central==11~"oct-dic",
                              mes_central==12~"nov-ene"))


tbl1$periodo <- factor(tb1$periodo, 
                    levels=c("dic-feb","ene-mar","feb-abr",
                             "mar-may","abr-jun","may-jul",
                             "jun-ago","jul-sep","ago-oct",
                             "sep-nov","oct-dic","nov-ene"))


tbl1 %<>%
  select(ano_trimestre, mes_central, prop_40, prop_41_a_44, periodo) %>% 
  mutate(id = row_number(),
         etiqueta = interaction(periodo, ano_trimestre)) 




db <- tbl1 %>% filter(id >= 13) %>% select(starts_with("prop"), id, etiqueta)


lm(prop_40 ~ id, db)
