# Codigo 40 horas: Procesamiento datos
# Autor: Andreas Laffert


# 1. Librerías -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, rio, sjlabelled, here, sjmisc, srvyr)

options(scipen=999)
options(survey.lonely.psu = "certainty")
rm(list = ls())

# 2. Cargar, procesar y estimar datos ---------------------------------------------

## 2.1 Agregado

for (yy in 2017:2023) { 
  for (mm in 1:12) { 
    periodo <- yy*100 + mm 
    if (periodo >= 201708 & periodo <= 202310){ 
      
      files <- paste0("O:/1. Bases de Datos (Encuestas)/ENE/STATA/Base Censo2017/40 horas/ene-",yy, ifelse(mm<10, paste0(0, mm), mm),".dta")
      
      base <- rio::import(files) %>% 
        sjlabelled::remove_all_labels(.) %>% 
        mutate(
          across(.cols = c(habituales, c2_2_3),
                 .fns = ~ set_na(., na = c(888, 999))),
          asal_priv = if_else(categoria_ocupacion == 3, 1, 0),
          formal = if_else(ocup_form == 1, 1, 0),
          hab_t = case_when(habituales >= 1 & habituales < 31 ~ "1 a 30",
                            habituales > 30 & habituales < 40 ~ "31 a 39",
                            habituales == 40 ~ "40",
                            habituales > 40 & habituales < 45 ~ "41 a 44",
                            habituales == 45 ~ "45",
                            habituales > 45 ~ "46 o más"),
          habituales_dos = ifelse(!is.na(c2_2_3), habituales + c2_2_3, habituales),
          hab_t_dos = case_when(habituales_dos >= 1 & habituales_dos < 31 ~ "1 a 30",
                                habituales_dos > 30 & habituales_dos < 40 ~ "31 a 39",
                                habituales_dos == 40 ~ "40",
                                habituales_dos > 40 & habituales_dos < 45 ~ "41 a 44",
                                habituales_dos == 45 ~ "45",
                                habituales_dos > 45 ~ "46 o más"),
          seg_ocup = ifelse(b19 == 1, 1, 0))
      
      base_pond <- as_survey_design(.data = base, ids = 1, strata = estrato, weights = fact_cal)
      
      ## A: HABITUALES OCUP PRINCIPAL  
      
      # medida de horas habituales promedio ocupacion principal
      
      agr1 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central) %>% 
        summarise(media = survey_mean(habituales, na.rm = T, vartype = "ci"))
      
      # tramo horas habituales 
      
      agr2 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t)) %>% 
        group_by(ano_trimestre, mes_central, hab_t) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci"),
                  prop = survey_prop(vartype = "ci", proportion = T)*100) 
      
      ## B: HORAS HABITUALES TOTALES (MATHO)
      
      # medida general de horas de trabajo total promedio (suma habituales de primera y segunda ocupacion en caso de que tenga)
      
      agr3 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central) %>% 
        summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"))
      
      # tramo horas de trabajo total 
      
      agr4 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos)) %>% 
        group_by(ano_trimestre, mes_central, hab_t_dos) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci"),
                  prop = survey_prop(vartype = "ci", proportion = T)*100) 
      
      assign(paste0("agr1_", periodo), agr1)
      assign(paste0("agr2_", periodo), agr2)
      assign(paste0("agr3_", periodo), agr3)
      assign(paste0("agr4_", periodo), agr4)
      
      # Muestra el avance en la consola
      cat("Iteración:", periodo, "\n")
      
      
    }}}

## 2.2 Aperturas

for (yy in 2020:2023) { 
  for (mm in 1:12) { 
    periodo <- yy*100 + mm 
    if (periodo >= 202002 & periodo <= 202310){ 
      
      files <- paste0("O:/1. Bases de Datos (Encuestas)/ENE/STATA/Base Censo2017/40 horas/ene-",yy, ifelse(mm<10, paste0(0, mm), mm),".dta")
      
      base <- rio::import(files) %>% 
        sjlabelled::remove_all_labels(.) %>% 
        mutate(
          across(.cols = c(habituales, c2_2_3),
                 .fns = ~ set_na(., na = c(888, 999))),
          asal_priv = if_else(categoria_ocupacion == 3, 1, 0),
          formal = if_else(ocup_form == 1, 1, 0),
          hab_t = case_when(habituales >= 1 & habituales < 31 ~ "1 a 30",
                            habituales > 30 & habituales < 40 ~ "31 a 39",
                            habituales == 40 ~ "40",
                            habituales > 40 & habituales < 45 ~ "41 a 44",
                            habituales == 45 ~ "45",
                            habituales > 45 ~ "46 o más"),
          habituales_dos = ifelse(!is.na(c2_2_3), habituales + c2_2_3, habituales),
          hab_t_dos = case_when(habituales_dos >= 1 & habituales_dos < 31 ~ "1 a 30",
                                habituales_dos > 30 & habituales_dos < 40 ~ "31 a 39",
                                habituales_dos == 40 ~ "40",
                                habituales_dos > 40 & habituales_dos < 45 ~ "41 a 44",
                                habituales_dos == 45 ~ "45",
                                habituales_dos > 45 ~ "46 o más"),
          seg_ocup = ifelse(b19 == 1, 1, 0),
          edad_t = case_when(edad >= 15 & edad <= 29 ~ "15 a 29 años",
                             edad >= 30 & edad <= 44 ~ "30 a 44 años",
                             edad >= 45 & edad <= 59 ~ "45 a 59 años",
                             edad >= 60 ~ "60 años o más"),
          edad_t = factor(edad_t, 
                          levels = c("15 a 29 años",
                                     "30 a 44 años",
                                     "45 a 59 años",
                                     "60 años o más")),
          cine = case_when(cine %in% c(1:4) ~ "Primaria o menos",
                           cine == 5 ~ "Secundaria",
                           cine == 6 ~ "Técnica",
                           cine %in% c(7:9) ~ "Universitaria o más"),
          cine = factor(cine,
                        levels = c("Primaria o menos",
                                   "Secundaria",
                                   "Técnica",
                                   "Universitaria o más")),
          rama = case_when(b14_rev4cl_caenes == 1 ~ "Agricultura, ganadería, silvicultura y pesca",
                           b14_rev4cl_caenes == 2 ~ "Explotación de minas y canteras", 
                           b14_rev4cl_caenes == 3 ~ "Industrias manufactureras",
                           b14_rev4cl_caenes == 4 ~ "Suministro de electricidad, gas, vapor y aire acondicionado",
                           b14_rev4cl_caenes == 5 ~ "Suministro de agua",
                           b14_rev4cl_caenes == 6 ~ "Construcción",
                           b14_rev4cl_caenes == 7 ~ "Comercio al por mayor y al por menor",
                           b14_rev4cl_caenes == 8 ~ "Transporte y almacenamiento",
                           b14_rev4cl_caenes == 9 ~ "Actividades de alojamiento y de servicio de comidas",
                           b14_rev4cl_caenes == 10 ~ "Información y comunicaciones",
                           b14_rev4cl_caenes == 11 ~ "Actividades financieras y de seguros",
                           b14_rev4cl_caenes == 12 ~ "Actividades inmobiliarias",
                           b14_rev4cl_caenes == 13 ~ "Actividades profesionales, científicas y técnicas",
                           b14_rev4cl_caenes == 14 ~ "Actividades de servicios administrativos y de apoyo",
                           b14_rev4cl_caenes == 15 ~ "Administración pública y defensa",
                           b14_rev4cl_caenes == 16 ~ "Enseñanza",
                           b14_rev4cl_caenes == 17 ~ "Actividades de atención de la salud humana y de asistencia social",
                           b14_rev4cl_caenes == 18 ~ "Actividades artísticas, de entretenimiento y recreativas",
                           b14_rev4cl_caenes == 19 ~ "Otras actividades de servicios",
                           b14_rev4cl_caenes == 20 ~ "Actividades de los hogares como empleadores",
                           b14_rev4cl_caenes == 21 ~ "Actividades de organizaciones y órganos extraterritoriales"),
          rama = factor(rama, 
                        levels = c("Agricultura, ganadería, silvicultura y pesca",
                                   "Explotación de minas y canteras", 
                                   "Industrias manufactureras",
                                   "Suministro de electricidad, gas, vapor y aire acondicionado",
                                   "Suministro de agua",
                                   "Construcción",
                                   "Comercio al por mayor y al por menor",
                                   "Transporte y almacenamiento",
                                   "Actividades de alojamiento y de servicio de comidas",
                                   "Información y comunicaciones",
                                   "Actividades financieras y de seguros",
                                   "Actividades inmobiliarias",
                                   "Actividades profesionales, científicas y técnicas",
                                   "Actividades de servicios administrativos y de apoyo",
                                   "Administración pública y defensa",
                                   "Enseñanza",
                                   "Actividades de atención de la salud humana y de asistencia social",
                                   "Actividades artísticas, de entretenimiento y recreativas",
                                   "Otras actividades de servicios",
                                   "Actividades de los hogares como empleadores",
                                   "Actividades de organizaciones y órganos extraterritoriales")),
          ciuo08 = case_when(b1 == 1 ~ "Directores y gerentes",
                             b1 == 2 ~ "Profesionales científicos e intelectuales",
                             b1 == 3 ~ "Técnicos y profesionales de nivel medio",
                             b1 == 4 ~ "Personal de apoyo administrativo",
                             b1 == 5 ~ "Trabajadores de los servicios y vendedores de comercios y mercados",
                             b1 == 6 ~ "Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros",
                             b1 == 7 ~ "Oficiales, operarios y artesanos de artes mecánicas y de otros oficios",
                             b1 == 8 ~ "Operadores de instalaciones y máquinas y ensambladores",
                             b1 == 9 ~ "Ocupaciones elementales",
                             b1 == 10 ~ "Otros no identificados",
                             TRUE ~ NA_character_),
          ciuo08 = factor(ciuo08, 
                          levels = c("Directores y gerentes",
                                     "Profesionales científicos e intelectuales",
                                     "Técnicos y profesionales de nivel medio",
                                     "Personal de apoyo administrativo",
                                     "Trabajadores de los servicios y vendedores de comercios y mercados",
                                     "Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros",
                                     "Oficiales, operarios y artesanos de artes mecánicas y de otros oficios",
                                     "Operadores de instalaciones y máquinas y ensambladores",
                                     "Ocupaciones elementales",
                                     "Otros no identificados")),
          tam_emp = factor(b15_1,
                           levels = c(1,2,3,4,5),
                           labels = c("Menos de 5 trabajadores", 
                                      "Micro empresa (entre 5 y 9 trabajadores)", 
                                      "Pequeña empresa (entre 10 y 49 trabajadores)", 
                                      "Mediana empresa (entre 50 y 199 trabajadores)",
                                      "Gran empresa (200 o más trabajadores)")))
      
      base_pond <- as_survey_design(.data = base, ids = 1, strata = estrato, weights = fact_cal)
  
    ## A: HABITUALES OCUP PRINCIPAL  
    
      # medida de horas habituales promedio ocupacion principal
      
      tbl1 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central) %>% 
        summarise(media = survey_mean(habituales, na.rm = T, vartype = "ci"))
      
      # medida horas habituales promedio segun sexo
      
      tbl2 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, sexo) %>% 
        summarise(media = survey_mean(habituales, na.rm = T, vartype = "ci"))

      # medida horas habituales promedio segun edad
      
      tbl3 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, edad_t) %>% 
        summarise(media = survey_mean(habituales, na.rm = T, vartype = "ci"))
     
      # medida horas habituales promedio segun educacion
      
      tbl4 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, cine) %>% 
        summarise(media = survey_mean(habituales, na.rm = T, vartype = "ci"))
      
      # medida horas habituales promedio segun grupo ocupacional
      
      tbl5 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, ciuo08) %>% 
        summarise(media = survey_mean(habituales, na.rm = T, vartype = "ci"))
      
      # medida horas habituales promedio segun tamaño empresa
      
      tbl6 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, tam_emp) %>% 
        summarise(media = survey_mean(habituales, na.rm = T, vartype = "ci"))
      
      # medida de horas habituales promedio segun rama
      
      tbl7 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, rama) %>% 
        summarise(media = survey_mean(habituales, na.rm = T, vartype = "ci"))
      
      # tramo horas habituales 
      
      tbl8 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t)) %>% 
        group_by(ano_trimestre, mes_central, hab_t) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci"),
                  prop = survey_prop(vartype = "ci", proportion = T)*100) 
      
      # tramo horas habituales segun sexo
      
      tbl9 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t) & !is.na(sexo)) %>% 
        group_by(ano_trimestre, mes_central, hab_t, sexo) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
        ungroup() %>%
        group_by(sexo) %>% 
        mutate(prop = prop.table(total)*100)
      
      
      # tramo horas habituales segun edad
      
      tbl10 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t) & !is.na(edad_t)) %>% 
        group_by(ano_trimestre, mes_central, hab_t, edad_t) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
        ungroup() %>%
        group_by(edad_t) %>% 
        mutate(prop = prop.table(total)*100)
      
      # tramo horas habituales segun cine
      
      tbl11 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t) & !is.na(cine)) %>% 
        group_by(ano_trimestre, mes_central, hab_t, cine) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
        ungroup() %>%
        group_by(cine) %>% 
        mutate(prop = prop.table(total)*100)
      
      
      # tramo horas habituales segun grupo ocupacional
      
      tbl12 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t) & !is.na(ciuo08)) %>% 
        group_by(ano_trimestre, mes_central, hab_t, ciuo08) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
        ungroup() %>%
        group_by(ciuo08) %>% 
        mutate(prop = prop.table(total)*100)
      
      # tramo horas habituales segun rama
      
      tbl13 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t) & !is.na(rama)) %>% 
        group_by(ano_trimestre, mes_central, hab_t, rama) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
        ungroup() %>%
        group_by(rama) %>% 
        mutate(prop = prop.table(total)*100)
      
      # tramo horas habituales total segun tam emp
      
      tbl14 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t) & !is.na(tam_emp)) %>% 
        group_by(ano_trimestre, mes_central, hab_t, tam_emp) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
        ungroup() %>%
        group_by(tam_emp) %>% 
        mutate(prop = prop.table(total)*100)
  
  ## B: HORAS HABITUALES TOTALES (MATHO)
      
      # medida general de horas de trabajo total promedio (suma habituales de primera y segunda ocupacion en caso de que tenga)
      
      tbl15 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central) %>% 
        summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"))
      
      # medida de horas de trabajo total promedio segun tener o no segunda ocup
      
      tbl16 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, seg_ocup) %>% 
        summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"))
      
      # medida de horas de trabajo total promedio segun sexo
      
      tbl17 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, sexo) %>% 
        summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"))
      
      # medida de horas de trabajo total promedio segun edad
      
      tbl18 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, edad_t) %>% 
        summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"))
      
      # medida de horas de trabajo total promedio segun educacion
      
      tbl19 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, cine) %>% 
        summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"))
      
      # medida de horas de trabajo total promedio segun grupo ocupacional
      
      tbl20 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, ciuo08) %>% 
        summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"))
      
      # medida de horas de trabajo total promedio segun tamaño empresa
      
      tbl21 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, tam_emp) %>% 
        summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"))
      
      # medida de horas de trabajo total promedio segun rama
      
      tbl22 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1) %>% 
        group_by(ano_trimestre, mes_central, rama) %>% 
        summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"))
      
      
      # tramo horas de trabajo total 
      
      tbl23 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos)) %>% 
        group_by(ano_trimestre, mes_central, hab_t_dos) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci"),
                  prop = survey_prop(vartype = "ci", proportion = T)*100) 
      
      # tramo horas de trabajo total segun sexo
      
      tbl24 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(sexo)) %>% 
        group_by(ano_trimestre, mes_central, hab_t_dos, sexo) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
        ungroup() %>%
        group_by(sexo) %>% 
        mutate(prop = prop.table(total)*100)
      
      # tramo horas de trabajo total segun edad
      
      tbl25 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(edad_t)) %>% 
        group_by(ano_trimestre, mes_central, hab_t_dos, edad_t) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
        ungroup() %>%
        group_by(edad_t) %>% 
        mutate(prop = prop.table(total)*100)
     
        # tramo horas de trabajo total segun cine
      
      tbl26 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(cine)) %>% 
        group_by(ano_trimestre, mes_central, hab_t_dos, cine) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
        ungroup() %>%
        group_by(cine) %>% 
        mutate(prop = prop.table(total)*100)
      
      # tramo horas de trabajo total segun grupo ocupacional
      
      tbl27 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(ciuo08)) %>% 
        group_by(ano_trimestre, mes_central, hab_t_dos, ciuo08) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
        ungroup() %>%
        group_by(ciuo08) %>% 
        mutate(prop = prop.table(total)*100)
      
      # tramo horas de trabajo total segun rama
      
      tbl28 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(rama)) %>% 
        group_by(ano_trimestre, mes_central, hab_t_dos, rama) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
        ungroup() %>%
        group_by(rama) %>% 
        mutate(prop = prop.table(total)*100)
      
      # tramo horas de trabajo total segun tam emp
      
      tbl29 <- base_pond %>% 
        filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(tam_emp)) %>% 
        group_by(ano_trimestre, mes_central, hab_t_dos, tam_emp) %>% 
        summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
        ungroup() %>%
        group_by(tam_emp) %>% 
        mutate(prop = prop.table(total)*100)
      
      tbls <- list(tbl1 = tbl1, 
                   tbl2 = tbl2, 
                   tbl3 = tbl3, 
                   tbl4 = tbl4, 
                   tbl5 = tbl5, 
                   tbl6 = tbl6, 
                   tbl7 = tbl7, 
                   tbl8 = tbl8, 
                   tbl9 = tbl9,
                   tbl10 = tbl10,
                   tbl11 = tbl11,
                   tbl12 = tbl12,
                   tbl13 = tbl13,
                   tbl14 = tbl14,
                   tbl15 = tbl15,
                   tbl16 = tbl16,
                   tbl17 = tbl17,
                   tbl18 = tbl18,
                   tbl19 = tbl19,
                   tbl20 = tbl20,
                   tbl21 = tbl21,
                   tbl22 = tbl22,
                   tbl23 = tbl23,
                   tbl24 = tbl24,
                   tbl25 = tbl25,
                   tbl26 = tbl26,
                   tbl27 = tbl27,
                   tbl28 = tbl28,
                   tbl29 = tbl29)
      
      for (i in seq_along(tbls)) {
        assign(paste0(names(tbls)[i], "_", periodo), tbls[[i]])
      }
      
      # Muestra el avance en la consola
      cat("Iteración:", periodo, "\n")
      
      
    }}}


# 3. Unir y transformar ---------------------------------------------------

agr1 <- do.call("bind_rows", lapply(ls(pattern = "agr1_"),get)) %>% ungroup()  
agr2 <- do.call("bind_rows", lapply(ls(pattern = "agr2_"),get)) %>% ungroup()  
agr3 <- do.call("bind_rows", lapply(ls(pattern = "agr3_"),get)) %>% ungroup()  
agr4 <- do.call("bind_rows", lapply(ls(pattern = "agr4_"),get)) %>% ungroup()

tbl1 <- do.call("bind_rows", lapply(ls(pattern = "tbl1_"),get)) %>% ungroup()  
tbl2 <- do.call("bind_rows", lapply(ls(pattern = "tbl2_"),get)) %>% ungroup()  
tbl3 <- do.call("bind_rows", lapply(ls(pattern = "tbl3_"),get)) %>% ungroup()  
tbl4 <- do.call("bind_rows", lapply(ls(pattern = "tbl4_"),get)) %>% ungroup()  
tbl5 <- do.call("bind_rows", lapply(ls(pattern = "tbl5_"),get)) %>% ungroup()  
tbl6 <- do.call("bind_rows", lapply(ls(pattern = "tbl6_"),get)) %>% ungroup()  
tbl7 <- do.call("bind_rows", lapply(ls(pattern = "tbl7_"),get)) %>% ungroup()  
tbl8 <- do.call("bind_rows", lapply(ls(pattern = "tbl8_"),get)) %>% ungroup()  
tbl9 <- do.call("bind_rows", lapply(ls(pattern = "tbl9_"),get)) %>% ungroup()  
tbl10 <- do.call("bind_rows", lapply(ls(pattern = "tbl10_"),get)) %>% ungroup()

tbl11 <- do.call("bind_rows", lapply(ls(pattern = "tbl11_"),get)) %>% ungroup()  
tbl12 <- do.call("bind_rows", lapply(ls(pattern = "tbl12_"),get)) %>% ungroup()  
tbl13 <- do.call("bind_rows", lapply(ls(pattern = "tbl13_"),get)) %>% ungroup()  
tbl14 <- do.call("bind_rows", lapply(ls(pattern = "tbl14_"),get)) %>% ungroup()  
tbl15 <- do.call("bind_rows", lapply(ls(pattern = "tbl15_"),get)) %>% ungroup()  
tbl16 <- do.call("bind_rows", lapply(ls(pattern = "tbl16_"),get)) %>% ungroup() 
tbl17 <- do.call("bind_rows", lapply(ls(pattern = "tbl17_"),get)) %>% ungroup() 
tbl18 <- do.call("bind_rows", lapply(ls(pattern = "tbl18_"),get)) %>% ungroup() 
tbl19 <- do.call("bind_rows", lapply(ls(pattern = "tbl19_"),get)) %>% ungroup() 
tbl20 <- do.call("bind_rows", lapply(ls(pattern = "tbl20_"),get)) %>% ungroup() 

tbl21 <- do.call("bind_rows", lapply(ls(pattern = "tbl21_"),get)) %>% ungroup()  
tbl22 <- do.call("bind_rows", lapply(ls(pattern = "tbl22_"),get)) %>% ungroup()  
tbl23 <- do.call("bind_rows", lapply(ls(pattern = "tbl23_"),get)) %>% ungroup()  
tbl24 <- do.call("bind_rows", lapply(ls(pattern = "tbl24_"),get)) %>% ungroup()  
tbl25 <- do.call("bind_rows", lapply(ls(pattern = "tbl25_"),get)) %>% ungroup()  
tbl26 <- do.call("bind_rows", lapply(ls(pattern = "tbl26_"),get)) %>% ungroup()  
tbl27 <- do.call("bind_rows", lapply(ls(pattern = "tbl27_"),get)) %>% ungroup()  
tbl28 <- do.call("bind_rows", lapply(ls(pattern = "tbl28_"),get)) %>% ungroup()  
tbl29 <- do.call("bind_rows", lapply(ls(pattern = "tbl29_"),get)) %>% ungroup()  



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

tbls <- list(agr1,
             agr2,
             agr3,
             agr4,
             tbl1,
             tbl2,
             tbl3,
             tbl4,
             tbl5,
             tbl6,
             tbl7,
             tbl8,
             tbl9,
             tbl10,
             tbl11,
             tbl12,
             tbl13,
             tbl14,
             tbl15,
             tbl16,
             tbl17,
             tbl18,
             tbl19,
             tbl20,
             tbl21,
             tbl22,
             tbl23,
             tbl24,
             tbl25,
             tbl26,
             tbl27,
             tbl28,
             tbl29)

proc_db <- map(tbls, fun_trim)

## 2.3 Comparaciones transversales

ene <- list()
dat <- c(202210, 202310)

i <- 1
for(t in dat){
  files <- paste0("O:/1. Bases de Datos (Encuestas)/ENE/STATA/Base Censo2017/40 horas/ene-", t, ".dta")
  ene[[i]] <- rio::import(files) %>% 
    sjlabelled::remove_all_labels(.) %>% 
    mutate(
      across(.cols = c(habituales, c2_2_3),
             .fns = ~ set_na(., na = c(888, 999))),
      asal_priv = if_else(categoria_ocupacion == 3, 1, 0),
      formal = if_else(ocup_form == 1, 1, 0),
      hab_t = case_when(habituales >= 1 & habituales < 31 ~ "1 a 30",
                        habituales > 30 & habituales < 40 ~ "31 a 39",
                        habituales == 40 ~ "40",
                        habituales > 40 & habituales < 45 ~ "41 a 44",
                        habituales == 45 ~ "45",
                        habituales > 45 ~ "46 o más"),
      habituales_dos = ifelse(!is.na(c2_2_3), habituales + c2_2_3, habituales),
      hab_t_dos = case_when(habituales_dos >= 1 & habituales_dos < 31 ~ "1 a 30",
                            habituales_dos > 30 & habituales_dos < 40 ~ "31 a 39",
                            habituales_dos == 40 ~ "40",
                            habituales_dos > 40 & habituales_dos < 45 ~ "41 a 44",
                            habituales_dos == 45 ~ "45",
                            habituales_dos > 45 ~ "46 o más"),
      seg_ocup = ifelse(b19 == 1, 1, 0),
      edad_t = case_when(edad >= 15 & edad <= 29 ~ "15 a 29 años",
                         edad >= 30 & edad <= 44 ~ "30 a 44 años",
                         edad >= 45 & edad <= 59 ~ "45 a 59 años",
                         edad >= 60 ~ "60 años o más"),
      edad_t = factor(edad_t, 
                      levels = c("15 a 29 años",
                                 "30 a 44 años",
                                 "45 a 59 años",
                                 "60 años o más")),
      cine = case_when(cine %in% c(1:4) ~ "Primaria o menos",
                       cine == 5 ~ "Secundaria",
                       cine == 6 ~ "Técnica",
                       cine %in% c(7:9) ~ "Universitaria o más"),
      cine = factor(cine,
                    levels = c("Primaria o menos",
                               "Secundaria",
                               "Técnica",
                               "Universitaria o más")),
      rama = case_when(b14_rev4cl_caenes == 1 ~ "Agricultura, ganadería, silvicultura y pesca",
                       b14_rev4cl_caenes == 2 ~ "Explotación de minas y canteras", 
                       b14_rev4cl_caenes == 3 ~ "Industrias manufactureras",
                       b14_rev4cl_caenes == 4 ~ "Suministro de electricidad, gas, vapor y aire acondicionado",
                       b14_rev4cl_caenes == 5 ~ "Suministro de agua",
                       b14_rev4cl_caenes == 6 ~ "Construcción",
                       b14_rev4cl_caenes == 7 ~ "Comercio al por mayor y al por menor",
                       b14_rev4cl_caenes == 8 ~ "Transporte y almacenamiento",
                       b14_rev4cl_caenes == 9 ~ "Actividades de alojamiento y de servicio de comidas",
                       b14_rev4cl_caenes == 10 ~ "Información y comunicaciones",
                       b14_rev4cl_caenes == 11 ~ "Actividades financieras y de seguros",
                       b14_rev4cl_caenes == 12 ~ "Actividades inmobiliarias",
                       b14_rev4cl_caenes == 13 ~ "Actividades profesionales, científicas y técnicas",
                       b14_rev4cl_caenes == 14 ~ "Actividades de servicios administrativos y de apoyo",
                       b14_rev4cl_caenes == 15 ~ "Administración pública y defensa",
                       b14_rev4cl_caenes == 16 ~ "Enseñanza",
                       b14_rev4cl_caenes == 17 ~ "Actividades de atención de la salud humana y de asistencia social",
                       b14_rev4cl_caenes == 18 ~ "Actividades artísticas, de entretenimiento y recreativas",
                       b14_rev4cl_caenes == 19 ~ "Otras actividades de servicios",
                       b14_rev4cl_caenes == 20 ~ "Actividades de los hogares como empleadores",
                       b14_rev4cl_caenes == 21 ~ "Actividades de organizaciones y órganos extraterritoriales"),
      rama = factor(rama, 
                    levels = c("Agricultura, ganadería, silvicultura y pesca",
                               "Explotación de minas y canteras", 
                               "Industrias manufactureras",
                               "Suministro de electricidad, gas, vapor y aire acondicionado",
                               "Suministro de agua",
                               "Construcción",
                               "Comercio al por mayor y al por menor",
                               "Transporte y almacenamiento",
                               "Actividades de alojamiento y de servicio de comidas",
                               "Información y comunicaciones",
                               "Actividades financieras y de seguros",
                               "Actividades inmobiliarias",
                               "Actividades profesionales, científicas y técnicas",
                               "Actividades de servicios administrativos y de apoyo",
                               "Administración pública y defensa",
                               "Enseñanza",
                               "Actividades de atención de la salud humana y de asistencia social",
                               "Actividades artísticas, de entretenimiento y recreativas",
                               "Otras actividades de servicios",
                               "Actividades de los hogares como empleadores",
                               "Actividades de organizaciones y órganos extraterritoriales")),
      ciuo08 = case_when(b1 == 1 ~ "Directores y gerentes",
                         b1 == 2 ~ "Profesionales científicos e intelectuales",
                         b1 == 3 ~ "Técnicos y profesionales de nivel medio",
                         b1 == 4 ~ "Personal de apoyo administrativo",
                         b1 == 5 ~ "Trabajadores de los servicios y vendedores de comercios y mercados",
                         b1 == 6 ~ "Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros",
                         b1 == 7 ~ "Oficiales, operarios y artesanos de artes mecánicas y de otros oficios",
                         b1 == 8 ~ "Operadores de instalaciones y máquinas y ensambladores",
                         b1 == 9 ~ "Ocupaciones elementales",
                         b1 == 10 ~ "Otros no identificados",
                         TRUE ~ NA_character_),
      ciuo08 = factor(ciuo08, 
                      levels = c("Directores y gerentes",
                                 "Profesionales científicos e intelectuales",
                                 "Técnicos y profesionales de nivel medio",
                                 "Personal de apoyo administrativo",
                                 "Trabajadores de los servicios y vendedores de comercios y mercados",
                                 "Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros",
                                 "Oficiales, operarios y artesanos de artes mecánicas y de otros oficios",
                                 "Operadores de instalaciones y máquinas y ensambladores",
                                 "Ocupaciones elementales",
                                 "Otros no identificados")),
      tam_emp = factor(b15_1,
                       levels = c(1,2,3,4,5),
                       labels = c("Menos de 5 trabajadores", 
                                  "Micro empresa (entre 5 y 9 trabajadores)", 
                                  "Pequeña empresa (entre 10 y 49 trabajadores)", 
                                  "Mediana empresa (entre 50 y 199 trabajadores)",
                                  "Gran empresa (200 o más trabajadores)")))
  i <- i+1
}


ene <- lapply(ene, fun_trim)

# 4. Guardar y exportar ---------------------------------------------------

save(proc_db, file = here("output", "proc_data_ene.RData"))
save(ene, file = here("output", "ene_transversal.RData"))

