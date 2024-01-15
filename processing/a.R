files <- paste0("O:/1. Bases de Datos (Encuestas)/ENE/STATA/Base Censo2017/Bases rectificadas/ene-202310.dta")

base <- rio::import(files) %>% 
  sjlabelled::remove_all_labels(.) %>% 
  mutate(
    across(.cols = c(habituales, c3_3, efectivas, c2_2_3),
           .fns = ~ set_na(., na = c(888, 999))),
    asal_priv = if_else(categoria_ocupacion == 3, 1, 0),
    formal = if_else(ocup_form == 1, 1, 0),
    hab_t = case_when(habituales >= 1 & habituales < 31 ~ "1 a 30",
                      habituales > 30 & habituales < 40 ~ "31 a 39",
                      habituales == 40 ~ "40",
                      habituales > 40 & habituales < 45 ~ "41 a 44",
                      habituales == 45 ~ "45",
                      habituales > 45 ~ "46 o más"),
    cnt_t = case_when(c3_3 >= 1 & c3_3 < 31 ~ "1 a 30",
                      c3_3 > 30 & c3_3 < 40 ~ "31 a 39",
                      c3_3 == 40 ~ "40",
                      c3_3 > 40 & c3_3 < 45 ~ "41 a 44",
                      c3_3 == 45 ~ "45",
                      c3_3 > 45 ~ "46 o más"),
    efc_t = case_when(efectivas >= 1 & efectivas < 31 ~ "1 a 30",
                      efectivas > 30 & efectivas < 40 ~ "31 a 39",
                      efectivas == 40 ~ "40",
                      efectivas > 40 & efectivas < 45 ~ "41 a 44",
                      efectivas == 45 ~ "45",
                      efectivas > 45 ~ "46 o más"),
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


# medida general de horas de trabajo total promedio (suma habituales de primera y segunda ocupacion en caso de que tenga)

tbl1 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1) %>% 
  group_by(ano_trimestre, mes_central) %>% 
  summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"),
            mediana = survey_median(habituales_dos, na.rm = T, vartype = "se"),
            ds = survey_sd(habituales_dos, na.rm = T),
            varianza = survey_var(habituales_dos, na.rm = T, vartype = NULL))


# medida de horas de trabajo total promedio segun tener o no segunda ocup

tbl2 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1) %>% 
  group_by(ano_trimestre, mes_central, seg_ocup) %>% 
  summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"),
            mediana = survey_median(habituales_dos, na.rm = T, vartype = "se"),
            ds = survey_sd(habituales_dos, na.rm = T),
            varianza = survey_var(habituales_dos, na.rm = T, vartype = NULL))


# medida de horas de trabajo total promedio segun sexo

tbl3 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1) %>% 
  group_by(ano_trimestre, mes_central, sexo) %>% 
  summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"),
            mediana = survey_median(habituales_dos, na.rm = T, vartype = "se"),
            ds = survey_sd(habituales_dos, na.rm = T),
            varianza = survey_var(habituales_dos, na.rm = T, vartype = NULL))


# medida de horas de trabajo total promedio segun edad

tbl4 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1) %>% 
  group_by(ano_trimestre, mes_central, edad_t) %>% 
  summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"),
            mediana = survey_median(habituales_dos, na.rm = T, vartype = "se"),
            ds = survey_sd(habituales_dos, na.rm = T),
            varianza = survey_var(habituales_dos, na.rm = T, vartype = NULL))

# medida de horas de trabajo total promedio segun educacion

tbl5 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1) %>% 
  group_by(ano_trimestre, mes_central, cine) %>% 
  summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"),
            mediana = survey_median(habituales_dos, na.rm = T, vartype = "se"),
            ds = survey_sd(habituales_dos, na.rm = T),
            varianza = survey_var(habituales_dos, na.rm = T, vartype = NULL))

# medida de horas de trabajo total promedio segun grupo ocupacional

tbl6 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1) %>% 
  group_by(ano_trimestre, mes_central, ciuo08) %>% 
  summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"),
            mediana = survey_median(habituales_dos, na.rm = T, vartype = "se"),
            ds = survey_sd(habituales_dos, na.rm = T),
            varianza = survey_var(habituales_dos, na.rm = T, vartype = NULL))


# medida de horas de trabajo total promedio segun tamaño empresa

tbl7 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1) %>% 
  group_by(ano_trimestre, mes_central, tam_emp) %>% 
  summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"),
            mediana = survey_median(habituales_dos, na.rm = T, vartype = "se"),
            ds = survey_sd(habituales_dos, na.rm = T),
            varianza = survey_var(habituales_dos, na.rm = T, vartype = NULL))

# medida de horas de trabajo total promedio segun rama

tbl8 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1) %>% 
  group_by(ano_trimestre, mes_central, rama) %>% 
  summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"),
            mediana = survey_median(habituales_dos, na.rm = T, vartype = "se"),
            ds = survey_sd(habituales_dos, na.rm = T),
            varianza = survey_var(habituales_dos, na.rm = T, vartype = NULL))

# medida de horas de trabajo total promedio segun si tiene o no 2da ocup y sexo

tbl9 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1) %>% 
  group_by(ano_trimestre, mes_central, seg_ocup, sexo) %>% 
  summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"),
            mediana = survey_median(habituales_dos, na.rm = T, vartype = "se"),
            ds = survey_sd(habituales_dos, na.rm = T),
            varianza = survey_var(habituales_dos, na.rm = T, vartype = NULL))

# medida de horas de trabajo total promedio segun si tiene o no 2da ocup y grupo ocupacional

tbl10 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1) %>% 
  group_by(ano_trimestre, mes_central, seg_ocup, ciuo08) %>% 
  summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"),
            mediana = survey_median(habituales_dos, na.rm = T, vartype = "se"),
            ds = survey_sd(habituales_dos, na.rm = T),
            varianza = survey_var(habituales_dos, na.rm = T, vartype = NULL))

# medida de horas de trabajo total promedio segun si tiene o no 2da ocup y rama

tbl11 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1) %>% 
  group_by(ano_trimestre, mes_central, seg_ocup, rama) %>% 
  summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"),
            mediana = survey_median(habituales_dos, na.rm = T, vartype = "se"),
            ds = survey_sd(habituales_dos, na.rm = T),
            varianza = survey_var(habituales_dos, na.rm = T, vartype = NULL))

# medida de horas de trabajo total promedio segun si tiene o no 2da ocup y tam emp

tbl12 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1) %>% 
  group_by(ano_trimestre, mes_central, seg_ocup, tam_emp) %>% 
  summarise(media = survey_mean(habituales_dos, na.rm = T, vartype = "ci"),
            mediana = survey_median(habituales_dos, na.rm = T, vartype = "se"),
            ds = survey_sd(habituales_dos, na.rm = T),
            varianza = survey_var(habituales_dos, na.rm = T, vartype = NULL))

# tramo horas de trabajo total 

tbl13 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos)) %>% 
  group_by(ano_trimestre, mes_central, hab_t_dos) %>% 
  summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
  mutate(prop = prop.table(total)*100)

# tramo horas de trabajo total segun sexo

tbl14 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(sexo)) %>% 
  group_by(ano_trimestre, mes_central, hab_t_dos, sexo) %>% 
  summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
  ungroup() %>%
  group_by(sexo) %>% 
  mutate(prop = prop.table(total)*100)

  
# tramo horas de trabajo total segun grupo ocupacional

tbl15 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(ciuo08)) %>% 
  group_by(ano_trimestre, mes_central, hab_t_dos, ciuo08) %>% 
  summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
  ungroup() %>%
  group_by(ciuo08) %>% 
  mutate(prop = prop.table(total)*100)

# tramo horas de trabajo total segun rama

tbl16 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(rama)) %>% 
  group_by(ano_trimestre, mes_central, hab_t_dos, rama) %>% 
  summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
  ungroup() %>%
  group_by(rama) %>% 
  mutate(prop = prop.table(total)*100)
  
# tramo horas de trabajo total segun tam emp

tbl17 <- base_pond %>% 
  filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(tam_emp)) %>% 
  group_by(ano_trimestre, mes_central, hab_t_dos, tam_emp) %>% 
  summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
  ungroup() %>%
  group_by(tam_emp) %>% 
  mutate(prop = prop.table(total)*100)

assign(paste0("tbl1_", periodo), tbl1)
assign(paste0("tbl2_", periodo), tbl2)
assign(paste0("tbl3_", periodo), tbl3)
assign(paste0("tbl4_", periodo), tbl4)
assign(paste0("tbl5_", periodo), tbl5)
assign(paste0("tbl6_", periodo), tbl6)
assign(paste0("tbl7_", periodo), tbl7)
assign(paste0("tbl8_", periodo), tbl8)
assign(paste0("tbl9_", periodo), tbl9)
assign(paste0("tbl10_", periodo), tbl10)
assign(paste0("tbl11_", periodo), tbl11)
assign(paste0("tbl12_", periodo), tbl12)
assign(paste0("tbl13_", periodo), tbl13)
assign(paste0("tbl14_", periodo), tbl14)
assign(paste0("tbl15_", periodo), tbl15)
assign(paste0("tbl16_", periodo), tbl16)
assign(paste0("tbl17_", periodo), tbl17)