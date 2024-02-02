# Codigo 40 horas: Analísis de datos
# Autor: Andreas Laffert


# 1. Librerías -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, 
               here, 
               sjmisc, 
               srvyr, 
               sjPlot, 
               rstatix, 
               psych, 
               nortest,
               summarytools,
               forecast,
               tseries,
               ggpubr,
               rempsyc,
               broom)

options(scipen=999)
options(survey.lonely.psu = "certainty")
rm(list = ls())


# 2. Datos ----------------------------------------------------------------

load(file = here("output", "proc_data_ene.RData"))
load(file = here("output", "ene_transversal.RData"))

# 3. Analisis -------------------------------------------------------------


miles <- function(x) {
  format(round(as.numeric(x),0), big.mark = ".")
}

decimales <- function(x) {
  format(round(as.numeric(x), 2), decimal.mark = ",")
}

# set theme

windowsFonts(`Times New Roman` = windowsFont("Times New Roman"))

theme_nice <- function() {
  theme_bw(base_family = "Times New Roman", base_size = 13.5) +
    theme(legend.position = "top",
          axis.title = element_text(size = 13.5),
          axis.text.y = element_text(size = rel(1.1)),
          plot.title = element_text(face = "bold", size = 13.5),
          axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     size = rel(1.1)))
  
  
}

theme_set(theme_nice())

ene <- lapply(ene, function(x){
  as.data.frame(x) %>% 
    filter(asal_priv == 1 & formal == 1)
})

ene_pond <- list(
  as_survey_design(.data = ene[[1]], ids = 1, strata = estrato, weights = fact_cal),
  as_survey_design(.data = ene[[2]], ids = 1, strata = estrato, weights = fact_cal)
)


# 3.1 Parte A -------------------------------------------------------------

## Evolucion media horas habituales 2017-2023

# convertir en serie de tiempo
t_serie1 <- ts(proc_db[[1]]$media, start = c(2017, 8), end = c(2023, 10), frequency = 12)

# descriptivo
psych::describe(proc_db[[1]]$media, 
                quant = c(.25,.75),
                skew = T, 
                ranges = T, 
                IQR = T)

# normalidad 

# visual
hist(proc_db[[1]]$media)

proc_db[[1]] %>% 
  ggplot(aes(x = media)) +
  geom_density(fill = "#F6C848") # bimodal

ggplot(proc_db[[1]], aes(media)) + 
  geom_histogram(aes(x = media, y = ..density..), bins = 50, color = "black", fill = "grey") +
  geom_density(color = "blue") +
  stat_function(fun = dnorm, args = list(mean = mean(proc_db[[1]]$media), sd = sd(proc_db[[1]]$media)))

ggpubr::ggqqplot(proc_db[[1]]$media)

# test
shapiro.test(proc_db[[1]]$media) # no es normal

# Visualizar

n <- nrow(proc_db[[1]])
time_points <- 1:n

m1 <- tslm(formula = t_serie1 ~ time_points)

plot(t_serie1)


## Evolucion tramos horas habituales 2017-2023


trims <- c(paste0(2017:2023, " son"), paste0(2017:2023, " nde"))

proc_db[[2]][c(1:3, 7, 10)] %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  ggplot(aes(x = anotrim, y = prop, group = hab_t, color = hab_t)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = function(prop){paste0(prop, "%")}, n.breaks = 8) +
  scale_x_discrete(breaks = trims) +
  labs(y = "% de trabajadores",
       x = NULL,
       color = "Tramos horas habituales",
       caption = "Fuente: Elaboración propia en base a ENE.") 



## EVolucion y tendencias tramos horas habituales 2020-2023


# Tramos 40 y 41-44 
db <- proc_db[[12]][c(1:3, 7, 10)] %>% 
  filter(hab_t %in% c("40", "41 a 44") & ano_trimestre >= 2022) %>% 
  mutate(hab_t = str_replace_all(hab_t, "\\s", "")) %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = hab_t,
              values_from = prop,
              names_glue = "{.value}_{hab_t}") %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim)),
         time = row_number())

db1 <- db %>% 
  dplyr::select(prop_40, prop_41a44, time) %>% 
  filter(time <= 12) %>% 
  rename(prop_40_1 = prop_40, 
         prop_41a44_1 = prop_41a44)

db2 <- db %>% 
  dplyr::select(prop_40, prop_41a44, time) %>% 
  filter(time >= 13) %>% 
  rename(prop_40_2 = prop_40, 
         prop_41a44_2 = prop_41a44)

db3 <- full_join(db1, db2)

db <- full_join(db[c(3,6)], db3)

m2 <- lm(formula = prop_40_1 ~ time, data = db)
m3 <- lm(formula = prop_40_2 ~ time, data = db)

m4 <- lm(formula = prop_41a44_1 ~ time, data = db)
m5 <- lm(formula = prop_41a44_2 ~ time, data = db)


# Tramos 45 46 o mas

db <- proc_db[[12]][c(1:3, 7, 10)] %>% 
  filter(hab_t %in% c("45", "46 o más") & ano_trimestre >= 2022) %>% 
  mutate(hab_t = str_replace_all(hab_t, "\\s", "")) %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = hab_t,
              values_from = prop,
              names_glue = "{.value}_{hab_t}") %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim)),
         time = row_number())

db1 <- db %>% 
  dplyr::select(prop_45, prop_46omás, time) %>% 
  filter(time <= 12) %>% 
  rename(prop_45_1 = prop_45, 
         prop_46omás_1 = prop_46omás)

db2 <- db %>% 
  dplyr::select(prop_45, prop_46omás, time) %>% 
  filter(time >= 13) %>% 
  rename(prop_45_2 = prop_45, 
         prop_46omás_2 = prop_46omás)

db3 <- full_join(db1, db2)

db <- full_join(db[c(3,6)], db3)

m6 <- lm(formula = prop_45_1 ~ time, data = db)
m7 <- lm(formula = prop_45_2 ~ time, data = db)

m8 <- lm(formula = prop_46omás_1 ~ time, data = db)
m9 <- lm(formula = prop_46omás_2 ~ time, data = db)



## SEXO


df <- proc_db[[13]] %>% 
  filter(hab_t %in% c("40", "41 a 44", "45", "46 o más")) %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  pivot_wider(id_cols = c(anotrim, sexo),
              names_from = hab_t,
              values_from = prop,
              names_glue = "{.value}_{hab_t}")

df$dif_40 <- NA
df$dif_41_44 <- NA
df$dif_45 <- NA
df$dif_46_mas <- NA

df[25:nrow(df),7:10] <- df[25:nrow(df),3:6] - df[1:(nrow(df)-24),3:6]

df <- df %>% 
  select(anotrim, sexo, starts_with("dif")) %>% 
  na.omit()

df

df$acum_40 <- ave(df$dif_40, df$sexo, FUN = cumsum)
df$acum_41_44 <- ave(df$dif_41_44, df$sexo, FUN = cumsum)
df$acum_45 <- ave(df$dif_45, df$sexo, FUN = cumsum)
df$acum_46_mas <- ave(df$dif_46_mas, df$sexo, FUN = cumsum)


df %>% 
  select(anotrim, sexo, starts_with("acum")) %>% 
  pivot_longer(cols = -c(1,2),
               names_to = "names",
               values_to = "value") %>% 
  na.omit() %>% 
  ggplot(aes(x = anotrim, y = value, group = names, color = names)) +
  geom_line() +
  facet_wrap(~sexo)


## EDAD


#proc_db[[7]] media

# Media

proc_db[[7]] %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>%
  ggplot(aes(x = anotrim, y = media, color = edad_t, group = edad_t)) +
  geom_line() # graficar

proc_db[[7]] %>% 
  group_by(edad_t) %>% 
  summarise(m = mean(media))

#proc_db[[14]] tramos

df <- proc_db[[14]] %>% 
  filter(hab_t %in% c("40", "41 a 44", "45", "46 o más")) %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  pivot_wider(id_cols = c(anotrim, edad_t),
              names_from = hab_t,
              values_from = prop,
              names_glue = "{.value}_{hab_t}")

df$dif_40 <- NA
df$dif_41_44 <- NA
df$dif_45 <- NA
df$dif_46_mas <- NA

df[49:nrow(df),7:10] <- df[49:nrow(df),3:6] - df[1:(nrow(df)-48),3:6]

df <- df %>% 
  select(anotrim, edad_t, starts_with("dif")) %>% 
  na.omit()

df

df$acum_40 <- ave(df$dif_40, df$edad_t, FUN = cumsum)
df$acum_41_44 <- ave(df$dif_41_44, df$edad_t, FUN = cumsum)
df$acum_45 <- ave(df$dif_45, df$edad_t, FUN = cumsum)
df$acum_46_mas <- ave(df$dif_46_mas, df$edad_t, FUN = cumsum)


df %>% 
  select(anotrim, edad_t, starts_with("acum")) %>% 
  pivot_longer(cols = -c(1,2),
               names_to = "names",
               values_to = "value") %>% 
  na.omit() %>% 
  ggplot(aes(x = anotrim, y = value, group = names, color = names)) +
  geom_line() +
  facet_wrap(~edad_t)

## EDUCACION 


# proc_db[[8]] media

proc_db[[8]] %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  na.omit() %>% 
  ggplot(aes(x = anotrim, y = media, group = cine, color = cine)) +
  geom_line()

## Revisar descomposicion


# pro_db[[15]] tramos

df <- proc_db[[15]] %>% 
  filter(hab_t %in% c("40", "41 a 44", "45", "46 o más")) %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  pivot_wider(id_cols = c(anotrim, cine),
              names_from = hab_t,
              values_from = prop,
              names_glue = "{.value}_{hab_t}")

df$dif_40 <- NA
df$dif_41_44 <- NA
df$dif_45 <- NA
df$dif_46_mas <- NA

df[49:nrow(df),7:10] <- df[49:nrow(df),3:6] - df[1:(nrow(df)-48),3:6]

df <- df %>% 
  select(anotrim, cine, starts_with("dif")) %>% 
  na.omit()

df

df$acum_40 <- ave(df$dif_40, df$cine, FUN = cumsum)
df$acum_41_44 <- ave(df$dif_41_44, df$cine, FUN = cumsum)
df$acum_45 <- ave(df$dif_45, df$cine, FUN = cumsum)
df$acum_46_mas <- ave(df$dif_46_mas, df$cine, FUN = cumsum)


df %>% 
  select(anotrim, cine, starts_with("acum")) %>% 
  pivot_longer(cols = -c(1,2),
               names_to = "names",
               values_to = "value") %>% 
  na.omit() %>% 
  mutate(names = case_when(names == "acum_40" ~ "40",
                           names == "acum_41_44" ~ "41 a 44",
                           names == "acum_45" ~ "45",
                           names == "acum_46_mas" ~ "46 o más")) %>% 
  ggplot(aes(x = anotrim, y = value, group = names, color = names)) +
  geom_line(linewidth = 0.7) +
  geom_vline(aes(xintercept = "2023 mam"), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), linetype = "solid", color = "black")+
  facet_wrap(~cine) +
  scale_y_continuous(labels = function(prop){paste0(prop, "%")}, 
                     n.breaks = 5) +
  scale_x_discrete(breaks = c(trims, "2023 mam")) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(y = "Variación anual acumulada",
       x = NULL,
       color = "Tramos horas habituales",
       caption = "Fuente: Elaboración propia en base a ENE.")


## CIUO 08

#proc[[9]]

proc_db[[9]] %>% 
  select(anotrim, ciuo08, media, media_low, media_upp) %>% 
  filter(ciuo08 != "Otros no identificados") %>% 
  na.omit() %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim)),
         ciuo08 = as.character(ciuo08),
         ciuo08 = str_trunc(ciuo08, 40, "right"),
         ciuo08 = factor(ciuo08, levels = unique(ciuo08))) %>% 
  ggplot(aes(x = anotrim, y = media, group = 1)) +
  geom_line(linewidth = 1, color = "#377EB8") +
  geom_ribbon(aes(ymin = media_low, ymax = media_upp), alpha = 0.1, fill = "blue") +
  scale_y_continuous(labels = decimales, n.breaks = 5) +
  scale_x_discrete(breaks = trims) +
  facet_wrap(~ciuo08) +
  labs(y = "Promedio horas habituales",
       x = NULL,
       caption = "Fuente: Elaboración propia en base a ENE.")


#proc[[16]]

proc_db[[16]] %>% 
  filter(hab_t %in% c("40", "41 a 44", "45", "46 o más") & ciuo08 != "Otros no identificados") %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  ggplot(aes(x = anotrim, y = prop, group = hab_t, color = hab_t)) +
  geom_line() +
  facet_wrap(~ciuo08)




df <- proc_db[[16]] %>% 
  filter(hab_t %in% c("40", "41 a 44", "45", "46 o más") & ciuo08 != "Otros no identificados") %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  pivot_wider(id_cols = c(anotrim, ciuo08),
              names_from = hab_t,
              values_from = prop,
              names_glue = "{.value}_{hab_t}")

df$dif_40 <- NA
df$dif_41_44 <- NA
df$dif_45 <- NA
df$dif_46_mas <- NA

df[109:nrow(df),7:10] <- df[109:nrow(df),3:6] - df[1:(nrow(df)-108),3:6]

df <- df %>% 
  select(anotrim, ciuo08, starts_with("dif")) %>% 
  na.omit()

df

df$acum_40 <- ave(df$dif_40, df$ciuo08, FUN = cumsum)
df$acum_41_44 <- ave(df$dif_41_44, df$ciuo08, FUN = cumsum)
df$acum_45 <- ave(df$dif_45, df$ciuo08, FUN = cumsum)
df$acum_46_mas <- ave(df$dif_46_mas, df$ciuo08, FUN = cumsum)


df %>% 
  select(anotrim, ciuo08, starts_with("acum")) %>% 
  pivot_longer(cols = -c(1,2),
               names_to = "names",
               values_to = "value") %>% 
  na.omit() %>% 
  mutate(names = case_when(names == "acum_40" ~ "40",
                           names == "acum_41_44" ~ "41 a 44",
                           names == "acum_45" ~ "45",
                           names == "acum_46_mas" ~ "46 o más")) %>% 
  ggplot(aes(x = anotrim, y = value, group = names, color = names)) +
  geom_line(linewidth = 0.7) +
  geom_vline(aes(xintercept = "2023 mam"), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), linetype = "solid", color = "black")+
  facet_wrap(~ciuo08) +
  scale_y_continuous(labels = function(prop){paste0(prop, "%")}, 
                     breaks = seq(-50, 100, by = 25)) +
  scale_x_discrete(breaks = c(trims, "2023 mam")) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(y = "Variación anual acumulada",
       x = NULL,
       color = "Tramos horas habituales",
       caption = "Fuente: Elaboración propia en base a ENE.")


# Tamaño

proc_db[[10]] %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  na.omit() %>% 
  ggplot(aes(x = anotrim, y = media, group = tam_emp, color = tam_emp)) +
  geom_line()

df <- proc_db[[18]] %>% 
  filter(hab_t %in% c("40", "41 a 44", "45", "46 o más")) %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  pivot_wider(id_cols = c(anotrim, tam_emp),
              names_from = hab_t,
              values_from = prop,
              names_glue = "{.value}_{hab_t}")

df$dif_40 <- NA
df$dif_41_44 <- NA
df$dif_45 <- NA
df$dif_46_mas <- NA

df[61:nrow(df),7:10] <- df[61:nrow(df),3:6] - df[1:(nrow(df)-60),3:6]

df <- df %>% 
  select(anotrim, tam_emp, starts_with("dif")) %>% 
  na.omit()

df

df$acum_40 <- ave(df$dif_40, df$tam_emp, FUN = cumsum)
df$acum_41_44 <- ave(df$dif_41_44, df$tam_emp, FUN = cumsum)
df$acum_45 <- ave(df$dif_45, df$tam_emp, FUN = cumsum)
df$acum_46_mas <- ave(df$dif_46_mas, df$tam_emp, FUN = cumsum)


df %>% 
  select(anotrim, tam_emp, starts_with("acum")) %>% 
  pivot_longer(cols = -c(1,2),
               names_to = "names",
               values_to = "value") %>% 
  na.omit() %>% 
  mutate(names = case_when(names == "acum_40" ~ "40",
                           names == "acum_41_44" ~ "41 a 44",
                           names == "acum_45" ~ "45",
                           names == "acum_46_mas" ~ "46 o más")) %>% 
  ggplot(aes(x = anotrim, y = value, group = names, color = names)) +
  geom_line(linewidth = 0.7) +
  geom_vline(aes(xintercept = "2023 mam"), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), linetype = "solid", color = "black")+
  facet_wrap(~tam_emp, ncol = 2) +
  scale_y_continuous(labels = function(prop){paste0(prop, "%")}, 
                     breaks = seq(-50, 100, by = 25)) +
  scale_x_discrete(breaks = c(trims, "2023 mam")) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(y = "Variación anual acumulada",
       x = NULL,
       color = "Tramos horas habituales",
       caption = "Fuente: Elaboración propia en base a ENE.")


# RAMA

proc_db[[11]] %>% 
  select(anotrim, rama, media, media_low, media_upp) %>% 
  na.omit() %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim)),
         rama = as.character(rama),
         rama = str_trunc(rama, 30, "right"),
         rama = factor(rama, levels = unique(rama))) %>% 
  ggplot(aes(x = anotrim, y = media, group = 1)) +
  geom_line(linewidth = 1, color = "#377EB8") +
  geom_ribbon(aes(ymin = media_low, ymax = media_upp), alpha = 0.1, fill = "blue") +
  scale_y_continuous(labels = decimales, n.breaks = 5) +
  scale_x_discrete(breaks = trims) +
  facet_wrap(~rama) +
  labs(y = "Promedio horas habituales",
       x = NULL,
       caption = "Fuente: Elaboración propia en base a ENE.") # hacer tabla



df <- proc_db[[17]] %>% 
  filter(hab_t %in% c("40", "41 a 44", "45", "46 o más")) %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  pivot_wider(id_cols = c(anotrim, rama),
              names_from = hab_t,
              values_from = prop,
              names_glue = "{.value}_{hab_t}")

df$dif_40 <- NA
df$dif_41_44 <- NA
df$dif_45 <- NA
df$dif_46_mas <- NA

df[248:nrow(df),7:10] <- df[248:nrow(df),3:6] - df[1:(nrow(df)-247),3:6]

df <- df %>% 
  select(anotrim, rama, starts_with("dif")) %>% 
  na.omit()

df

df$acum_40 <- ave(df$dif_40, df$rama, FUN = cumsum)
df$acum_41_44 <- ave(df$dif_41_44, df$rama, FUN = cumsum)
df$acum_45 <- ave(df$dif_45, df$rama, FUN = cumsum)
df$acum_46_mas <- ave(df$dif_46_mas, df$rama, FUN = cumsum)


df %>% 
  select(anotrim, rama, starts_with("acum")) %>% 
  pivot_longer(cols = -c(1,2),
               names_to = "names",
               values_to = "value") %>% 
  na.omit() %>% 
  mutate(names = case_when(names == "acum_40" ~ "40",
                           names == "acum_41_44" ~ "41 a 44",
                           names == "acum_45" ~ "45",
                           names == "acum_46_mas" ~ "46 o más"),
         rama = as.character(rama),
         rama = str_trunc(rama, 30, "right"),
         rama = factor(rama, levels = unique(rama))) %>% 
  ggplot(aes(x = anotrim, y = value, group = names, color = names)) +
  geom_line(linewidth = 0.7) +
  geom_vline(aes(xintercept = "2023 mam"), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), linetype = "solid", color = "black")+
  facet_wrap(~rama, scales = "free_y") +
  scale_y_continuous(labels = function(prop){paste0(prop, "%")}, 
                     breaks = seq(-50, 100, by = 25)) +
  scale_x_discrete(breaks = c(trims, "2023 mam")) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(y = "Variación anual acumulada",
       x = NULL,
       color = "Tramos horas habituales",
       caption = "Fuente: Elaboración propia en base a ENE.")




##### HORAS TOTALES HABITUALES ####





## ****************** ##
## PROPRPORCIONES     ##
## ****************** ##


## Diferencias de proporciones tramos entre 2022 y 2023

# 40 horas

p_data <- bind_rows(
  ene[[1]] %>% 
    dplyr::select(anotrim, hab_t, fact_cal) %>% 
    mutate(hab_t = if_else(hab_t == "40", "40", "Otros")),
  ene[[2]] %>% 
    dplyr::select(anotrim, hab_t, fact_cal) %>% 
    mutate(hab_t = if_else(hab_t == "40", "40", "Otros"))
) %>% na.omit()

p_data$anotrim <- as.factor(p_data$anotrim)

table(p_data$hab_t, p_data$anotrim)

xtab <- as.table(rbind(c(1174, 756),c(18096, 16446))) # invertir para test

dimnames(xtab) <- list(
  tramo = c("si", "no"),
  anotrim = c("2023", "2022")
)
xtab

p_results <- prop_test(xtab, alternative = "greater", detailed = T) %>% 
  mutate(tramo = "40")

# 41 a 44

p_data2 <- bind_rows(
  ene[[1]] %>% 
    dplyr::select(anotrim, hab_t, fact_cal) %>% 
    mutate(hab_t = if_else(hab_t == "41 a 44", "41 a 44", "Otros")),
  ene[[2]] %>% 
    dplyr::select(anotrim, hab_t, fact_cal) %>% 
    mutate(hab_t = if_else(hab_t == "41 a 44", "41 a 44", "Otros"))
) %>% na.omit()

p_data2$anotrim <- as.factor(p_data2$anotrim)

table(p_data2$hab_t, p_data2$anotrim)

xtab <- as.table(rbind(c(2322, 2042),c(16948, 15160))) # invertir para test

dimnames(xtab) <- list(
  tramo = c("si", "no"),
  anotrim = c("2023", "2022")
)

xtab

p_results <- bind_rows(
  p_results,
  prop_test(xtab, alternative = "greater", detailed = T) %>% 
    mutate(tramo = "41 a 44")
)

# 45

p_data3 <- bind_rows(
  ene[[1]] %>% 
    dplyr::select(anotrim, hab_t, fact_cal) %>% 
    mutate(hab_t = if_else(hab_t == "45", "45", "Otros")),
  ene[[2]] %>% 
    dplyr::select(anotrim, hab_t, fact_cal) %>% 
    mutate(hab_t = if_else(hab_t == "45", "45", "Otros"))
) %>% na.omit()

p_data3$anotrim <- as.factor(p_data3$anotrim)

table(p_data3$hab_t, p_data3$anotrim)

xtab <- as.table(rbind(c(11852, 10966),c(7418, 6236))) # invertir para test

dimnames(xtab) <- list(
  tramo = c("si", "no"),
  anotrim = c("2023", "2022")
)

xtab

p_results <- bind_rows(
  p_results,
  prop_test(xtab, alternative = "less", detailed = T) %>% 
    mutate(tramo = "45")
)

# 46 o mas

p_data4 <- bind_rows(
  ene[[1]] %>% 
    dplyr::select(anotrim, hab_t, fact_cal) %>% 
    mutate(hab_t = if_else(hab_t == "46 o más", "46 o más", "Otros")),
  ene[[2]] %>% 
    dplyr::select(anotrim, hab_t, fact_cal) %>% 
    mutate(hab_t = if_else(hab_t == "46 o más", "46 o más", "Otros"))
) %>% na.omit()

p_data4$anotrim <- as.factor(p_data4$anotrim)

table(p_data4$hab_t, p_data4$anotrim)

xtab <- as.table(rbind(c(2107, 1861),c(17163, 15341))) # invertir para test

dimnames(xtab) <- list(
  tramo = c("si", "no"),
  anotrim = c("2023", "2022")
)

xtab

p_results <- bind_rows(
  p_results,
  prop_test(xtab, alternative = "less", detailed = T) %>% 
    mutate(tramo = "46 o más")
)

p_results <- p_results %>% 
  mutate(alternative = if_else(alternative == "greater", "Mayor que", "Menor que")) %>% 
  dplyr::select(
    Tramo = tramo, 
    `Proporción 2023` = estimate1,
    `Proporción 2022` = estimate2,
    Estadístico = statistic, 
    p, 
    `Dirección` = alternative) 





## Diferencias de proporciones tramos por sexo entre 2022 y 2023


# Mujeres

# 40 horas
p_m <- bind_rows(
  ene[[1]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 2) %>% 
    mutate(hab_t = if_else(hab_t == "40", "40", "Otros")),
  ene[[2]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 2) %>% 
    mutate(hab_t = if_else(hab_t == "40", "40", "Otros"))
) %>% na.omit()

p_m$anotrim <- as.factor(p_m$anotrim)

table(p_m$hab_t, p_m$anotrim)

xtab <- as.table(rbind(c(623, 370),c(6848, 6254))) # invertir para test

dimnames(xtab) <- list(
  tramo = c("si", "no"),
  anotrim = c("2023", "2022")
)
xtab

prm1 <- prop_test(xtab, alternative = "greater", detailed = T)

# 41 a 44

p_m2 <- bind_rows(
  ene[[1]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 2) %>% 
    mutate(hab_t = if_else(hab_t == "41 a 44", "41 a 44", "Otros")),
  ene[[2]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 2) %>% 
    mutate(hab_t = if_else(hab_t == "41 a 44", "41 a 44", "Otros"))
) %>% na.omit()

p_m2$anotrim <- as.factor(p_m2$anotrim)

table(p_m2$hab_t, p_m2$anotrim)

xtab <- as.table(rbind(c(662, 587),c(6809, 6037))) # invertir para test

dimnames(xtab) <- list(
  tramo = c("si", "no"),
  anotrim = c("2023", "2022")
)
xtab

prm2 <- prop_test(xtab, alternative = "greater", detailed = T)

# 45

p_m3 <- bind_rows(
  ene[[1]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 2) %>% 
    mutate(hab_t = if_else(hab_t == "45", "45", "Otros")),
  ene[[2]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 2) %>% 
    mutate(hab_t = if_else(hab_t == "45", "45", "Otros"))
) %>% na.omit()

p_m3$anotrim <- as.factor(p_m3$anotrim)

table(p_m3$hab_t, p_m3$anotrim)

xtab <- as.table(rbind(c(4468, 4141),c(3003, 2483))) # invertir para test

dimnames(xtab) <- list(
  tramo = c("si", "no"),
  anotrim = c("2023", "2022")
)
xtab

prm3 <- prop_test(xtab, alternative = "less", detailed = T)

# 46

p_m4 <- bind_rows(
  ene[[1]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 2) %>% 
    mutate(hab_t = if_else(hab_t == "46 o más", "46 o más", "Otros")),
  ene[[2]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 2) %>% 
    mutate(hab_t = if_else(hab_t == "46 o más", "46 o más", "Otros"))
) %>% na.omit()

p_m4$anotrim <- as.factor(p_m4$anotrim)

table(p_m4$hab_t, p_m4$anotrim)

xtab <- as.table(rbind(c(627, 571),c(6844, 6053))) # invertir para test

dimnames(xtab) <- list(
  tramo = c("si", "no"),
  anotrim = c("2023", "2022")
)
xtab

prm4 <- prop_test(xtab, alternative = "less", detailed = T)

pm_results <- bind_rows(prm1, prm2, prm3, prm4)

nice_table(pm_results)

# Hombres

# 40 horas
p_h <- bind_rows(
  ene[[1]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 1) %>% 
    mutate(hab_t = if_else(hab_t == "40", "40", "Otros")),
  ene[[2]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 1) %>% 
    mutate(hab_t = if_else(hab_t == "40", "40", "Otros"))
) %>% na.omit()

p_h$anotrim <- as.factor(p_h$anotrim)

table(p_h$hab_t, p_h$anotrim)

xtab <- as.table(rbind(c(551, 386),c(11248, 10192))) # invertir para test

dimnames(xtab) <- list(
  tramo = c("si", "no"),
  anotrim = c("2023", "2022")
)
xtab

prh1 <- prop_test(xtab, alternative = "greater", detailed = T)

# 41 a 44

p_h2 <- bind_rows(
  ene[[1]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 1) %>% 
    mutate(hab_t = if_else(hab_t == "41 a 44", "41 a 44", "Otros")),
  ene[[2]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 1) %>% 
    mutate(hab_t = if_else(hab_t == "41 a 44", "41 a 44", "Otros"))
) %>% na.omit()

p_h2$anotrim <- as.factor(p_h2$anotrim)

table(p_h2$hab_t, p_h2$anotrim)

xtab <- as.table(rbind(c(1660, 1455),c(10139, 9123))) # invertir para test

dimnames(xtab) <- list(
  tramo = c("si", "no"),
  anotrim = c("2023", "2022")
)
xtab

prh2 <- prop_test(xtab, alternative = "greater", detailed = T)

# 45

p_h3 <- bind_rows(
  ene[[1]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 1) %>% 
    mutate(hab_t = if_else(hab_t == "45", "45", "Otros")),
  ene[[2]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 1) %>% 
    mutate(hab_t = if_else(hab_t == "45", "45", "Otros"))
) %>% na.omit()

p_h3$anotrim <- as.factor(p_h3$anotrim)

table(p_h3$hab_t, p_h3$anotrim)

xtab <- as.table(rbind(c(7384, 6825),c(4415, 3753))) # invertir para test

dimnames(xtab) <- list(
  tramo = c("si", "no"),
  anotrim = c("2023", "2022")
)
xtab

prh3 <- prop_test(xtab, alternative = "less", detailed = T)

# 46

p_h4 <- bind_rows(
  ene[[1]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 1) %>% 
    mutate(hab_t = if_else(hab_t == "46 o más", "46 o más", "Otros")),
  ene[[2]] %>% 
    dplyr::select(anotrim, sexo, hab_t, fact_cal) %>% 
    filter(sexo == 1) %>% 
    mutate(hab_t = if_else(hab_t == "46 o más", "46 o más", "Otros"))
) %>% na.omit()

p_h4$anotrim <- as.factor(p_h4$anotrim)

table(p_h4$hab_t, p_h4$anotrim)

xtab <- as.table(rbind(c(1480, 1290),c(10319, 9288))) # invertir para test

dimnames(xtab) <- list(
  tramo = c("si", "no"),
  anotrim = c("2023", "2022")
)
xtab

prh4 <- prop_test(xtab, alternative = "less", detailed = T)

ph_results <- bind_rows(prh1, prh2, prh3, prh4)

nice_table(ph_results)






data <- proc_db[[10]] %>% 
  select(anotrim, tam_emp, media) %>% 
  na.omit()

ts_data <- ts(data$media, frequency = 12, start = c(2020, 2))

stl_results <- lapply(split(data, data$tam_emp), function(subdata) {
  ts_subdata <- ts(subdata$media, frequency = 12, start = c(2020, 2))
  stl(ts_subdata, s.window = "periodic")
})

stl_results


autoplot(stl_results$`Menos de 5 trabajadores`)


data %>% 
  group_by(tam_emp) %>% 
  summarise(gm = mean(media))

