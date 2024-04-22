
pacman::p_load(lmtest, sandwich)

db <- proc_db[[12]][c(1:3, 7, 10)] %>% 
  filter(hab_t %in% c("41 a 44") & ano_trimestre >= 2022) %>% 
  mutate(hab_t = str_replace_all(hab_t, "\\s", "")) %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = hab_t,
              values_from = prop,
              names_glue = "{.value}_{hab_t}") %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim)),
         time = row_number())

db <- db %>% 
  select(ano_trimestre, mes_central, time, prop_41a44) %>% 
  mutate(cambio = if_else(time >= 13, 1, 0),
         cambio = as.factor(cambio))

m1 <- lm(formula = prop_41a44 ~ time, data = db)
m2 <- lm(formula = prop_41a44 ~ time*cambio, data = db)

summary(m2)

bptest(m2) # homocedasticidad se cumple

ggplot(db, aes(x = time, y = prop_41a44, color = cambio)) +
  geom_line(linetype = "longdash") +  # Puntos de datos observados
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "solid") +  # Línea de regresión antes del cambio
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "solid") +  # Línea de regresión después del cambio
  labs(x = "Tiempo", y = "Prop_41a44", color = "Cambio") +
  theme_minimal()

db <- proc_db[[12]][c(1:3, 7, 10)] %>% 
  filter(hab_t %in% c("41 a 44") & ano_trimestre >= 2020) %>% 
  mutate(hab_t = str_replace_all(hab_t, "\\s", "")) %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = hab_t,
              values_from = prop,
              names_glue = "{.value}_{hab_t}") %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim)),
         time = row_number())

db <- db %>% 
  select(ano_trimestre, mes_central, time, prop_41a44) %>% 
  mutate(cambio = if_else(time >= 36, 1, 0),
         cambio = as.factor(cambio))


pacman::p_load(strucchange)

baptisms.cus <- efp(durab ~ 1, type = "Rec-CUSUM")

plot(baptisms.cus)

sctest(baptisms.cus)

baptisms.mos <- efp(durab ~ 1, type = "OLS-MOSUM")

plot(baptisms.mos)

sctest(baptisms.mos)

# -




db <- ts(data = db$prop_41a44, start = c(2022, 1), end = c(2023, 10), frequency = 12)

m2 <- efp(db ~ 1, type = "Rec-CUSUM")

plot(m2)

summary(m2)

sctest(m2)

boundary(x = m2) %>% plot()

db <- proc_db[[12]][c(1:3, 7, 10)] %>% 
  filter(hab_t %in% c("41 a 44") & ano_trimestre >= 2022) %>% 
  mutate(hab_t = str_replace_all(hab_t, "\\s", "")) %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = hab_t,
              values_from = prop,
              names_glue = "{.value}_{hab_t}") %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim)),
         time = row_number())

db <- db %>% 
  select(ano_trimestre, mes_central, time, prop_41a44) %>% 
  mutate(cambio = if_else(time >= 13, 1, 0),
         cambio = as.factor(cambio))

m1 <- lm(formula = prop_41a44 ~ time, data = db)
m2 <- lm(formula = prop_41a44 ~ time*cambio, data = db)

summary(m2)

stargazer::stargazer(m2, type = "text")


db %>% 
  select(time, prop_41a44) %>% plot()


qlr <- Fstats(prop_41a44 ~ time, data = db)

breakpoints(qlr)
sctest(qlr, type = "supF")
plot(qlr)




am <- proc_db[[13]][c(1:4,8:9)] %>% 
  filter(sexo == 2) %>% 
  mutate(hab_t = str_replace_all(hab_t, "\\s", "")) %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = hab_t,
              values_from = prop,
              names_glue = "{.value}_{hab_t}") %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim)),
         time = row_number(),
         cambio = if_else(time >= 36, 1, 0),
         cambio = as.factor(cambio))


am %>% 
  ggplot(aes(x = time, y = prop_40)) +
  geom_line()


lm(formula = prop_40 ~ time*cambio, data = am) %>% summary()


df <- ts(data = am$prop_40, 
         start = c(2020, 2), 
         end = c(2023, 10), 
         frequency = 12)


sctest(efp(formula = prop_40 ~ time, data = am, type = "Rec-CUSUM"))


plot(efp(df ~ 1, type = "OLS-MOSUM"), alpha = 0.05)



am %>% 
  ggplot(aes(x = time, y = prop_40, color = cambio)) +
  geom_line(linetype = "longdash") +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = FALSE, 
              linetype = "solid") +
  geom_vline(mapping = aes(xintercept = 36)) +
  scale_x_continuous(limits = c(1, 45), 
                     breaks = 1:45) +
  scale_y_continuous(labels = function(prop){paste0(prop, "%")}, 
                     n.breaks = 5) +
  scale_color_manual(values = scales::hue_pal()(2), 
                     labels = c("Antes", "Después")) +  
  labs(y = "% trabajadores",
       x = NULL,
       color = "Cambio",
       caption = "Fuente: Elaboración propia en base a ENE.")




db <- left_join(
  oecd_long %>%
    filter(year >= 2002) %>% 
    rename(work_h = value),
  laborgdp_long %>%
    filter(year >= 2002) %>% 
    rename(labor_gdp = value)
) %>% 
  na.omit() %>% 
  select(-pais_name)

# Instala el paquete plm si aún no lo tienes instalado
install.packages("plm")

# Carga el paquete plm
library(plm)

# Carga tus datos (asegúrate de tener cargada la base de datos)

# Establece tus datos como un objeto de tipo pdata.frame
pdata <- pdata.frame(db, index = c("country", "year"))

# Estima un modelo de efectos fijos utilizando la función plm
modelo_efectos_fijos <- plm(labor_gdp ~ work_h, data = pdata, model = "within")

# Ver los resultados del modelo
summary(modelo_efectos_fijos)

stargazer(modelo_efectos_fijos, type = "text")
