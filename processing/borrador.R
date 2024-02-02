

db <- proc_db[[12]][c(1:3, 7, 10)] %>% 
  filter(hab_t %in% c("40", "41 a 44") & ano_trimestre >= 2022) %>% 
  pivot_wider(id_cols = c(ano_trimestre, mes_central, anotrim),
              names_from = hab_t,
              values_from = prop) %>% 
  mutate(anotrim = factor(anotrim), levels = unique(anotrim))

t_serie2 <- ts(db[, c("40", "41 a 44")], start = c(2022,1), end = c(2023, 10), frequency = 12)

n <- nrow(t_serie2[1:12, 1:2])
time_points <- 1:n

m2 <- lm(formula = t_serie2[1:12, 1:2] ~ time_points)


df_tiempo <- data.frame(fecha = time(t_serie2), 
                        valor_40 = as.numeric(t_serie2[, "40"]), 
                        valor_41_44 = as.numeric(t_serie2[, "41 a 44"]))



df1 <- df_tiempo[1:12, 1:3] %>% 
  rename(valor_40_1 = valor_40, valor_41_44_1 = valor_41_44)

df2 <- df_tiempo[13:nrow(df_tiempo), 1:3] %>% 
  rename(valor_40_2 = valor_40, valor_41_44_2 = valor_41_44)

df_tiempo <- full_join(df1, df2, by = "fecha")


ggplot(df_tiempo, aes(x = fecha)) +
  geom_vline(xintercept = 2022.95, linetype = "solid", color = "red", size=1) +
  geom_line(aes(y = valor_40_1, color = "Grupo 40"), size = 1) +
  geom_smooth(aes(x = fecha, y = valor_40_1), method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  geom_line(aes(y = valor_40_2, color = "Grupo 40"), size = 1) +
  geom_smooth(aes(x = fecha, y = valor_40_2), method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  geom_line(aes(y = valor_41_44_1, color = "Grupo 41 a 44"), size = 1) +
  geom_smooth(aes(x = fecha, y = valor_41_44_1), method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_line(aes(y = valor_41_44_2, color = "Grupo 41 a 44"), size = 1) +
  geom_smooth(aes(x = fecha, y = valor_41_44_2), method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(labels = function(prop){paste0(prop, "%")}, limits = c(4,12), n.breaks = 10) +
  scale_x_continuous() 


# Step 1: Generate Fictitious Time Series Data
set.seed(123)
n <- 100  # Number of observations
time_points <- seq(as.Date("2022-01-01"), by = "1 day", length.out = n)
data_values <- 20 + cumsum(rnorm(n))  # Simulating a trend

# Introduce seasonality
seasonal_pattern <- 5 * sin(2 * pi * (1:n) / 30)
data_values <- data_values + seasonal_pattern

# Create time series object
time_series <- ts(data_values, start = time_points[1], frequency = 365)

# Step 2: Data Cleaning (Not shown for simplicity)

# Step 3: Time Series Visualization
plot(time_series, main = "Fictitious Time Series Data", xlab = "Time", ylab = "Values")

# Step 4: Trend Analysis
library(ggplot2)
library(forecast)

# Plotting time series with a trend line
ggplot(data.frame(time = time(time_series), values = as.vector(time_series)), aes(x = time, y = values)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Trend Analysis", x = "Time", y = "Values")

# Step 5: Seasonality Detection
# Autocorrelation plot to identify seasonality
acf(time_series, main = "Autocorrelation Plot for Seasonality Detection")

# Seasonal decomposition using stl
seasonal_decomposition <- stl(time_series, s.window = "periodic")

# Plotting decomposed components
plot(seasonal_decomposition)


library(tseries)


tseries::adf.test(time_series)

# Step 1: Data Collection
set.seed(123)  # For reproducibility
n <- 100
time_points <- 1:n
data <- sin(0.1 * time_points) + rnorm(n)

# Step 2: Data Cleaning (No missing data in this example)

# Step 3: Descriptive Statistics
summary(data)

# Step 4: Time Series Plot (Trend Analysis)
plot(time_points, data, type = "l", col = "blue", xlab = "Time", ylab = "Value", main = "Time Series Plot")

# Step 5: Seasonal Patterns (Seasonality Detection)
library(ggplot2)
library(seasonal)
seasonal_decomp <- stl(data, s.window = "periodic")
seasonal_plot <- autoplot(seasonal_decomp, main = "Seasonal Decomposition")
seasonal_plot

# Step 4: Time Series Plot (Trend Analysis)
plot(time_points, data, type = "l", col = "blue", xlab = "Time", ylab = "Value", main = "Time Series Plot")

# Statistical Test for Trend
trend_test <- lm(data ~ time_points)
summary(trend_test)

# Step 5: Seasonal Patterns (Seasonality Detection)
library(tseries)

# Statistical Test for Seasonality (Augmented Dickey-Fuller Test)
adf_test <- adf.test(data)
cat("ADF Statistic:", adf_test$statistic, "\n", "P-value:", adf_test$p.value, "\n")

# Seasonal Decomposition
seasonal_decomp <- stl(data, s.window = "periodic")

# Plotting the Decomposed Components
plot(seasonal_decomp, main = "Seasonal Decomposition")


db <- proc_db[[1]] %>% filter(ano)

time_series <- ts(proc_db[[1]]$media, start = c(2017, 8), end = c(2023, 10), frequency = 12)

n <- 75
time_points <- 1:n

time_series %>% plot()

lm(time_series ~ time_points)
adf.test(time_series)


ggplot(data.frame(time = time(time_series), values = as.vector(time_series)), aes(x = time, y = values)) +
  geom_line(linetype = "solid", color = "darkorange", linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Trend Analysis", x = "Time", y = "Values")



acf(time_series, main = "Autocorrelation Plot for Seasonality Detection")



paste("y =", round(coef(m1)[2], 2), "x +", round(coef(m1)[1], 2))


g5 <- bind_rows(
  ene_pond[[1]] %>% 
    filter(hab_t %in% c("40", "41 a 44", "45", "46 o más")) %>% 
    group_by(hab_t, sexo) %>% 
    summarise(t = survey_total(vartype = NULL),
              p = survey_prop(proportion = T, vartype = "ci")*100) %>% 
    mutate(anotrim = "2022 son"),
  ene_pond[[2]] %>% 
    filter(hab_t %in% c("40", "41 a 44", "45", "46 o más")) %>% 
    group_by(hab_t, sexo) %>% 
    summarise(t = survey_total(vartype = NULL),
              p = survey_prop(proportion = T, vartype = "ci")*100)  %>% 
    mutate(anotrim = "2023 son")
) %>% 
  select(anotrim, hab_t, sexo, starts_with("p")) %>% 
  ungroup() %>% 
  mutate(sexo = if_else(sexo == 1, "Hombres", "Mujeres")) %>%
  ggplot(aes(x = hab_t, y = p, fill = anotrim, label = scales::percent(p, accuracy = 0.1, scale = 1),
             group = interaction(hab_t, anotrim))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = p_low, ymax = p_upp), 
                position = position_dodge(width = 0.9), width = 0.3, size = 0.5) +
  geom_label(position = position_dodge(width = 0.9), vjust = 2, size = 3,
             fill = "white", color = "black", fontface = "bold") +
  facet_wrap(~sexo, ncol = 1) +
  scale_y_continuous(labels = function(prop){paste0(prop, "%")},
                     breaks = c(10,30,50,70)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  labs(y = "Proporción trabajadores",
       x = "Tramo horas habituales",
       fill = "Año",
       caption = "Fuente: Elaboración propia en base a ENE.") +
  theme(axis.text.x = element_text(angle = 0,
                                   vjust = 0.5,
                                   size = rel(1)))



proc_db[[14]] %>% 
  select(anotrim, hab_t, edad_t, prop) %>% 
  filter(hab_t %in% c("40", "41 a 44")) %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  ggplot(aes(x = anotrim, y = prop, color = hab_t, group = hab_t)) +
  geom_line(linewidth = 0.7) +
  geom_vline(aes(xintercept = "2023 mam"), linetype = "dashed") +
  facet_wrap(~edad_t) +
  scale_y_continuous(labels = function(prop){paste0(prop, "%")}, 
                     breaks = seq(0, 12, by = 2)) +
  scale_x_discrete(breaks = c(trims, "2023 mam")) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(y = "% trabajadores",
       x = NULL,
       color = "Tramos horas habituales",
       caption = "Fuente: Elaboración propia en base a ENE.")



g9 <- proc_db[[14]] %>% 
  select(anotrim, hab_t, edad_t, prop) %>% 
  filter(hab_t %in% c("45", "46 o más")) %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  ggplot(aes(x = anotrim, y = prop, color = hab_t, group = hab_t)) +
  geom_line(linewidth = 0.7) +
  geom_vline(aes(xintercept = "2023 mam"), linetype = "dashed") +
  facet_wrap(~edad_t) +
  scale_y_continuous(labels = function(prop){paste0(prop, "%")}, 
                     n.breaks = 10) +
  scale_x_discrete(breaks = c(trims, "2023 mam")) +
  scale_color_manual(values = c("#4DAF4A", "#984EA3")) +
  labs(y = "% trabajadores",
       x = NULL,
       color = "Tramos horas habituales",
       caption = "Fuente: Elaboración propia en base a ENE.")
