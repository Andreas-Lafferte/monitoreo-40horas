library(tseries)
library(forecast)

serie_temporal <- ts(db$prop_41a44, start = c(2020, 2), frequency = 12)

indice_cambio <- which(db$cambio == 1)[1]  # Índice del primer cambio

serie_temporal_antes <- serie_temporal[1:(indice_cambio - 1)]

# Ajustar un modelo SARIMA para el período antes del cambio
modelo_sarima_antes <- auto.arima(serie_temporal_antes)

predicciones_despues <- forecast(modelo_sarima_antes, h = length(serie_temporal) - indice_cambio)

datos_despues <- serie_temporal[indice_cambio:length(serie_temporal)]

autoplot(predicciones_despues, series = "Predicciones") 

###
# Extraer las predicciones y sus tiempos
pred_tiempo <- as.numeric(time(predicciones_despues))
pred_valores <- as.numeric(predicciones_despues$mean)

# Ajustar la longitud de los vectores para que coincidan
longitud_minima <- min(length(pred_tiempo), length(pred_valores))
pred_tiempo <- pred_tiempo[1:longitud_minima]
pred_valores <- pred_valores[1:longitud_minima]

# Crear el data frame de predicciones
pred_df <- data.frame(tiempo = pred_tiempo, predicciones = pred_valores)

# Ajustar el número de filas para que coincida con los datos reales después del cambio
datos_despues_df <- data.frame(tiempo = time(datos_despues), prop_41a44 = as.numeric(datos_despues))
pred_df <- pred_df[1:length(datos_despues), ]

ggplot() +
  geom_line(data = pred_df, aes(x = tiempo, y = predicciones), color = "red") +
  geom_line(data = datos_despues_df, aes(x = tiempo, y = prop_41a44), color = "blue") +
  xlab("Tiempo")

datos_despues_df$tiempo <- 35:44

autoplot(predicciones_despues, series = "Predicciones") +
  geom_line(data = df, aes(x = time, y = prop_41a44), color = "red")

df <- db %>% 
  select(time, prop_41a44)



