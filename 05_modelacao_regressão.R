# 05_modelacao_regressao.R

library(dplyr)
library(ggplot2)
library(caret)
library(glmnet)

# ==========================
# Dividir dados em treino e teste
# ==========================
df <- read.csv("data/clean_seoul_bike_sharing.csv", stringsAsFactors = FALSE)

df <- df %>%
  select(-date, -holiday, -seasons, -functioning_day)  # dummies já incluídas

set.seed(123)
split_index <- createDataPartition(df$rented_bike_count, p = 0.7, list = FALSE)
train_data <- df[split_index, ]
test_data <- df[-split_index, ]

# ==========================
# Modelo de regressão com variáveis meteorológicas
# ==========================
modelo_meteo <- lm(rented_bike_count ~ temperature_c + humidity + wind_speed_m_s +
                     solar_radiation_mj_m2 + rainfall_mm + snowfall_cm,
                   data = train_data)
summary(modelo_meteo)

# ==========================
# Modelo com variáveis temporais
# ==========================
modelo_tempo <- lm(rented_bike_count ~ hour + is_holiday +
                     season_winter + season_spring + season_summer + season_autumn,
                   data = train_data)
summary(modelo_tempo)

# ==========================
# Avaliar modelos e identificar variáveis importantes
# ==========================
previsoes_meteo <- predict(modelo_meteo, newdata = test_data)
previsoes_tempo <- predict(modelo_tempo, newdata = test_data)

rmse_meteo <- RMSE(previsoes_meteo, test_data$rented_bike_count)
rmse_tempo <- RMSE(previsoes_tempo, test_data$rented_bike_count)

print(paste("RMSE Modelo Meteorológico:", round(rmse_meteo, 2)))
print(paste("RMSE Modelo Temporal:", round(rmse_tempo, 2)))

# ==========================
# Refinar modelos: Termos de ordem superior
# ==========================
modelo_poly <- lm(rented_bike_count ~ poly(temperature_c, 2) + humidity + wind_speed_m_s +
                    solar_radiation_mj_m2 + rainfall_mm + snowfall_cm +
                    hour + season_winter + season_spring + season_summer + season_autumn +
                    is_holiday,
                  data = train_data)
summary(modelo_poly)

# ==========================
# Refinar modelos: Interações
# ==========================
modelo_interacao <- lm(rented_bike_count ~ temperature_c * hour +
                         humidity + wind_speed_m_s + solar_radiation_mj_m2 +
                         rainfall_mm + snowfall_cm +
                         season_winter + season_spring + season_summer + season_autumn +
                         is_holiday,
                       data = train_data)
summary(modelo_interacao)

# ==========================
# Regularização
# ==========================
x_train <- model.matrix(rented_bike_count ~ . -1, data = train_data)
y_train <- train_data$rented_bike_count
x_test <- model.matrix(rented_bike_count ~ . -1, data = test_data)

lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)
best_lambda <- lasso_model$lambda.min
lasso_final <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

pred_lasso <- predict(lasso_final, s = best_lambda, newx = x_test)
rmse_lasso <- RMSE(pred_lasso, test_data$rented_bike_count)
print(paste("RMSE Modelo LASSO:", round(rmse_lasso, 2)))

# ==========================
# Experimentos até obter melhor desempenho
# ==========================
pred_poly <- predict(modelo_poly, newdata = test_data)
rmse_poly <- RMSE(pred_poly, test_data$rented_bike_count)

pred_interacao <- predict(modelo_interacao, newdata = test_data)
rmse_interacao <- RMSE(pred_interacao, test_data$rented_bike_count)

print(paste("RMSE - Modelo Meteorológico:", round(rmse_meteo, 2)))
print(paste("RMSE - Modelo Temporal:", round(rmse_tempo, 2)))
print(paste("RMSE - Modelo com termos de ordem superior:", round(rmse_poly, 2)))
print(paste("RMSE - Modelo com interações:", round(rmse_interacao, 2)))
print(paste("RMSE - Modelo LASSO:", round(rmse_lasso, 2)))

# ==========================
# Guardar o modelo com interações para usar no Shiny
# ==========================
save(modelo_interacao, file = "modelo_interacao.RData")