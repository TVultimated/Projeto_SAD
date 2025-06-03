# 03_analise_exploratoria.R

library(DBI)
library(RSQLite)
library(dplyr)

# Criar ligação à base de dados SQLite
con <- dbConnect(SQLite(), "data/projeto_sad.db")

# Importar os datasets limpos
dbWriteTable(con, "seoul_bike", read.csv("data/clean_seoul_bike_sharing.csv"), overwrite = TRUE)
dbWriteTable(con, "systems",    read.csv("data/clean_bike_sharing_systems.csv"), overwrite = TRUE)
dbWriteTable(con, "weather",    read.csv("data/clean_cities_weather_forecast.csv"), overwrite = TRUE)
dbWriteTable(con, "world",      read.csv("data/clean_worldcities.csv"), overwrite = TRUE)

# ==========================
# Tarefa 1 – Contagem de Registos
# ==========================
t1 <- dbGetQuery(con, "
  SELECT COUNT(*) AS total_registos
  FROM seoul_bike
")
print(t1)

# ==========================
# Tarefa 2 – Horário de Funcionamento
# ==========================
t2 <- dbGetQuery(con, "
  SELECT COUNT(*) AS horas_com_aluguer
  FROM seoul_bike
  WHERE rented_bike_count <> 0
")
print(t2)

# ==========================
# Tarefa 3 – Perspetivas meteorológicas
# ==========================
t3 <- dbGetQuery(con, "
  SELECT *
  FROM weather
  WHERE city = 'Seoul'
  ORDER BY date
  LIMIT 1
")
print(t3)

# ==========================
# Tarefa 4 – Estações
# ==========================
t4 <- dbGetQuery(con, "
  SELECT DISTINCT seasons
  FROM seoul_bike
")
print(t4)

# ==========================
# Tarefa 5 – Intervalo de datas
# ==========================
t5 <- dbGetQuery(con, "
  SELECT 
    MIN(date) AS primeira_data,
    MAX(date) AS ultima_data
  FROM seoul_bike
")
print(t5)

# ==========================
# Tarefa 6 – Subconsulta - 'máximo histórico'
# ==========================
t6 <- dbGetQuery(con, "
  SELECT date, hour, rented_bike_count
  FROM seoul_bike
  WHERE rented_bike_count = (
    SELECT MAX(rented_bike_count)
    FROM seoul_bike
  )
")
print(t6)

# ==========================
# Tarefa 7 – Popularidade horária e temperatura por estação
# ==========================
t7 <- dbGetQuery(con, "
  SELECT seasons, hour,
         AVG(temperature_c)     AS temperatura_media,
         AVG(rented_bike_count) AS alugueres_medios
  FROM seoul_bike
  GROUP BY seasons, hour
  ORDER BY alugueres_medios DESC
  LIMIT 10
")
print(t7)

# ==========================
# Tarefa 8 – Sazonalidade do aluguer
# ==========================
df_seoul <- dbReadTable(con, "seoul_bike")
t8 <- df_seoul %>%
  group_by(seasons) %>%
  summarise(
    media = mean(rented_bike_count),
    minimo = min(rented_bike_count),
    maximo = max(rented_bike_count),
    desvio_padrao = sd(rented_bike_count)
  )
print(t8)

# ==========================
# Tarefa 9 – Sazonalidade Meteorológica
# ==========================
t9 <- dbGetQuery(con, "
  SELECT seasons,
         AVG(temperature_c)            AS temperatura_media,
         AVG(humidity)                 AS humidade_media,
         AVG(wind_speed_m_s)           AS vento_medio,
         AVG(visibility_10m)           AS visibilidade_media,
         AVG(dew_point_temperature_c)  AS ponto_orvalho_medio,
         AVG(solar_radiation_mj_m2)    AS radiacao_media,
         AVG(rainfall_mm)              AS chuva_media,
         AVG(snowfall_cm)              AS neve_media,
         AVG(rented_bike_count)        AS alugueres_medios
  FROM seoul_bike
  GROUP BY seasons
  ORDER BY alugueres_medios DESC
")
print(t9)

# ==========================
# Tarefa 10 – Contagem total de bicicletas e informações sobre a cidade de Seul
# ==========================
# Nº total de bicicletas em Seul (3.000) retirado do texto da Wikipédia, não disponível nas tabelas.
# Valor incluído manualmente após junção implícita entre WORLD_CITIES e BIKE_SHARING_SYSTEMS.
t10 <- dbGetQuery(con, "
  SELECT 
    w.city,
    w.country,
    w.lat,
    w.lng,
    w.population,
    3000 AS total_bicicletas
  FROM world w, systems s
  WHERE w.city = s.city_region
    AND w.city = 'Seoul'
")
print(t10)
