# 02_limpeza_transformacao.R

# Bibliotecas
library(dplyr)
library(stringr)
library(stringi)

# ========================================================================
# 1) Seoul Bike Sharing
# Fonte: https://archive.ics.uci.edu/dataset/560/seoul+bike+sharing+demand
# ========================================================================

# ------------------------------
# TAREFA: Padronizar nomes de colunas para todos os conjuntos de dados
# ------------------------------
df_seoul <- read.csv("data/raw_seoul_bike_sharing.csv",
                     sep = ";",
                     fileEncoding = "latin1",
                     stringsAsFactors = FALSE)

names(df_seoul) <- names(df_seoul) %>%
  tolower() %>%
  gsub("[^a-z0-9]", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("_$", "", .)

# ------------------------------
# TAREFA: Detetar e manipular valores ausentes
# ------------------------------
df_seoul[df_seoul == ""] <- NA

# ------------------------------
# TAREFA: Criar variáveis indicadoras (fictícias) para variáveis categóricas
# ------------------------------
df_seoul <- df_seoul %>%
  mutate(
    is_holiday    = ifelse(holiday == "Holiday", 1, 0),
    season_winter = ifelse(seasons == "Winter", 1, 0),
    season_spring = ifelse(seasons == "Spring", 1, 0),
    season_summer = ifelse(seasons == "Summer", 1, 0),
    season_autumn = ifelse(seasons == "Autumn", 1, 0)
  )

# ------------------------------
# TAREFA: Normalizar dados
# ------------------------------
df_seoul <- df_seoul %>%
  mutate(
    temperature_norm      = scale(temperature_c),
    humidity_norm         = scale(humidity),
    wind_speed_norm       = scale(wind_speed_m_s),
    solar_radiation_norm  = scale(solar_radiation_mj_m2)
  )

# ========================================================================
# GUARDAR ficheiro limpo
# ========================================================================
write.csv(df_seoul, "data/clean_seoul_bike_sharing.csv", row.names = FALSE)


# Nota: As tarefas "Remover links de referência indesejados" e
# "Extrair valores numéricos com expressões regulares" foram avaliadas
# mas não aplicadas neste conjunto de dados, uma vez que:
# - Não existem campos com link;
# - Todas as colunas com números estão já corretamente em formato numérico,
#   não havendo necessidade de extração com expressões regulares.




# ========================================================================
# 2) Sistemas de Partilha de Bicicletas (Wikipedia)
# Fonte: https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems
# ========================================================================

# ------------------------------
# TAREFA: Padronizar nomes de colunas para todos os conjuntos de dados
# ------------------------------
df_systems <- read.csv("data/raw_bike_sharing_systems.csv",
                       sep = ";",
                       stringsAsFactors = FALSE,
                       fileEncoding = "latin1")

names(df_systems) <- names(df_systems) %>%
  tolower() %>%
  gsub(" ", "_", .) %>%
  gsub("\\(|\\)", "", .) %>%
  gsub("[^a-z0-9_]", "", .)

# Remover coluna duplicada
df_systems <- df_systems %>% select(-country1)

# Renomear colunas para melhor clareza
df_systems <- df_systems %>%
  rename(
    city_region = cityregion,
    system_company = name
  )

# Limpar city_region (remover "+ região" e números de distrito)
df_systems <- df_systems %>%
  mutate(city_region = str_replace(city_region, "\\s*\\+.*", ""),
         city_region = str_replace(city_region, "\\s*\\d+$", ""),
         city_region = str_trim(city_region))

# ------------------------------
# TAREFA: Remover links de referência indesejados usando expressões regulares
# ------------------------------
df_systems <- df_systems %>%
  mutate(across(where(is.character),
                ~ str_replace_all(., "http[^ ]+|\\[.*?\\]|\\(.*?\\)", ""))) %>%
  mutate(across(where(is.character),
                ~ str_trim(.)))

# ------------------------------
# TAREFA: Extrair valores numéricos usando expressões regulares
# ------------------------------
df_systems <- df_systems %>%
  mutate(
    launch_year        = str_extract(launched, "\\d{4}"),
    discontinued_year  = str_extract(discontinued, "\\d{4}"),
    system_generation  = str_extract(system, "\\d+")
  )

# Atualizar generation se ainda for NA
df_systems <- df_systems %>%
  mutate(system_generation = ifelse(is.na(system_generation),
                                    str_extract(system, "\\d+"),
                                    system_generation))

# Remover indicação de geração do nome do sistema
df_systems <- df_systems %>%
  mutate(system = str_replace(system, "(\\d+\\s*(&\\s*\\d+)?\\s*Gen\\.?\\s*)", ""))

# Remover valores como "1.0", "2.0E", etc.
df_systems <- df_systems %>%
  mutate(system = str_replace(system, "^\\d+(\\.\\d+)?[A-Za-z]?$", ""))

# ------------------------------
# TAREFA: Detetar e manipular valores ausentes
# ------------------------------
df_systems[df_systems == ""] <- NA

# ------------------------------
# TAREFA: Criar variáveis indicadoras (fictícias) para variáveis categóricas
# ------------------------------
df_systems <- df_systems %>%
  mutate(is_discontinued = ifelse(discontinued == "Discontinued", 1, 0))

# ------------------------------
# TAREFA: Normalizar dados
# ------------------------------
df_systems <- df_systems %>%
  mutate(
    launch_year_norm       = scale(as.numeric(launch_year)),
    discontinued_year_norm = scale(as.numeric(discontinued_year))
  )

# ------------------------------
# Limpeza adicional
# ------------------------------

# Função para detetar ruído de codificação baseado em padrões suspeitos
tem_ruido_codificacao <- function(linha) {
  any(grepl("Ã|�|Å|‚|Â|Ÿ|‰|œ|™", linha))
}

# Aplicar a função a todas as linhas (qualquer coluna com ruído)
df_systems <- df_systems[!apply(df_systems, 1, function(l) any(tem_ruido_codificacao(l))), ]

# Remover colunas irrelevantes
df_systems <- df_systems %>%
  select(-launched, -discontinued,
         -launch_year_norm, -discontinued_year_norm)

# ------------------------------
# Guardar ficheiro limpo
# ------------------------------
write.csv(df_systems, "data/clean_bike_sharing_systems.csv", row.names = FALSE)







# ========================================================================
# 3) Previsão Meteorológica das Cidades
# Fonte: raw_cities_weather_forecast.csv
# ========================================================================

# ------------------------------
# TAREFA: Padronizar nomes de colunas para todos os conjuntos de dados
# ------------------------------
df_weather <- read.csv("data/raw_cities_weather_forecast.csv",
                       sep = ";",
                       stringsAsFactors = FALSE,
                       fileEncoding = "latin1")

names(df_weather) <- names(df_weather) %>%
  tolower() %>%
  gsub(" ", "_", .) %>%
  gsub("\\(|\\)", "", .) %>%
  gsub("[^a-z0-9_]", "", .)

# ------------------------------
# TAREFA:  Remover links de referência indesejados usando expressões regulares
# ------------------------------
df_weather <- df_weather %>%
  mutate(across(where(is.character),
                ~ str_replace_all(., "http[^ ]+|\\[.*?\\]|\\(.*?\\)", ""))) %>%
  mutate(across(where(is.character),
                ~ str_trim(.)))

# ------------------------------
# TAREFA: Extrair valores numéricos usando expressões regulares
# ------------------------------
df_weather <- df_weather %>%
  mutate(hour = str_extract(dt_txt, "\\d{2}(?=:\\d{2}$)"))

# ------------------------------
# TAREFA: Detetar e manipular valores ausentes
# ------------------------------
df_weather[df_weather == ""] <- NA

# ------------------------------
# TAREFA: Criar variáveis indicadoras (fictícias) para variáveis categóricas
# ------------------------------
df_weather <- df_weather %>%
  mutate(is_daytime = ifelse(syspod == "d", 1, 0))

# ------------------------------
# TAREFA: Normalizar dados
# ------------------------------
df_weather <- df_weather %>%
  mutate(
    temp_norm = scale(maintemp),
    humidity_norm = scale(mainhumidity),
    wind_speed_norm = scale(windspeed)
  )

# ------------------------------
# OUTRAS ALTERAÇÕES
# ------------------------------

# Separar data e hora do dt_txt, remover dt e dt_txt
df_weather <- df_weather %>%
  mutate(
    date = as.Date(dt_txt, format = "%d/%m/%Y"),
    hour = format(as.POSIXct(dt_txt, format = "%d/%m/%Y %H:%M"), "%H:%M")
  ) %>%
  select(-dt, -dt_txt)

# Renomear colunas para maior clareza (exceto syspod, que vai ser removido)
df_weather <- df_weather %>%
  rename(
    city = cidade,
    precipitation_prob = pop,
    rain_last_3h = rain3h
  )

# Criar is_daytime com base na hora, e remover coluna syspod (caso ainda esteja presente)
df_weather <- df_weather %>%
  mutate(is_daytime = ifelse(as.numeric(substr(hour, 1, 2)) %in% 6:18, 1, 0)) %>%
  select(-syspod)

# ------------------------------
# Guardar ficheiro limpo
# ------------------------------
write.csv(df_weather, "data/clean_cities_weather_forecast.csv", row.names = FALSE)






# ========================================================================
# 4) World Cities
# Fonte: https://simplemaps.com/data/world-cities
# ========================================================================

# ------------------------------
# TAREFA: Padronizar nomes de colunas para todos os conjuntos de dados
# ------------------------------
df_worldcities <- read.csv("data/raw_worldcities.csv",
                           sep = ";",
                           stringsAsFactors = FALSE,
                           fileEncoding = "latin1")

names(df_worldcities) <- names(df_worldcities) %>%
  tolower() %>%
  gsub(" ", "_", .) %>%
  gsub("\\(|\\)", "", .) %>%
  gsub("[^a-z0-9_]", "", .)

# ------------------------------
# TAREFA: Remover links de referência indesejados usando expressões regulares
# ------------------------------
df_worldcities <- df_worldcities %>%
  mutate(across(where(is.character),
                ~ str_replace_all(., "http[^ ]+|\\[.*?\\]|\\(.*?\\)", ""))) %>%
  mutate(across(where(is.character),
                ~ str_trim(.)))

# ------------------------------
# TAREFA: Extrair valores numéricos usando expressões regulares
# ------------------------------
# (Nenhuma coluna requer extração)

# ------------------------------
# TAREFA: Detetar e manipular valores ausentes
# ------------------------------
df_worldcities[df_worldcities == ""] <- NA

# ------------------------------
# TAREFA: Criar variáveis indicadoras (fictícias) para variáveis categóricas
# ------------------------------
# (Sem necessidade da criação de variáveis indicadoras (fictícias))

# ------------------------------
# TAREFA: Normalizar dados
# ------------------------------
df_world <- df_world %>%
  select(city, country, lat, lng, population) %>%
  mutate(population_norm = scale(population))

# ========================================================================
# GUARDAR ficheiro limpo
# ========================================================================
write.csv(df_world, "data/clean_worldcities.csv", row.names = FALSE)