# 04_visualizacoes.R

library(dplyr)
library(ggplot2)

# ==========================
# Tarefa 1 – Carregar o conjunto de dados (sem normalização)
# ==========================
df_seoul <- read.csv("data/raw_seoul_bike_sharing.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "latin1")

# ==========================
# Tarefa 2 – Reformular DATE como data
# ==========================
df_seoul$Date <- as.Date(df_seoul$Date, format = "%d/%m/%Y")

# ==========================
# Tarefa 3 – Transmitir HORAS como variável categórica ordenada
# ==========================
df_seoul$Hour <- factor(df_seoul$Hour, levels = 0:23, ordered = TRUE)

# ==========================
# Tarefa 4 – Resumo do conjunto de dados
# ==========================
summary(df_seoul)

# ==========================
# Tarefa 5 – Calcular o número total de feriados
# ==========================
n_feriados <- sum(df_seoul$Holiday == "Holiday")
print(paste("Número de feriados:", n_feriados))

# ==========================
# Tarefa 6 – Calcular a percentagem de registos em feriado
# ==========================
percent_feriados <- mean(df_seoul$Holiday == "Holiday") * 100
print(paste("Percentagem de registos em feriado:", round(percent_feriados, 2), "%"))

# ==========================
# Tarefa 7 – Determinar quantos registos esperar num ano completo
# ==========================
registos_esperados <- 365 * 24
print(paste("Registos esperados para um ano completo:", registos_esperados))

# ==========================
# Tarefa 8 – Quantos registos existem com Functioning Day == "Yes"
# ==========================
registos_funcionais <- sum(df_seoul$Functioning.Day == "Yes")
print(paste("Registos em dias funcionais:", registos_funcionais))

# ==========================
# Tarefa 9 – Precipitação e neve totais por estação
# ==========================
df_seoul %>%
  group_by(Seasons) %>%
  summarise(
    precipitacao_total = sum(Rainfall.mm.),
    neve_total = sum(Snowfall..cm.)
  ) %>%
  print()

# ==========================
# Tarefa 10 – Dispersão: RENTED_BIKE_COUNT vs DATE
# ==========================
ggplot(df_seoul, aes(x = Date, y = Rented.Bike.Count)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  labs(
    title = "Dispersão do número de alugueres por data",
    x = "Data",
    y = "Alugueres"
  ) +
  theme_minimal()

# ==========================
# Tarefa 11 – Dispersão: RENTED_BIKE_COUNT vs DATE, com cor por HORA
# ==========================
ggplot(df_seoul, aes(x = Date, y = Rented.Bike.Count, color = Hour)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Alugueres por data com variação por hora",
    x = "Data",
    y = "Alugueres",
    color = "Hora"
  ) +
  theme_minimal()

# ==========================
# Tarefa 12 – Histograma normalizado com curva de densidade
# ==========================
ggplot(df_seoul, aes(x = Rented.Bike.Count)) +
  geom_histogram(aes(y = after_stat(density)), fill = "lightblue", color = "black", bins = 30) +
  geom_density(color = "darkblue", linewidth = 1.2) +
  labs(
    title = "Distribuição do número de alugueres",
    x = "Alugueres",
    y = "Densidade"
  ) +
  theme_minimal()

# ==========================
# Tarefa 13 – Dispersão: RENTED_BIKE_COUNT vs TEMPERATURA por ESTAÇÃO, com cor por HORA
# ==========================
ggplot(df_seoul, aes(x = Temperature..C., y = Rented.Bike.Count, color = Hour)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~Seasons) +
  labs(
    title = "Alugueres vs Temperatura por Estação",
    x = "Temperatura (°C)",
    y = "Alugueres",
    color = "Hora"
  ) +
  theme_minimal()

# ==========================
# Tarefa 14 – Boxplots de RENTED_BIKE_COUNT vs HORA, por ESTAÇÃO
# ==========================
ggplot(df_seoul, aes(x = Hour, y = Rented.Bike.Count, fill = Seasons)) +
  geom_boxplot() +
  facet_wrap(~Seasons) +
  labs(
    title = "Distribuição de Alugueres por Hora e Estação",
    x = "Hora",
    y = "Alugueres"
  ) +
  theme_minimal()

# ==========================
# Tarefa 15 – Precipitação e neve totais por DATA
# ==========================
df_seoul %>%
  group_by(Date) %>%
  summarise(
    precipitacao_total = sum(Rainfall.mm.),
    neve_total = sum(Snowfall..cm.)
  ) %>%
  print()

# ==========================
# Tarefa 16 – Número de dias com queda de neve (> 0 cm)
# ==========================
dias_com_neve <- df_seoul %>%
  group_by(Date) %>%
  summarise(neve_total = sum(Snowfall..cm.)) %>%
  filter(neve_total > 0) %>%
  nrow()

print(paste("Número de dias com queda de neve:", dias_com_neve))