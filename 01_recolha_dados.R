# 01_recolha_dados.R

# Bibliotecas
install.packages("rvest")
library(rvest)
library(httr)
library(jsonlite)

# Web Scrape a Global Bike-Sharing System Wiki Page

# URL da página da Wikipédia
url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"
# Ler o HTML da página
pagina <- read_html(url)

# Extrair a tabela
tabela <- html_table(html_nodes(pagina, "table")[[1]])

# Guardar como CSV na pasta data/
write.csv(tabela, "data/raw_bike_sharing_systems.csv", row.names = FALSE)

#########################################################


# Chamada de API OpenWeather
api_key <- "1d07867305411bde049a0d4745fb25c5"
cidades <- c("Seoul", "New York", "Paris", "Suzhou", "London")

obter_previsao <- function(cidade) {
  cidade_codificada <- URLencode(cidade)
  url <- paste0(
    "https://api.openweathermap.org/data/2.5/forecast?q=",
    cidade_codificada,
    "&appid=", api_key,
    "&units=metric"
  )
  
  resposta <- GET(url)
  dados <- fromJSON(content(resposta, as = "text"), flatten = TRUE)
  
  if (!is.null(dados$list)) {
    previsao <- dados$list
    previsao$cidade <- cidade
    return(previsao)
  } else {
    message("Erro ao obter dados para: ", cidade)
    return(NULL)
  }
}

lista_previsoes <- lapply(cidades, obter_previsao)
todas_previsoes <- do.call(rbind, lista_previsoes)

# Remover colunas do tipo lista para garantir escrita no CSV
todas_previsoes_simplificado <- todas_previsoes[, sapply(todas_previsoes, Negate(is.list))]

write.csv(todas_previsoes_simplificado, "data/raw_cities_weather_forecast.csv", row.names = FALSE)

#########################################################