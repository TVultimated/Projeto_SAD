# app.R

library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(lubridate)

# Modelo treinado carregado de ficheiro .RData 
load("modelo_interacao.RData")

# API KEY da OpenWeather
api_key <- "1d07867305411bde049a0d4745fb25c5"

# Cidades e coordenadas
cidades <- data.frame(
  nome = c("New York", "Paris", "Suzhou", "London"),
  lat = c(40.7128, 48.8566, 31.2983, 51.5074),
  lon = c(-74.0060, 2.3522, 120.5832, -0.1278),
  pais = c("US", "FR", "CN", "GB"),
  stringsAsFactors = FALSE
)

# Função para obter previsão meteorológica dos próximos 5 dias
obter_previsao_5dias <- function(cidade_info) {
  url <- paste0("https://api.openweathermap.org/data/2.5/forecast?lat=", cidade_info$lat, 
                "&lon=", cidade_info$lon, "&appid=", api_key, "&units=metric")
  
  resp <- GET(url)
  dados <- fromJSON(content(resp, "text"), flatten = TRUE)
  
  if (!is.null(dados$list) && length(dados$list) > 0) {
    # A API retorna previsões em intervalos de 3 horas por 5 dias (40 previsões)
    previsoes <- data.frame(
      data_hora = as.POSIXct(dados$list$dt_txt, format = "%Y-%m-%d %H:%M:%S"),
      temperatura = dados$list$main.temp,
      humidade = dados$list$main.humidity,
      velocidade_vento = dados$list$wind.speed,
      stringsAsFactors = FALSE
    )
    
    # Tratamento seguro para chuva e neve
    if ("rain" %in% names(dados$list) && !is.null(dados$list$rain)) {
      if (is.data.frame(dados$list$rain) && "3h" %in% names(dados$list$rain)) {
        previsoes$chuva <- dados$list$rain$"3h"
      } else {
        previsoes$chuva <- 0
      }
    } else {
      previsoes$chuva <- 0
    }
    
    if ("snow" %in% names(dados$list) && !is.null(dados$list$snow)) {
      if (is.data.frame(dados$list$snow) && "3h" %in% names(dados$list$snow)) {
        previsoes$neve <- dados$list$snow$"3h"
      } else {
        previsoes$neve <- 0
      }
    } else {
      previsoes$neve <- 0
    }
    
    # Adicionar hora e estação do ano
    previsoes$hora <- hour(previsoes$data_hora)
    
    # Determinar a estação do ano para cada previsão
    mes <- month(previsoes$data_hora)
    previsoes$season_winter <- ifelse(mes %in% c(12, 1, 2), 1, 0)
    previsoes$season_spring <- ifelse(mes %in% c(3, 4, 5), 1, 0)
    previsoes$season_summer <- ifelse(mes %in% c(6, 7, 8), 1, 0)
    previsoes$season_autumn <- ifelse(mes %in% c(9, 10, 11), 1, 0)
    
    # Variáveis dummy para feriados (assumimos 0 para simplificar)
    previsoes$is_holiday <- 0
    
    # Adicionar radiação solar (uma aproximação simples baseada na hora do dia)
    previsoes$solar_radiation_mj_m2 <- ifelse(
      previsoes$hora >= 6 & previsoes$hora <= 18, 
      sin((previsoes$hora - 6) * pi / 12) * 3, 
      0
    )
    
    return(previsoes)
  } else {
    return(NULL)
  }
}

# Função para aplicar o modelo de previsão aos dados meteorológicos
prever_demanda <- function(previsoes) {
  # Renomear colunas para corresponder ao modelo
  dados_modelo <- previsoes %>%
    rename(
      temperature_c = temperatura,
      humidity = humidade,
      wind_speed_m_s = velocidade_vento,
      rainfall_mm = chuva,
      snowfall_cm = neve,
      hour = hora
    )
  
  # Fazer a previsão
  dados_modelo$demanda_prevista <- predict(modelo_interacao, newdata = dados_modelo)
  
  return(dados_modelo)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Previsão de Aluguer de Bicicletas"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa Interativo", tabName = "mapa", icon = icon("map")),
      menuItem("Comparação de Cidades", tabName = "comparacao", icon = icon("chart-bar")),
      menuItem("Dados Detalhados", tabName = "dados", icon = icon("table"))
    ),
    hr(),
    selectInput("cidade", "Cidade:", choices = cidades$nome),
    actionButton("atualizar", "Atualizar Previsões", icon = icon("sync"))
  ),
  
  dashboardBody(
    tabItems(
      # Aba do Mapa
      tabItem(tabName = "mapa",
              fluidRow(
                box(
                  width = 12,
                  title = "Mapa de Demanda Máxima de Bicicletas",
                  status = "primary",
                  solidHeader = TRUE,
                  leafletOutput("mapa_demanda", height = 600)
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Informações",
                  "Este mapa mostra a demanda máxima prevista de aluguer de bicicletas para os próximos 5 dias em cada cidade.",
                  "O tamanho dos círculos representa a demanda prevista."
                )
              )
      ),
      
      # Aba de Comparação
      tabItem(tabName = "comparacao",
              fluidRow(
                box(
                  width = 12,
                  title = "Comparação de Demanda entre Cidades",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("grafico_comparacao", height = 500)
                )
              )
      ),
      
      # Aba de Dados Detalhados
      tabItem(tabName = "dados",
              fluidRow(
                box(
                  width = 12,
                  title = "Previsão Detalhada por Cidade",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("grafico_detalhado", height = 500)
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Dados de Previsão",
                  status = "primary",
                  DT::dataTableOutput("tabela_dados")
                )
              )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Objeto reativo para armazenar todas as previsões
  todas_previsoes <- reactiveVal(list())
  
  # Função para atualizar todas as previsões
  atualizar_todas_previsoes <- function() {
    previsoes_lista <- list()
    
    # Para cada cidade, obter as previsões e calcular a demanda
    for (i in 1:nrow(cidades)) {
      cidade_info <- cidades[i, ]
      withProgress(
        message = paste0("Obtendo dados para ", cidade_info$nome, "..."),
        value = i/nrow(cidades), {
          previsao <- obter_previsao_5dias(cidade_info)
          
          if (!is.null(previsao)) {
            previsao_com_demanda <- prever_demanda(previsao)
            previsao_com_demanda$cidade <- cidade_info$nome
            previsoes_lista[[cidade_info$nome]] <- previsao_com_demanda
          }
        }
      )
    }
    
    todas_previsoes(previsoes_lista)
  }
  
  # Atualizar previsões quando o botão for clicado
  observeEvent(input$atualizar, {
    atualizar_todas_previsoes()
    showNotification("Previsões atualizadas com sucesso!", type = "message")
  })
  
  # Atualizar previsões quando a app for iniciada
  observe({
    # Verificar se as previsões já foram carregadas
    if (length(todas_previsoes()) == 0) {
      atualizar_todas_previsoes()
    }
  })
  
  # Renderizar mapa de demanda máxima
  output$mapa_demanda <- renderLeaflet({
    previsoes <- todas_previsoes()
    
    if (length(previsoes) == 0) {
      return(
        leaflet(cidades) %>%
          addTiles() %>%
          addMarkers(~lon, ~lat, label = ~nome)
      )
    }
    
    # Calcular demanda máxima para cada cidade
    demanda_maxima <- data.frame(
      cidade = character(),
      lat = numeric(),
      lon = numeric(),
      max_demanda = numeric(),
      hora_max = character(),
      stringsAsFactors = FALSE
    )
    
    for (nome_cidade in names(previsoes)) {
      cidade_dados <- previsoes[[nome_cidade]]
      max_idx <- which.max(cidade_dados$demanda_prevista)
      
      cidade_info <- cidades[cidades$nome == nome_cidade, ]
      
      demanda_maxima <- rbind(
        demanda_maxima,
        data.frame(
          cidade = nome_cidade,
          lat = cidade_info$lat,
          lon = cidade_info$lon,
          max_demanda = round(cidade_dados$demanda_prevista[max_idx], 0),
          hora_max = format(cidade_dados$data_hora[max_idx], "%d/%m/%Y %H:%M"),
          stringsAsFactors = FALSE
        )
      )
    }
    
    # Normalizar o tamanho dos círculos
    max_demanda <- max(demanda_maxima$max_demanda)
    min_demanda <- min(demanda_maxima$max_demanda)
    
    # Criar raios proporcionais à demanda
    demanda_maxima$raio <- 2000 + 8000 * (demanda_maxima$max_demanda - min_demanda) / (max_demanda - min_demanda)
    
    # Criar mapa
    leaflet(demanda_maxima) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon, ~lat,
        radius = ~sqrt(raio)/10,  # Raio proporcional à raiz quadrada da demanda para melhor visualização
        color = "blue",
        fillColor = "blue",
        fillOpacity = 0.6,
        popup = ~paste0(
          "<b>", cidade, "</b><br>",
          "Demanda máxima: ", max_demanda, " bicicletas/hora<br>",
          "Data e hora: ", hora_max
        )
      )
  })
  
  # Renderizar gráfico de comparação
  output$grafico_comparacao <- renderPlotly({
    previsoes <- todas_previsoes()
    
    if (length(previsoes) == 0) {
      return(NULL)
    }
    
    # Criar dataframe com demanda máxima por dia para cada cidade
    demanda_diaria <- data.frame(
      cidade = character(),
      data = as.Date(character()),
      max_demanda = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (nome_cidade in names(previsoes)) {
      cidade_dados <- previsoes[[nome_cidade]]
      
      # Agrupar por dia
      cidade_dados$data <- as.Date(cidade_dados$data_hora)
      
      # Encontrar demanda máxima por dia
      dias_dados <- cidade_dados %>%
        group_by(data) %>%
        summarise(max_demanda = max(demanda_prevista, na.rm = TRUE))
      
      demanda_diaria <- rbind(
        demanda_diaria,
        data.frame(
          cidade = nome_cidade,
          data = dias_dados$data,
          max_demanda = dias_dados$max_demanda,
          stringsAsFactors = FALSE
        )
      )
    }
    
    # Criar gráfico
    p <- ggplot(demanda_diaria, aes(x = data, y = max_demanda, color = cidade, group = cidade)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(
        title = "Demanda Máxima Diária por Cidade",
        x = "Data",
        y = "Demanda Máxima (bicicletas/hora)",
        color = "Cidade"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Renderizar gráfico detalhado para a cidade selecionada
  output$grafico_detalhado <- renderPlotly({
    previsoes <- todas_previsoes()
    cidade_selecionada <- input$cidade
    
    if (length(previsoes) == 0 || is.null(previsoes[[cidade_selecionada]])) {
      return(NULL)
    }
    
    cidade_dados <- previsoes[[cidade_selecionada]]
    
    # Criar gráfico
    p <- ggplot(cidade_dados, aes(x = data_hora, y = demanda_prevista)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "blue", size = 2) +
      labs(
        title = paste("Previsão Detalhada para", cidade_selecionada),
        x = "Data e Hora",
        y = "Demanda Prevista (bicicletas/hora)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Renderizar tabela de dados
  output$tabela_dados <- DT::renderDataTable({
    previsoes <- todas_previsoes()
    cidade_selecionada <- input$cidade
    
    if (length(previsoes) == 0 || is.null(previsoes[[cidade_selecionada]])) {
      return(NULL)
    }
    
    cidade_dados <- previsoes[[cidade_selecionada]]
    
    # Selecionar e formatar colunas para exibição
    dados_tabela <- cidade_dados %>%
      select(
        data_hora,
        temperature_c,
        humidity,
        wind_speed_m_s,
        rainfall_mm,
        snowfall_cm,
        demanda_prevista
      ) %>%
      mutate(
        data_hora = format(data_hora, "%d/%m/%Y %H:%M"),
        demanda_prevista = round(demanda_prevista, 0)
      )
    
    # Renomear colunas para exibição
    colnames(dados_tabela) <- c(
      "Data e Hora",
      "Temperatura (°C)",
      "Humidade (%)",
      "Velocidade do Vento (m/s)",
      "Chuva (mm)",
      "Neve (cm)",
      "Demanda Prevista (bicicletas/hora)"
    )
    
    dados_tabela
  })
}

# Iniciar a aplicação
shinyApp(ui, server)