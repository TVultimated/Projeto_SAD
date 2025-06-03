# Previsão de Procura em Sistemas de Bicicletas Partilhadas

Este projeto foi desenvolvido no âmbito da unidade curricular **Sistemas de Apoio à Decisão (SAD)**. O objetivo principal é prever a procura por bicicletas partilhadas com base em dados meteorológicos e temporais, utilizando como caso de estudo a cidade de Seul e comparando com outras grandes cidades mundiais.

## Estrutura do Projeto
```
📁 data/
│   ├── raw_bike_sharing_systems.csv
│   ├── raw_cities_weather_forecast.csv
│   ├── raw_seoul_bike_sharing.csv
│   ├── raw_worldcities.csv
│   ├── clean_bike_sharing_systems.csv
│   ├── clean_cities_weather_forecast.csv
│   ├── clean_seoul_bike_sharing.csv
│   ├── clean_worldcities.csv
│   └── projeto_sad (base de dados SQLite)

📁 images/
│   ├── Rplot01.png ... Rplot05.png  # Gráficos da análise exploratória

📄 01_recolha_dados.R            # Web scraping e chamadas API
📄 02_limpeza_transformacao.R    # Tratamento e limpeza dos dados
📄 03_analise_exploratoria.R     # Consultas SQL e análise descritiva
📄 04_visualizacoes.R            # Gráficos com ggplot2
📄 05_modelacao_regressao.R      # Modelos de regressão e avaliação
📄 app.R                         # Aplicação Shiny
📄 modelo_interacao.RData        # Modelo final com interações
📄 Projeto_SAD.Rproj             # Projeto RStudio
📄 .Rhistory / .RData            # Ficheiros de sessão R
📄 UAL - Projeto SAD 2024-2025.pdf  # Enunciado do projeto
```

## Tecnologias Utilizadas

- **Linguagem:** R
- **Ambiente:** RStudio
- **Bibliotecas:** `dplyr`, `ggplot2`, `plotly`, `caret`, `glmnet`, `rvest`, `httr`, `jsonlite`, `lubridate`, `shiny`, `shinydashboard`, `leaflet`, `DT`, `DBI`, `RSQLite`  
- **Outros:** SQLite, OpenWeather API, Wikipédia

## Passos Executados

### 1. Recolha de Dados
- Web scraping da Wikipédia com `rvest`
- API da OpenWeather com `httr` e `jsonlite`
- Dados históricos de Seul (UCI Repository)
- Ficheiro de cidades (SimpleMaps)

### 2. Limpeza e Transformação
- Padronização de nomes e formatos
- Criação de variáveis dummies
- Normalização de variáveis numéricas
- Extração de hora e estação

### 3. Análise Exploratória
- Executada com `dplyr` e `DBI` sobre base de dados SQLite
- Contagem de registos, análise de sazonalidade e popularidade horária
- Verificação de dados meteorológicos e cruzamento com alugueres
- Cálculo de máximos históricos e médias por estação

### 4. Visualizações
- Gráficos produzidos com `ggplot2`:
  - Dispersão do número de alugueres ao longo do tempo
  - Dispersão por hora com variação de cor
  - Histograma com curva de densidade
  - Relação entre temperatura e alugueres por estação
  - Boxplots por hora e estação

### 5. Modelação Preditiva
- Modelos lineares simples: meteorológico e temporal
- Modelos melhorados:
  - Com termos polinomiais
  - Com interações (ex: temperatura * hora)
  - Com regularização LASSO (`glmnet`)
- Avaliação com RMSE sobre dados de teste
- Modelo final (com interações) guardado em `modelo_interacao.RData`

### 6. Aplicação Shiny
- Interface composta por três separadores:
  - **Mapa Interativo**: visualiza a procura máxima prevista por cidade com `leaflet`
  - **Comparação de Cidades**: gráfico temporal interativo com `plotly`
  - **Dados Detalhados**: gráfico e tabela interativa com previsões por hora
- Dados meteorológicos obtidos em tempo real via API OpenWeather
- Previsão gerada com `predict()` sobre o modelo treinado
- Atualização manual ou automática ao iniciar a app

## Como Executar

1. Abrir o projeto com `Projeto_SAD.Rproj` no RStudio.
2. Executar os scripts `01_recolha_dados.R` até `05_modelacao_regressao.R` por ordem.
3. Iniciar a aplicação com `app.R` (usar botão “Run App” no RStudio).
4. Verifica se tens os seguintes pacotes instalados:
   ```r
   install.packages(c("dplyr", "ggplot2", "plotly", "caret", "glmnet", "rvest", "httr", 
                      "jsonlite", "lubridate", "shiny", "shinydashboard", "leaflet", 
                      "DT", "DBI", "RSQLite"))

## Autores
- Diogo Costa – 30011282
- Guilherme Fernandes – 30010398
- Tomás Viana – 30010623
