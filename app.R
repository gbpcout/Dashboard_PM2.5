library(shiny)
library(shinydashboard)
library(bslib)
library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(lubridate)
library(BlandAltmanLeh)
library(irr)

# Carregamento e preparação de dados



# ===== Carregamento dos dados =====

rs = read_csv(file = "rs_pm25.csv")

rsmes <- rs %>%
  group_by(Cod, mes) %>%
  summarise(pm2.5 = mean(PM2.5, na.rm = TRUE), .groups = "drop")

rsano <- rsmes %>%
  group_by(Cod) %>%
  summarise(pm2.5 = mean(pm2.5, na.rm = TRUE), .groups = "drop")

rsUF <- rsmes %>%
  group_by(mes) %>%
  summarise(pm2.5 = mean(pm2.5, na.rm = TRUE), .groups = "drop")

rs.donkelar <- read_excel("dados_completos_consolidado_donkelar.xlsx") %>%
  filter(SIGLA_UF == 43)

rs.donk.reduzido <- rs.donkelar %>%
  select(Mes, Media_PM25) %>%
  dplyr::rename(., mes = Mes, `pm2.5` = Media_PM25)%>%
  mutate(fonte = "Von Donkelar")

rsano$fonte <- "CAMS"
rs.donk.ano <- rs.donkelar %>%
  select(CD_MUN, Mes, Media_PM25) %>%
  mutate(fonte = "Von Donkelar")

rsmes$fonte <- "CAMS"

rs.agregado <- bind_rows(rsmes, rs.donk.reduzido)
rs.agregado.2 <- left_join(rsano, rs.donk.ano, by = c("Cod" = "CD_MUN"))











#=============== UI ==========================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Painel PM2.5 - Satélites"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Boxplot", tabName = "boxplot", icon = icon("box")),
      menuItem("Série Temporal", tabName = "serie", icon = icon("chart-line")),
      menuItem("Dispersão", tabName = "dispersao", icon = icon("braille")),
      menuItem("Bland-Altman", tabName = "bland", icon = icon("chart-scatter")),
      menuItem("Mapa CAMS", tabName = "mapcams", icon = icon("map")),
      menuItem("Mapa Donkelar", tabName = "mapdonk", icon = icon("map")),
      menuItem("Estatísticas", tabName = "estat", icon = icon("calculator")),
      
      # Controles
      checkboxGroupInput("fonte", "Fonte de dados:",
                         choices = c("CAMS", "Von Donkelar"),
                         selected = c("CAMS", "Von Donkelar")),
      selectInput("mes", "Selecione o mês", choices = c("Todos", 1:12), selected = "Todos"),
      checkboxInput("agrupar_meses", "Agrupar por Satélite", value = FALSE)
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "boxplot", plotOutput("boxplot")),
      
      tabItem(tabName = "serie",
              plotOutput("serie"),
              br(),
              tags$p(em(" - A faixa de confiança é da média da diferença entre os métodos (CAMS e Von Donkelar)."),
                     style = "color: black; font-size: 20px;")
      ),
      
      tabItem(tabName = "dispersao", plotOutput("dispersao")),
      
      tabItem(tabName = "bland", plotOutput("bland")),
      
      tabItem(tabName = "mapcams", tmapOutput("mapcams", height = "80vh")),
      
      tabItem(tabName = "mapdonk", tmapOutput("mapdonk", height = "80vh")),
      
      tabItem(tabName = "estat", verbatimTextOutput("estat"))
    )
  )
)












# ========================================= Server ============================
server <- function(input, output) {
  # Filtrando dados por mês e fonte
  dados_filtrados <- reactive({
    if (input$mes == "Todos") {
      rs.agregado
    } else {
      rs.agregado %>% filter(mes == as.numeric(input$mes))
    }
  })
  
  
  output$boxplot <- renderPlot({
    dados <- dados_filtrados() %>%
      filter(fonte %in% input$fonte)
    
    if (input$agrupar_meses) {
      ggplot(dados, aes(x = fonte, y = pm2.5, fill = fonte)) +
        geom_boxplot() +
        theme_minimal() +
        labs(
          title = "Boxplot de PM2.5 (Todos os Meses Agrupados)",
          x = "Fonte",
          y = "PM2.5"
        ) +
        scale_fill_manual(values = c("CAMS" = "#1f77b4", "Von Donkelar" = "#ff7f0e"))
    } else {
      ggplot(dados, aes(x = factor(mes), y = pm2.5, fill = fonte)) +
        geom_boxplot(position = position_dodge(0.8)) +
        theme_minimal() +
        labs(
          title = "Boxplot de PM2.5 por Mês",
          x = "Mês",
          y = "PM2.5"
        ) +
        scale_x_discrete(labels = month.abb) +
        scale_fill_manual(values = c("CAMS" = "#1f77b4", "Von Donkelar" = "#ff7f0e"))
    }
  })
  
  
  
  output$serie <- renderPlot({
    
    # Agrega os dados por fonte e mês
    rs.plot <- rs.agregado %>%
      group_by(fonte, mes) %>%
      summarise(media = mean(pm2.5, na.rm = TRUE), .groups = "drop")
    
    # Linha de média geral
    media.geral <- rs.plot %>%
      group_by(mes) %>%
      summarise(media = mean(media), .groups = "drop") %>%
      mutate(fonte = "Média Geral")
    
    # Junta tudo
    rs.plot.total <- bind_rows(rs.plot, media.geral)
    
    ggplot(rs.plot.total, aes(x = mes, y = media, color = fonte, group = fonte)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      geom_smooth(
        data = rs.plot %>% filter(fonte != "Média Geral"),
        method = "loess", se = FALSE, linetype = "dashed", size = 0.8
      ) +
      geom_smooth(
        data = media.geral,
        method = "loess", se = TRUE,
        size = 1.2, alpha = 0.2, linetype = 1, color = "darkgreen"
      ) +
      scale_color_manual(values = c(
        "CAMS" = "darkblue",
        "Von Donkelar" = "orange",
        "Média Geral" = "darkgreen"
      )) +
      scale_x_continuous(
        breaks = 1:12,
        labels = month.abb
      ) +
      labs(
        title = "Série Temporal de PM2.5 por Fonte",
        x = "Mês",
        y = "PM2.5 médio (µg/m³)",
        color = "Fonte"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  
  output$dispersao <- renderPlot({
    ggplot(rs.agregado.2, aes(x = pm2.5, y = Media_PM25)) +
      geom_point(alpha = 0.7, color = "#1f77b4") +
      geom_smooth(method = "lm", se = FALSE, color = "darkred") +
      labs(x = "CAMS", y = "Von Donkelar", title = "Dispersão entre Satélites") +
      theme_minimal()
  })
  
  output$bland <- renderPlot({
    bland.altman.plot(rs.agregado.2$pm2.5, rs.agregado.2$Media_PM25,
                      main = "Bland-Altman", xlab = "Média", ylab = "Diferença")
  })
  
  output$estat <- renderPrint({
    rs.agregado.2.mensal <- rs.agregado.2 %>%
      group_by(Cod) %>%
      summarise(
        mensal.cams = mean(pm2.5),
        mensal.VonDonk = mean(Media_PM25)
      )
    
    list(
      Normalidade = jarque.bera.test(rsmes$pm2.5),
      Teste_pareado = t.test(rs.agregado.2.mensal$mensal.cams, rs.agregado.2.mensal$mensal.VonDonk, paired = TRUE),
      ICC = icc(data.frame(rs.agregado.2.mensal$mensal.cams, rs.agregado.2.mensal$mensal.VonDonk),
                model = "twoway", type = "agreement")
    )
  })
  
  
  output$mapcams <- renderTmap({
    tmap_mode("view")
    
    # Carrega o shapefile e junta com os dados
    map <- read_sf("RS_Municipios_2024.shp")
    rsano$Cod <- as.character(rsano$Cod)
    map <- left_join(map, rsano, by = c("CD_MUN" = "Cod"))
    
    # Calcular 4 intervalos (=> 5 pontos de corte)
    breaks_valores <- quantile(map$pm2.5, probs = seq(0, 1, length.out = 5), na.rm = TRUE)
    breaks_valores <- unique(breaks_valores)  # Tira duplicados (se houver)
    
    # Se por acaso tiver menos de 5 valores únicos, forçar um pequeno ajuste para manter 4 classes
    if(length(breaks_valores) < 5) {
      breaks_valores <- pretty(map$pm2.5, n = 4)
    }
    
    tm_shape(map) +
      tm_polygons(
        col = "pm2.5",
        palette = "Blues",
        style = "fixed",
        breaks = breaks_valores,
        id = "NM_MUN",
        title = "PM2.5 Médio"
      ) +
      tm_borders(lwd = 0.65)
  })
  
  
  
  
  
  
  output$mapdonk <- renderTmap({
    tmap_mode("view")
    map <- read_sf("RS_Municipios_2024.shp")
    
    # Corrigir tipo de dado
    rs.donkelar$CD_MUN <- as.character(rs.donkelar$CD_MUN)
    
    # Agregação
    donk <- rs.donkelar %>%
      group_by(CD_MUN) %>%
      summarise(Media_PM25 = mean(Media_PM25, na.rm = TRUE))
    
    map <- left_join(map, donk, by = "CD_MUN")
    
    # Calcular 4 intervalos (=> 5 pontos de corte)
    breaks_valores <- quantile(map$Media_PM25, probs = seq(0, 1, length.out = 5), na.rm = TRUE)
    breaks_valores <- unique(breaks_valores)  # Tira duplicados (se houver)
    
    # Se por acaso tiver menos de 5 valores únicos, forçar um pequeno ajuste para manter 4 classes
    if(length(breaks_valores) < 5) {
      breaks_valores <- pretty(map$Media_PM25, n = 4)
    }
    
    
    tm_shape(map) +
      tm_polygons(
        col = "Media_PM25",
        palette = "Oranges",
        style = "fixed",
        breaks = breaks_valores,
        id = "NM_MUN",
        title = "PM2.5 Médio"
      ) +
      tm_borders(lwd = 0.65)
  })
  
}

shinyApp(ui, server)
