# Cargamos las librerías necesarias
library(shiny)
library(highcharter)
library(readxl)
library(here)
library(readr)
# Importar base

url <- "https://raw.githubusercontent.com/laboratoriolide/datos-bce/main/Data/Puntos%20riesgo%20pa%C3%ADs.csv"

download.file(url, here("Data/Puntos riesgo país.csv"))

# Definimos la UI para la aplicación
ui <- fluidPage(
  titlePanel("Riesgo país Ecuador 2004-2023"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('dateRange',
                     label = 'Filtrado dinámico por fechas',
                     start = as.Date("2004-07-01"), 
                     end = as.Date("2023-06-01")
      ),
      helpText("Fuente: Banco Central del Ecuador. Los puntos base del riesgo país representan la diferencia 
               entre la tasa de rendimiento de la deuda de un país y la tasa libre de riesgo. Un mayor número de 
               puntos base indica un mayor riesgo percibido asociado con la inversión en ese país.")
    ),
    
    mainPanel(
      highchartOutput("countryRiskChart")
    )
  )
)

# Definimos el servidor para la aplicación
server <- function(input, output) {
  # Leemos los datos
  data <- reactive({
    df <- read.delim("Data/Puntos riesgo país.csv", sep = ";", header = TRUE)
    df$Periodo <- as.Date(df$Periodo, format = "%d/%m/%Y")
    df
  })
  
  # Creamos el gráfico interactivo
  output$countryRiskChart <- renderHighchart({
    df <- data()
    df <- df[df$Periodo >= input$dateRange[1] & df$Periodo <= input$dateRange[2],]
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(text = "", style = list(fontSize = "24px", fontWeight = "bold")) %>%
      hc_subtitle(text = "", style = list(fontSize = "12px")) %>%
      hc_xAxis(categories = df$Periodo, labels = list(rotation = -45)) %>%
      hc_yAxis(title = list(text = ""), labels = list(style = list(fontSize = "8px"))) %>%
      hc_plotOptions(
        series = list(
          color = 'black',
          marker = list(
            enabled = TRUE,
            fillColor = 'black',
            radius = 2
          )
        )
      ) %>%
      hc_add_series(name = "Riesgo país", data = df$`Puntos_base`)
  })
}

# Ejecutamos la aplicación 
shinyApp(ui = ui, server = server)


